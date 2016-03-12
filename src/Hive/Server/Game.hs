-- | The Game Server. Servant API & state management
module Hive.Server.Game where

-- imports {{{
import Control.Concurrent               (forkIO)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad                    (forever)
import Control.Monad.IO.Class
import Control.Monad.Reader             (ask, asks, ReaderT, runReaderT, lift)
import Control.Monad.Trans              (MonadTrans)
import Control.Monad.Trans.Either

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Int
import Data.Map                         (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe                       (fromJust, fromMaybe)
import Data.Monoid                      ((<>))
import Data.Text

import Debug.Trace

import           Network.HTTP.Types     (status400)
import qualified Network.WebSockets             as WS
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.WebSockets as WaiSock


import Hive.Game.Move                   (AbsoluteMove)
import Hive.Game.Engine                 (Game(..))
import qualified Hive.Game.Engine as Engine
import Hive.Game.Piece                  (Team(..))
import Hive.Server.Types

import Servant

import Text.Printf
-- }}}

--------------------------------------------------------------------------------
-- Servant API {{{
--------------------------------------------------------------------------------
type GameAPI = "game" :>
                (Header "Authorization" FakeAuth :> Post '[JSON] NewGameResult
                :<|> Capture "gameid" GameId :>
                    (
                        Header "Authorization" FakeAuth :> "join" :> Post '[JSON] Game
                        :<|> "force" :> Post '[PlainText] Text
                        :<|> Header "Authorization" FakeAuth :> ReqBody '[JSON] AbsoluteMove :> Post '[JSON] Game
                        :<|> Get '[JSON] Game
                    ))

api :: Proxy GameAPI
api = Proxy

gameServer :: ServerT GameAPI AppM
gameServer = makeGame :<|> usingGameId
  where
    usingGameId gid =
                     joinGame gid
                :<|> forceUpdate gid
                :<|> applyMove gid
                :<|> getGame gid

makeGame :: Maybe FakeAuth -> AppM NewGameResult
makeGame mAuth = do
    liftIO $ putStrLn "in makeGame"
    player <- requestPlayer mAuth
    gid <- apptomically $ createGameInfo (Just player) Nothing
    liftIO $ putStrLn "made the game"
    return NewGameResult { gameId = gid }

getGame :: GameId -> AppM Game
getGame gid = giGame <$> apptomically (getGameInfo' gid)

requestPlayer :: Maybe FakeAuth -> AppM Player
requestPlayer (Just (FakeAuth name)) = return $ Player name 1
requestPlayer Nothing                = lift $ left errNoAuth

notifyGameState :: GameId -> AppM ()
notifyGameState gid = do
    db <- asks gameDB
    liftIO $ atomically $ do
        mGI <- getGameInfo db gid
        case mGI of
            Nothing           ->
                traceM $ printf "can't notify about game %d because it doesn't exist :-(\n" gid
            Just GameInfo{..} ->
                writeTChan giAnnounceChan giGame

-- | debug helper to force sending a game state to all connected websockets
forceUpdate :: GameId -> AppM Text
forceUpdate gid = do
    notifyGameState gid
    return "upwardly dated"

joinGame :: GameId -> Maybe FakeAuth -> AppM Game
joinGame gid mAuth = do
    player <- requestPlayer mAuth
    gi <- modifyGameInfo gid $ \gi@GameInfo{..} ->
        case (giWhite, giBlack) of
            (Nothing, _) -> return gi { giWhite = Just player }
            (_, Nothing) -> return gi { giBlack = Just player }
            _            -> lift $ left errGameIsFull
    notifyGameState gid
    return $ giGame gi

teamForPlayerInGame :: Player -> GameInfo -> Maybe Team
teamForPlayerInGame p GameInfo{ giWhite=pw, giBlack=pb }
    | Just p == pw = Just White
    | Just p == pb = Just Black
    | otherwise    = Nothing

applyMove :: GameId -> Maybe FakeAuth -> AbsoluteMove -> AppM Game
applyMove gid mAuth move = do
    player <- requestPlayer mAuth
    gi <- modifyGameInfo gid $ \gi@GameInfo{..} -> do
        case teamForPlayerInGame player gi of
            Nothing                       -> lift $ left errNotInGame
            Just t | t /= gameTurn giGame -> lift $ left errNotYourTurn
                   | otherwise            -> return ()
        game' <- lift $ hoistEither $ errify $ Engine.applyMove move giGame
        return gi { giGame = game' }
    notifyGameState gid
    return $ giGame gi
  where
    errify (Left err) = Left err400 { errBody = LBS.pack err }
    errify (Right x)  = Right x


errNoAuth = err401 { errBody = "Authorization required for this resource." }
errGameIsFull = err400 { errBody = "Game is full." }
errNotYourTurn = err400 { errBody = "It's not your turn, buddy!" }
errNotInGame = err400 { errBody = "You're not in this game! \
                                  \Do you even go to this school?"
                      } -- 401?

-- }}}

--------------------------------------------------------------------------------
-- Websocket stuff {{{
--------------------------------------------------------------------------------
instance WS.WebSocketsData Game where
    toLazyByteString = JSON.encode
    fromLazyByteString = error "nobody should be sending us game data"
                        -- fromJust . JSON.decode

instance WS.WebSocketsData GameId where
    toLazyByteString = LBS.pack . show
    fromLazyByteString = read . LBS.unpack

-- pity we can't use a Raw endpoint to embed this in a route
-- seems like Raw and Enter are incompatible
announceServerOr :: Config -> Wai.Application -> Wai.Application
announceServerOr cfg = WaiSock.websocketsOr WS.defaultConnectionOptions app
  where
    -- TODO: wss:// support?
    app pending = do
        conn <- WS.acceptRequest pending
        WS.forkPingThread conn 30
        -- it'd be prettier to get the gid from the url, which is why i wish i could use Raw
        -- i suppose i could get the Wai request and parse it my own damn self
        -- orrrr i could make another servant app that doesn't use AppM
        -- to capture the ID and such, also auth
        -- and combine it somehow with the main one...
        gid <- WS.receiveData conn
        mGI <- atomically $ getGameInfo (gameDB cfg) gid
        case mGI of
            Nothing ->
                WS.sendClose conn ("No such game ID." :: Text)
            Just gi -> do
                WS.sendTextData conn (giGame gi)
                flip finally (printf "*** got an exception!\n") $ do
                    chan <- atomically $ dupTChan (giAnnounceChan gi)
                    forever $
                        atomically (readTChan chan) >>= WS.sendTextData conn
                return ()

-- }}}
--------------------------------------------------------------------------------
-- Fake "Storage" "Backend" that justifies all these "scare quotes" {{{
--------------------------------------------------------------------------------

-- XXX instead of putting this into a hacky global, need to stash this in AppM

createGameInfo :: Maybe Player -> Maybe Player -> AppSTM GameId
createGameInfo wp bp = do
    chan <- liftSTM newTChan
    gid <- nextId
    db <- asks gameDB
    liftSTM $ modifyTVar' db $
         Map.insert gid GameInfo { giWhite = wp
                                 , giBlack = bp
                                 , giGame = Engine.newGame
                                 , giAnnounceChan = chan
                                 }
    return gid

-- | plain STM version
getGameInfo :: GameDB -> GameId -> STM (Maybe GameInfo)
getGameInfo gameDB gid = Map.lookup gid <$> readTVar gameDB

-- | AppSTM version
getGameInfo' :: GameId -> AppSTM GameInfo
getGameInfo' gid = do
    db <- asks gameDB
    mGI <- liftSTM (getGameInfo db gid)
    lift $ hoistEither $ maybe err Right mGI
  where err = Left err404 { errBody = "No game found for id "
                                        <> LBS.pack (show gid)
                          }

updateGameInfo :: GameId -> GameInfo -> AppSTM GameInfo
updateGameInfo gid ginfo = do
    db <- asks gameDB
    liftSTM $ modifyTVar' db $ Map.update (const (Just ginfo)) gid
    return ginfo


modifyGameInfo :: GameId -> (GameInfo -> AppSTM GameInfo) -> AppM GameInfo
modifyGameInfo gid mutator =
    apptomically $ do
        gi' <- mutator =<< getGameInfo' gid
        updateGameInfo gid gi'
        pure gi'

-- | super cheesy nonsense alert
nextId :: AppSTM GameId
nextId = fromIntegral . (1 +) . Map.size <$> readDB

readDB :: AppSTM (Map GameId GameInfo)
readDB = (liftSTM . readTVar) =<< asks gameDB

--}}
