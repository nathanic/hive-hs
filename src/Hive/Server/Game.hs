module Hive.Server.Game where

-- game state management stuff

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
import Data.Maybe                       (fromJust)
import Data.Monoid                      ((<>))
import Data.Text

import Debug.Trace

import           Network.HTTP.Types     (status400)
import qualified Network.WebSockets             as WS
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.WebSockets as WaiSock


import System.IO.Unsafe                 (unsafePerformIO)

import Hive.Game.Move                   (AbsoluteMove)
import Hive.Game.Engine                 (Game(..))
import qualified Hive.Game.Engine as Engine
import Hive.Server.Types

import Servant

import Text.Printf

type GameAPI = "game" :>
                (Post '[JSON] NewGameResult
                :<|> Capture "gameid" GameId :>
                    (
                        "join" :> Post '[JSON] Game
                        :<|> "force" :> Post '[PlainText] Text
                        :<|> ReqBody '[JSON] AbsoluteMove :> Post '[JSON] Game
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

makeGame :: AppM NewGameResult
makeGame = do
    liftIO $ putStrLn "in makeGame"
    player <- requestPlayer
    gid <- latomically $ createGameInfo (Just player) Nothing
    liftIO $ putStrLn "made the game"
    return NewGameResult { gameId = gid }

getGame :: GameId -> AppM Game
-- getGame gid = lift . either left (right . giGame) =<< latomically (getGameInfo' gid)
-- getGame gid = lift . hoistEither =<< latomically (giGame <$> getGameInfo' gid)
getGame gid = do
    liftIO $ putStrLn ("fetching game " <> show gid)
    gi <- latomically $ getGameInfo' gid
    giGame <$> lhoistEither gi
-- getGame gid = giGame <$> webomically err404 (getGameInfo' gid)

-- stub
requestPlayer :: AppM Player
requestPlayer = return $ Player "nobody" 1

-- traceM s = trace s (return ())

notifyGameState :: GameId -> AppM ()
notifyGameState gid =
    latomically $ do
        mGI <- getGameInfo gid
        case mGI of
            Nothing           -> traceM $ printf "can't notify about game %d because it doesn't exist :-(\n" gid 
            Just GameInfo{..} -> writeTChan giAnnounceChan giGame

-- | debug helper to force sending a game state to all connected websockets
forceUpdate :: GameId -> AppM Text
forceUpdate gid = do
    liftIO $ printf "sending out forced update on game %d\n" gid
    notifyGameState gid
    liftIO $ printf "sent forced update on game %d\n" gid
    return "upwardly dated"


-- XXX what about observers? we probably want to support that
-- I guess we can let anybody get a websocket into it...  we don't currently
-- reify the socket clients; we just dupe a TChan thread to shovel data out to
-- the [write-only] socket.  if we want to support private games we'll have to
-- do an auth check, but i suppose otherwise we don't really care about their
-- identity
joinGame :: GameId -> AppM Game
joinGame gid = do
    player <- requestPlayer -- TODO: make this a thing
    gi <- modifyGameInfo gid $ \gi@GameInfo{..} ->
        case (giWhite, giBlack) of
            (Nothing, _) -> return gi { giWhite = Just player }
            (_, Nothing) -> return gi { giBlack = Just player }
            _            -> left err400 { errBody = "Game is full." }
    notifyGameState gid
    return $ giGame gi


applyMove :: GameId -> AbsoluteMove -> AppM Game
applyMove gid move = do
    gi <- modifyGameInfo gid $ \gi -> do
        game' <- hoistEither . errify $ Engine.applyMove move (giGame gi)
        return gi { giGame = game' }
    notifyGameState gid
    return $ giGame gi
  where
    -- not sure i like this error mapping business
    errify :: Either String a -> Either ServantErr a
    errify (Left err) = Left err404 { errBody = "Move " <> LBS.pack (show move)
                                                <> " is not valid for game with id "
                                                <> LBS.pack (show gid) <> ":\n"
                                                <> LBS.pack err
                                    }
    errify (Right x) = Right x

instance WS.WebSocketsData Game where
    toLazyByteString = JSON.encode
    fromLazyByteString = error "nobody should be sending us websocket data"
                        -- fromJust . JSON.decode

instance WS.WebSocketsData GameId where
    toLazyByteString = LBS.pack . show
    fromLazyByteString = read . LBS.unpack

announceServerOr :: Config -> Wai.Application -> Wai.Application
announceServerOr cfg = WaiSock.websocketsOr WS.defaultConnectionOptions app
  where
    -- errorApp _ respond = respond $ Wai.responseLBS status400 [] "Sorry bub, this is a WebSocket endpoint."
    app pending = do
        putStrLn "got a request for a websocket"
        conn <- WS.acceptRequest pending
        WS.forkPingThread conn 30
        gid <- WS.receiveData conn
        printf "they sent GameId %d\n" gid
        mGI <- atomically $ getGameInfo gid
        printf "got the game info\n"
        case mGI of
            Nothing -> do
                printf "no such game :-(\n"
                WS.sendClose conn ("No such game ID." :: Text)
            Just gi -> do
                printf "got the game info, sending it down the pipe \n"
                WS.sendTextData conn (giGame gi)
                flip finally (printf "*** got an exception!\n") $ do
                    chan <- atomically $ dupTChan (giAnnounceChan gi)
                    forever $ do
                        printf $! "waiting to pick up some data...\n"
                        g <- atomically (readTChan chan)
                        printf $! "got the data, now sending it on the socket\n"
                        WS.sendTextData conn g
                        printf "shoveled it good!\n"
                return ()


--------------------------------------------------------------------------------
-- Fake/Temporary "Storage" "Backend" that justifies all these "scare quotes"
--------------------------------------------------------------------------------

-- XXX instead of putting this into a hacky global, need to stash this in AppM
{-# NOINLINE gameDB #-}
gameDB :: TVar (Map GameId GameInfo)
gameDB = unsafePerformIO $ newTVarIO mempty

createGameInfo :: Maybe Player -> Maybe Player -> STM GameId
createGameInfo wp bp = do
    chan <- newTChan
    gid <- nextId
    modifyTVar' gameDB $
         Map.insert gid GameInfo { giWhite = wp
                                 , giBlack = bp
                                 , giGame = Engine.newGame
                                 , giAnnounceChan = chan
                                 }
    return gid

getGameInfo :: GameId -> STM (Maybe GameInfo)
getGameInfo gid = Map.lookup gid <$> readTVar gameDB

-- more concern-mixey and ugly, but also DRYer & more convenient
getGameInfo' :: GameId -> STM (Either ServantErr GameInfo)
getGameInfo' gid = maybe err Right <$> getGameInfo gid
  where err = Left err404 { errBody = "No game found for id " <> LBS.pack (show gid) }

updateGameInfo :: GameId -> GameInfo -> STM GameInfo
updateGameInfo gid ginfo = do
    modifyTVar' gameDB $ Map.update (const (Just ginfo)) gid
    return ginfo

nextId :: STM GameId
nextId = fromIntegral . (1 +) . Map.size <$> readTVar gameDB


--------------------------------------------------------------------------------
-- Weird Little Helpers
--------------------------------------------------------------------------------
lhoistEither :: (Monad m, MonadTrans t) => Either e a -> t (EitherT e m) a
lhoistEither = lift . hoistEither

latomically :: MonadIO m => STM a -> m a
latomically = liftIO . atomically

-- | Little helper to run STM transactions between our two main EitherT-bearing monad stacks
eitomically :: EitherT ServantErr STM a -> AppM a
eitomically esAction = lhoistEither =<< latomically (runEitherT esAction)

-- | Run an EitherT ServantErr STM transaction against the given GameInfo
modifyGameInfo :: GameId -> (GameInfo -> EitherT ServantErr STM GameInfo) -> AppM GameInfo
modifyGameInfo gid action =
    eitomically $ do
        gi <- EitherT (getGameInfo' gid)
        gi' <- action gi
        lift $ updateGameInfo gid gi'
        pure gi'

-- webomically :: ServantErr -> STM (Either String a) -> AppM a
-- webomically serr action = lift $ mapEitherT translate
--   where
--     translate :: STM (Either String a) -> IO (Either ServantErr a)
--     translate serr tx = do
--         result <- atomically action
--         case result of
--             Left err -> Left serr { errBody = err }
--             x        -> x


