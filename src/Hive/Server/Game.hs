module Hive.Server.Game where

-- game state management stuff

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader             (ask, asks, ReaderT, runReaderT, lift)
import Control.Monad.Trans              (MonadTrans)
import Control.Monad.Trans.Either

import Data.ByteString.Lazy.Char8       (pack)
import Data.Int
import Data.Map                         (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid                      ((<>))

import System.IO.Unsafe                 (unsafePerformIO)

import Hive.Game.Move (AbsoluteMove)
import Hive.Game.Engine (Game(..))
import qualified Hive.Game.Engine as Engine
import Hive.Server.Types

import Servant

type GameAPI = "game" :> Capture "gameid" GameId :>
                ( Get '[JSON] Game
                  :<|> "join" :> Post '[JSON] Game
                  :<|> ReqBody '[JSON] AbsoluteMove :> Post '[JSON] Game
                  -- :<|> "announce" :> Raw
                )

api :: Proxy GameAPI
api = Proxy

gameServer :: ServerT GameAPI AppM
gameServer gid = getGame gid
            :<|> joinGame gid
            :<|> applyMove gid
            -- :<|> announceSocket gid

getGame :: GameId -> AppM Game
-- getGame gid = lift . either left (right . giGame) =<< latomically (getGameInfo' gid)
-- getGame gid = lift . hoistEither =<< latomically (giGame <$> getGameInfo' gid)
getGame gid = do
    gi <- latomically $ getGameInfo' gid
    giGame <$> lhoistEither gi
-- getGame gid = giGame <$> webomically err404 (getGameInfo' gid)

-- stub
requestPlayer :: AppM Player
requestPlayer = return $ Player "nobody" 1

-- XXX what about observers? we probably want to support that
joinGame :: GameId -> AppM Game
joinGame gid = do
    player <- requestPlayer -- TODO: make this a thing
    gi <- modifyGameInfo gid $ \gi@GameInfo{..} ->
        case (giWhite, giBlack) of
            (Nothing, _) -> return gi { giWhite = Just player }
            (_, Nothing) -> return gi { giBlack = Just player }
            _            -> left err400 { errBody = "Game is full." }
    return $ giGame gi
    -- TODO: notify other connected clients


applyMove :: GameId -> AbsoluteMove -> AppM Game
applyMove gid move =
    fmap giGame $
        modifyGameInfo gid $ \gi -> do
            game' <- hoistEither . errify $ Engine.applyMove move (giGame gi)
            return gi { giGame = game' }
    -- TODO: notify other connected clients
  where
    -- not sure i like this error mapping business
    errify :: Either String a -> Either ServantErr a
    errify (Left err) = Left err404 { errBody = "Move " <> pack (show move)
                                                <> " is not valid for game with id "
                                                <> pack (show gid) <> ":\n"
                                                <> pack err
                                    }
    errify (Right x) = Right x
    -- maybe pull out a general one for Either String :~> Either ServantError
    -- webify :: STM (Either String a) -> IO (Either ServantErr a)
    -- webify tx = do
    --     result <- atomically tx
    --     return $ case result of
    --         Left err -> Left err404 { errBody = "Move " <> pack (show move)
    --                                             <> " is not valid for game with id "
    --                                             <> pack (show gid) <> ":\n"
    --                                             <> pack err
    --                                 }
    --         -- feels weird but we have to re-Right this to change the left-type
    --         Right x        -> Right x



-- applyMove :: GameId -> AbsoluteMove -> AppM Game
-- applyMove gid move = do
--     result <- latomically $ do
--         mGI <- getGameInfo gid
--         case mGI of
--             Nothing -> return $ Left err404 { errBody = "No game found for id " <> pack (show gid) }
--             Just gi@GameInfo{giGame=game} ->
--                 case Engine.applyMove move game of
--                     Left err    -> return $ Left
--                         err404 { errBody = "Move " <> pack (show move)
--                                             <> " is not valid for game with id "
--                                             <> pack (show gid) <> ":\n"
--                                             <> pack err
--                                }
--                     Right game' -> Right <$> updateGameInfo gid gi { giGame = game' }
--     either (lift . left) pure result


--------------------------------------------------------------------------------
-- Fake/Temporary "Storage" "Backend" that justifies all these "scare quotes"
--------------------------------------------------------------------------------
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
  where err = Left err404 { errBody = "No game found for id " <> pack (show gid) }

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


