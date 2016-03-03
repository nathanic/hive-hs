module Hive.Server.Game where

-- game state management stuff

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Reader             (ask, asks, ReaderT, runReaderT, lift)
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

import Servant (ServantErr(..), err400, err404)

applyMove :: AbsoluteMove -> GameId -> AppM ()
applyMove move gid = do
    result <- liftIO . atomically $ do
        mGI <- getGameInfo gid
        case mGI of
            Nothing -> return $ Left err404 { errBody = "No game found for id " <> pack (show gid) }
            Just gi@GameInfo{giGame=game} ->
                case Engine.applyMove move game of
                    Left err    -> return $ Left
                        err404 { errBody = "Move " <> pack (show move)
                                            <> " is not valid for game with id "
                                            <> pack (show gid) <> ":\n"
                                            <> pack err
                               }
                    Right game' -> Right <$> updateGameInfo gid gi { giGame = game' }
    either (lift . left) return result


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

updateGameInfo :: GameId -> GameInfo -> STM ()
updateGameInfo gid ginfo =
    modifyTVar' gameDB $ Map.update (const (Just ginfo)) gid

nextId :: STM GameId
nextId = fromIntegral . (1 +) . Map.size <$> readTVar gameDB



