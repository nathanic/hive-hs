module Hive.Server.Game where

-- game state management stuff

import Control.Concurrent.STM
import Control.Monad.IO.Class

import Data.Int
import Data.Map (Map)
import qualified Data.Map.Strict as Map

import System.IO.Unsafe (unsafePerformIO)

import Hive.Game.Engine (Game(..))
import qualified Hive.Game.Engine as Engine


-- should i have IDs in the models or not?
-- Persistent keeps them out...
type PlayerId = Int64
data Player = Player { playerName :: String
                     , playerId :: Int64
                     } deriving (Eq, Show)

type GameId = Int64
data GameInfo = GameInfo { giWhite          :: Maybe Player
                         , giBlack          :: Maybe Player
                         , giGame           :: Game
                         , giAnnounceChan   :: TChan Game
                         }

{-# NOINLINE gameDB #-}
gameDB :: TVar (Map GameId GameInfo)
gameDB = unsafePerformIO $ newTVarIO mempty

createGameInfo :: (MonadIO m) => Maybe Player -> Maybe Player -> m GameId
createGameInfo wp bp = liftIO . atomically $ do
    chan <- newTChan
    (gid,_) <- Map.findMax <$> readTVar gameDB
    let gid' = gid + 1
    modifyTVar' gameDB $ \db ->
        let (gid,_) = Map.findMax db
         in Map.insert gid'
                       GameInfo { giWhite = wp
                                , giBlack = bp
                                , giGame = Engine.newGame
                                , giAnnounceChan = chan
                                }
                       db
    return gid'

getGameInfo :: (MonadIO m) => GameId -> m (Maybe GameInfo)
getGameInfo gid = Map.lookup gid <$> liftIO (readTVarIO gameDB)

updateGameInfo :: (MonadIO m) => GameId -> GameInfo -> m ()
updateGameInfo gid ginfo = liftIO . atomically $
    modifyTVar' gameDB $ Map.update (const (Just ginfo)) gid


