{-# LANGUAGE TemplateHaskell #-}
module Hive.Server.Types where

import Control.Concurrent.STM
import Control.Monad.Reader         (ask, asks, ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either

import Data.Aeson
import Data.Aeson.TH
import Data.Int

import Servant (ServantErr)

import Hive.Game.Engine (Game(..))

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


type AppM = ReaderT Config (EitherT ServantErr IO)

data Config = Config { userDB :: TVar [User]
-- okay, to start out there will be no backing store
-- we'll just have a list of Games in memory
-- we'll also need to know extra things about a game,
-- such as the websockets of connected clients so we can notify on state changes
                     }


data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)


