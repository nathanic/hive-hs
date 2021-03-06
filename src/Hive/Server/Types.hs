{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Hive.Server.Types where

import Control.Concurrent.STM
import Control.Monad.Reader         (ask, asks, ReaderT, runReaderT, lift)
import Control.Monad.Trans
import Control.Monad.Trans.Except

import Data.Aeson
import Data.Aeson.TH
import Data.Hashable                (Hashable)
import qualified Data.HashMap.Strict as H
import Data.Int
import Data.Map                     (Map)
import qualified Data.Map as Map
import Data.Maybe                   (fromJust)
import Data.Text (Text(..))
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T

import Servant

-- TODO: make some kind of roll-up package Hive.Game that re-exports the public stuff
import Hive.Game.Board              (Board(..))
import Hive.Game.Move               (AbsoluteMove(..))
import Hive.Game.HexGrid            (AxialPoint(..))
import Hive.Game.Piece              (Piece(..), Team(..), piece)
import Hive.Game.Engine             (Game(..),GameState(..))

-- should i have IDs in the models or not?
-- Persistent keeps them out...
type PlayerId = Int64
data Player = Player { playerName :: Text
                     , playerId :: Int64
                     } deriving (Eq, Show)

type GameId = Int64
data GameInfo = GameInfo { giWhite          :: Maybe Player
                         , giBlack          :: Maybe Player
                         , giGame           :: Game
                         , giAnnounceChan   :: TChan Game
                         }


type AppM = ReaderT Config (ExceptT ServantErr IO)
type AppSTM = ReaderT Config (ExceptT ServantErr STM)

runApp :: Config -> AppM a -> IO (Either ServantErr a)
runApp cfg app = runExceptT (runReaderT app cfg)

liftSTM :: STM a -> AppSTM a
liftSTM = lift . lift

-- | like `atomically` but for our custom monad stacks
apptomically :: AppSTM a -> AppM a
apptomically stmAct = do
    context <- ask
    lift $ ExceptT $ liftIO $ atomically $
        runExceptT $ runReaderT stmAct context

type GameDB = TVar (Map GameId GameInfo)

data Config = Config { userDB :: TVar [User]
                     , gameDB :: GameDB
                     }

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

$(deriveJSON defaultOptions ''AxialPoint)
$(deriveJSON defaultOptions ''Team)
$(deriveJSON defaultOptions ''Board)
$(deriveJSON defaultOptions ''AbsoluteMove)
$(deriveJSON defaultOptions ''GameState)
$(deriveJSON defaultOptions ''Game)

instance ToJSON Piece where
    toJSON = toJSON . pieceName

instance FromJSON Piece where
    parseJSON = (piece <$>) . parseJSON

instance ToJSON a => ToJSON (Map Piece a) where
    toJSON = Object . mapHashKeyVal (T.pack . show) toJSON

instance FromJSON a => FromJSON (Map Piece a) where
    -- TODO: validate that this is a legit piece
    parseJSON = fmap (hashMapKey piece) . parseJSON

instance ToJSON a => ToJSON (Map AxialPoint a) where
    toJSON = Object . mapHashKeyVal (T.pack . show) toJSON

instance FromJSON a => FromJSON (Map AxialPoint a) where
    parseJSON = fmap (hashMapKey shittyParse) . parseJSON
      where
        shittyParse s = let [p,q] = read s
                         in Axial p q


-- | Transform a 'Map' into a 'HashMap' while transforming the keys.
-- (ganked directly from Data.Aeson.Functions in service of yak-shavery)
mapHashKeyVal :: (Eq k2, Hashable k2) => (k1 -> k2) -> (v1 -> v2)
              -> Map k1 v1 -> H.HashMap k2 v2
mapHashKeyVal fk kv = Map.foldrWithKey (\k v -> H.insert (fk k) (kv v)) H.empty

-- | Transform a 'M.Map' into a 'H.HashMap' while transforming the keys.
-- (also ganked directly from Data.Aeson.Functions)
hashMapKey :: (Ord k2) => (k1 -> k2)
           -> H.HashMap k1 v -> Map k2 v
hashMapKey kv = H.foldrWithKey (Map.insert . kv) Map.empty
{-# INLINE hashMapKey #-}


-- eventually might stick some more useful stuff in here
data NewGameResult = NewGameResult { gameId :: GameId
                                   } deriving (Show)
$(deriveJSON defaultOptions ''NewGameResult)


newtype FakeAuth = FakeAuth Text -- ^ username
    deriving (Eq, Ord, Show)

instance FromHttpApiData FakeAuth where
    parseUrlPiece t =
        case T.words $ T.strip t of
            "Fake":r:_ -> Right (FakeAuth r)
            _          -> Left "We only cotton to the Fake realm round these parts"


hoistEither :: Monad m => Either e a -> ExceptT e m a
hoistEither = ExceptT . return
