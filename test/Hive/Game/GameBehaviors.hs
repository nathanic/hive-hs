module Hive.Game.GameBehaviors
  where

import qualified Test.Tasty as Tasty
import Test.Tasty.Hspec

import Data.List (find, sort, maximumBy, (\\))
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set

import Hive.Game.Board
import Hive.Game.HexGrid (AxialPoint(..))
import qualified Hive.Game.HexGrid as Grid
import Hive.Game.Engine
import Hive.Game.Move
import Hive.Game.Piece

import Hive.Game.TestUtil

gameSpec :: Spec
gameSpec = parallel $
    describe "Game" $ do
        it "is over when a queen is surrounded" $ do
            -- make a game where a queen is almost surrounded
            -- assert game isn't over
            -- surround the queen
            -- assert game is over, and winner is opposite team of surrounded queen
            pending
        it "is a draw when both queens are surrounded in the same move" $ do
            -- contrive this scenario
            -- assert game is over in a draw state
            pending
        it "forces a player to pass when they have no free pieces" $ do
            -- set up a situation where a player has no free pieces
            -- assert that the resulting game state represents a pass for that player
            pending

