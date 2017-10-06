module Main where

import qualified Test.Tasty as Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Debug.Trace (trace)

import Hive.Game.MoveBehaviors (pieceMovementSpec)
import Hive.Game.GameBehaviors (gameSpec)
import Hive.Game.Properties (gameProperties)

-- handy commands to remember:
-- $ stack test --file-watch
-- $ stack repl :hive-test-suite

main :: IO ()
main = do
    moveBehaviors <- testSpec "Piece Movement Behaviors" pieceMovementSpec
    gameBehaviors <- testSpec "Game Rule Behaviors" gameSpec
    Tasty.defaultMain $ Tasty.testGroup "All Tests" [moveBehaviors, gameBehaviors, gameProperties]

