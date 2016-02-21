module Main where

import qualified Test.Tasty as Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Debug.Trace (trace)

import Hive.MoveBehaviors (pieceMovementSpec)
import Hive.GameProperties (gameProperties)

-- handy commands to remember:
-- $ stack test --file-watch
-- $ stack repl :hive-test-suite

main :: IO ()
main = do
    behaviors <- testSpec "Piece Movement Behaviors" pieceMovementSpec
    Tasty.defaultMain $ Tasty.testGroup "All Tests" [behaviors, gameProperties]

