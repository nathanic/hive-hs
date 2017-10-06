module Hive.Game.TestUtil where

import Control.Monad (unless)
import Data.List (init, (\\))

import Test.Tasty.Hspec

import Hive.Game.Board
import Hive.Game.HexGrid (AxialPoint(..))
import Hive.Game.Piece

safeDrop 0 xs = xs
safeDrop n [] = []
safeDrop n (_:xs) = safeDrop (n-1) xs

safeTake 0 _ = []
safeTake n [] = []
safeTake n (x:xs) = x : safeTake (n-1) xs

safeTail [] = []
safeTail (_:xs) = xs

safeButLast [] = []
safeButLast xs = init xs


-- some helpers for building test Boards

addPieces :: [(Int,Int,Piece)] -> Board -> Board
addPieces = flip $ foldl (\b (p, q, pc) -> addPiece pc (Axial p q) b)

makeBoard :: [(Int,Int,Piece)] -> Board
makeBoard spec = addPieces spec emptyBoard


-- surprised this one wasn't in hspec already
shouldContainElements :: (Show a, Eq a) => [a] -> [a] -> Expectation
shouldContainElements haystack needles = unless (null missing) (expectationFailure message)
  where
    missing = needles \\ haystack
    message = "Actual list " ++ show haystack ++ " does not contain expected elements " ++ show missing

shouldNotContainElements :: (Show a, Eq a) => [a] -> [a] -> Expectation
shouldNotContainElements haystack antineedles = unless (unfound == antineedles) (expectationFailure message)
  where
    unfound = antineedles \\ haystack
    message = "Actual list " ++ show haystack ++ " contains unexpected elements " ++ show unfound
