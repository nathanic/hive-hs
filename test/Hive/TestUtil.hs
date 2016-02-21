module Hive.TestUtil where

import Data.List (init)

import Hive.Board
import Hive.HexGrid (AxialPoint(..))
import Hive.Piece

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

