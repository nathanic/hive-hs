module Board
    (Board
    , isOccupiedAt
    , isNotOccupiedAt
    , occupiedNeighbors
    , topPieceAt
    , piecesAt
    , removePiecesAt
    , removeTopPieceAt)
    where

import Piece
import HexGrid (AxialPoint)
import qualified HexGrid as Grid

import Data.Maybe (isJust, listToMaybe, fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- defensive programming: hide the details of the Board type
-- such that only this module can see them, so i don't forget
-- to deal with stacks (happened a lot in the clojure version)
newtype Board = Board { fromBoard :: Map AxialPoint [Piece] }
  deriving (Eq, Show)

piecesAt :: Board -> AxialPoint -> [Piece]
piecesAt (Board board) pq = fromMaybe [] $ Map.lookup pq board

removePiecesAt :: Board -> AxialPoint -> Board
removePiecesAt (Board board) pq = Board $ Map.delete pq board

removeTopPieceAt :: Board -> AxialPoint -> Board
removeTopPieceAt (Board board) pq = Board $ Map.update tailOrDeath pq board
  where
    tailOrDeath []     = Nothing -- don't retain empty lists
    tailOrDeath (_:xs) = Just xs

topPieceAt :: Board -> AxialPoint -> Maybe Piece
topPieceAt board pq = listToMaybe $ board `piecesAt` pq

isOccupiedAt :: Board -> AxialPoint -> Bool
isOccupiedAt board pq = not . null $ board `piecesAt` pq

isNotOccupiedAt = (not .) . isOccupiedAt

occupiedNeighbors :: Board -> AxialPoint -> [AxialPoint]
occupiedNeighbors board pq = filter (board `isOccupiedAt`) $ Grid.neighbors pq
