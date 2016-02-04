module Board
    -- (Board
    -- , allOccupiedPositions
    -- , isOccupiedAt
    -- , isUnoccupiedAt
    -- , occupiedNeighbors
    -- , unoccupiedNeighbors
    -- , topPieceAt
    -- , piecesAt
    -- , pieceIsFree
    -- , removePiecesAt
    -- , removeTopPieceAt
    -- , example1
    -- )
    where

import Piece
import HexGrid (AxialPoint(..))
import qualified HexGrid as Grid

import Data.Graph (stronglyConnComp, SCC(..))
import Data.Maybe (isJust, listToMaybe, fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- defensive programming: hide the details of the Board type
-- such that only this module can see them, so i don't forget
-- to deal with stacks (happened a lot in the clojure version)
newtype Board = Board { unBoard :: Map AxialPoint [Piece] }
  deriving (Eq, Show)

piecesAt :: Board -> AxialPoint -> [Piece]
piecesAt (Board board) pq = fromMaybe [] $ Map.lookup pq board

removePiecesAt :: Board -> AxialPoint -> Board
removePiecesAt (Board board) pq = Board $ Map.delete pq board

removeTopPieceAt :: Board -> AxialPoint -> Board
removeTopPieceAt (Board board) pq = Board $ Map.update tailOrDeath pq board
  where
    tailOrDeath []     = Nothing 
    tailOrDeath [_]    = Nothing -- don't retain empty lists
    tailOrDeath (_:xs) = Just xs

topPieceAt :: Board -> AxialPoint -> Maybe Piece
topPieceAt board pq = listToMaybe $ board `piecesAt` pq

isOccupiedAt :: Board -> AxialPoint -> Bool
isOccupiedAt board pq = not . null $ board `piecesAt` pq

isUnoccupiedAt :: Board -> AxialPoint -> Bool
isUnoccupiedAt = (not .) . isOccupiedAt

occupiedNeighbors :: Board -> AxialPoint -> [AxialPoint]
occupiedNeighbors board pq = filter (board `isOccupiedAt`) $ Grid.neighbors pq

unoccupiedNeighbors :: Board -> AxialPoint -> [AxialPoint]
unoccupiedNeighbors board pq = filter (board `isUnoccupiedAt`) $ Grid.neighbors pq

allOccupiedPositions :: Board -> [AxialPoint]
allOccupiedPositions (Board board) = Map.keys board

-- remove this piece from board
-- make a graph of the board
-- find strongly connected components of board graph
-- if there's more than one maximal SCC then the piece ain't free
pieceIsFree :: Board -> AxialPoint -> Bool
pieceIsFree board pq =
    case stronglyConnComp (boardToAdjacencyList board') of
        [CyclicSCC _] -> True
        _             -> False
  where
    board' = removeTopPieceAt board pq


-- convert a board (map of axial coords to Pieces) into a Data.Graph style adjacency list
-- each vertex is a piece's axial coordinates
-- each piece has edges to its occupied neighbors
-- adjlist format is [(node, key, [keys of other nodes this node has directed edges to])]
boardToAdjacencyList :: Board -> [(AxialPoint, AxialPoint, [AxialPoint])]
boardToAdjacencyList board@(Board bm) = map convert $ Map.keys bm
  where convert pq  = (pq, pq, occupiedNeighbors board pq)




--------------------------------------------------------------------------------
--Debug Helpers

instance Show a => Show (SCC a) where
    show (AcyclicSCC x) = "AcyclicSCC " ++ show x
    show (CyclicSCC xs) = "CyclicSCC " ++ show xs

-- XXX total hack, does not support stacks!
boardFromAscList = Board . Map.fromAscList . map (\((p,q), name) -> (Axial p q, [pieceFromName name]))

-- and here is where i miss clojure
-- ghci destroys all bindings when you :r
example1 = boardFromAscList
    [ ((3,4), "bQ")
    , ((2,5), "bS1")
    , ((2,6), "wS1")
    , ((1,7), "wQ")
    , ((1,6), "wG1")
    , ((2,4), "bL")
    , ((4,4), "bP")
    , ((0,7), "wA1")

    , ((4,5), "bA2")
    , ((4,6), "bA3")
    , ((3,7), "wS2")
    ]

example2 = boardFromAscList
    [ ((0,0), "wQ")
    , ((0,1), "bQ")
    , ((1,0), "wS1")
    ]

example3 = boardFromAscList
    [ ((0,0), "wQ")
    , ((0,1), "bQ")
    , ((0,2), "wS1")
    , ((0,3), "bS1")
    ]

example4 = boardFromAscList
    [ ((0,0), "wQ")
    , ((0,1), "bQ")
    , ((0,2), "wS1")
    , ((0,3), "bS1")
    , ((1,2), "wS2")
    ]

