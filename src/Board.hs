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

import qualified Data.Graph as Graph
import Data.List (nub)
import Data.Maybe (isJust, listToMaybe, fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- defensive programming: hide the details of the Board type
-- such that only this module can see them, so i don't forget
-- to deal with stacks (happened a lot in the clojure version)
newtype Board = Board { unBoard :: Map AxialPoint [Piece] }
  deriving (Eq, Show)

piecesAt :: Board -> AxialPoint -> [Piece]
piecesAt (Board bmap) pq = fromMaybe [] $ Map.lookup pq bmap

removePiecesAt :: Board -> AxialPoint -> Board
removePiecesAt (Board bmap) pq = Board $ Map.delete pq bmap

removeTopPieceAt :: Board -> AxialPoint -> Board
removeTopPieceAt (Board bmap) pq = Board $ Map.update tailOrDeath pq bmap
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
allOccupiedPositions (Board bmap) = Map.keys bmap

allPiecesOnBoard  = concat . Map.elems . unBoard

pieceIsFree :: Board -> AxialPoint -> Bool
pieceIsFree board pq = isOneHive (board `removeTopPieceAt` pq)

-- convert a board (map of axial coords to Pieces) into a Data.Graph style adjacency list
-- each vertex is a piece's axial coordinates
-- each piece has edges to its occupied neighbors
-- adjlist format is [(node, key, [keys of other nodes this node has directed edges to])]
boardToAdjacencyList :: Board -> [(AxialPoint, AxialPoint, [AxialPoint])]
boardToAdjacencyList board@(Board bm) = map convert $ Map.keys bm
  where convert pq  = (pq, pq, occupiedNeighbors board pq)

-- | Is the board a contiguous set of pieces? (Does it satisfy the One Hive Rule?)
-- firstly, we must have no more than one strongly connected component in the board graph
isOneHive :: Board -> Bool
isOneHive b =
    case Graph.stronglyConnComp $ boardToAdjacencyList b of
        []  -> True  -- empty board is okay(?)
        [_] -> True  -- nonempty must have exactly one strongly connected graph o' pieces
        _   -> False -- multiple disconnected islands of pieces -> bad board

isValidBoard :: Board -> Bool
isValidBoard b = isOneHive b && length allPieces == length (nub allPieces)
  where allPieces = allPiecesOnBoard b


--------------------------------------------------------------------------------
--Debug Helpers

instance Show a => Show (Graph.SCC a) where
    show (Graph.AcyclicSCC x) = "AcyclicSCC " ++ show x
    show (Graph.CyclicSCC xs) = "CyclicSCC " ++ show xs

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

example5 = boardFromAscList
  [ ((1,4), "wQ")
  , ((1,5), "wA1")
  , ((2,3), "wL")
  , ((2,4), "wS1")
  , ((3,4), "bS1")
  , ((3,5), "bA1")
  , ((4,3), "bQ")
  , ((4,4), "bG1")
  ]

-- YO NATHAN
-- READ THIS
-- the next thing you need to do is write some tests
-- because probably a lot of this shit is wrong

-- though testing would probably be easier if i had a move parser
-- and it'd be fun to get parsec out...

