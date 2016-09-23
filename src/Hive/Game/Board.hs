module Hive.Game.Board
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

import Hive.Game.Piece
import Hive.Game.HexGrid (AxialPoint(..))
import qualified Hive.Game.HexGrid as Grid

import qualified Data.Graph as Graph
import Data.List (nub)
import Data.Maybe (isJust, listToMaybe, fromJust, fromMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import qualified Data.Tree as Tree

-- defensive programming: hide the details of the Board type
-- such that only this module can see them, so i don't forget
-- to deal with stacks (happened a lot in the clojure version)
newtype Board = Board { unBoard :: Map AxialPoint [Piece] }
  deriving (Eq, Show)

piecesAt :: Board -> AxialPoint -> [Piece]
piecesAt (Board bmap) pos = fromMaybe [] $ Map.lookup pos bmap

isStacked :: Board -> AxialPoint -> Bool
isStacked board pos =
    case piecesAt board pos of
        (_:_:_) -> True
        _       -> False

addPiece :: Piece -> AxialPoint -> Board -> Board
addPiece piece pos = Board . Map.insertWith (++) pos [piece] . unBoard

movePieceTo :: Board -> AxialPoint -> AxialPoint -> Board
movePieceTo board from to = addPiece piece to $ board `removeTopPieceAt` from
  where piece = unsafeTopPieceAt board from

removeTopPieceAt :: Board -> AxialPoint -> Board
removeTopPieceAt (Board bmap) pos = Board $ Map.update tailOrDeath pos bmap
  where
    tailOrDeath []     = Nothing
    tailOrDeath [_]    = Nothing -- don't retain empty lists
    tailOrDeath (_:xs) = Just xs

topPieceAt :: Board -> AxialPoint -> Maybe Piece
topPieceAt board pos = listToMaybe $ board `piecesAt` pos

unsafeTopPieceAt = (fromJust .) . topPieceAt

isOccupiedAt :: Board -> AxialPoint -> Bool
isOccupiedAt board pos = not . null $ board `piecesAt` pos

isUnoccupiedAt :: Board -> AxialPoint -> Bool
isUnoccupiedAt = (not .) . isOccupiedAt

occupiedNeighbors :: Board -> AxialPoint -> [AxialPoint]
occupiedNeighbors board = filter (board `isOccupiedAt`) . Grid.neighbors

unoccupiedNeighbors :: Board -> AxialPoint -> [AxialPoint]
unoccupiedNeighbors board = filter (board `isUnoccupiedAt`) . Grid.neighbors

allOccupiedPositions :: Board -> [AxialPoint]
allOccupiedPositions = Map.keys . unBoard

allPiecesOnBoard  = concat . Map.elems . unBoard

findPieces :: (Piece -> Bool) -> Board -> [(AxialPoint, [Piece])]
findPieces f = Map.toList . Map.filter (any f) . unBoard

findTopPieces :: (Piece -> Bool) -> Board -> [AxialPoint]
findTopPieces f board = Map.foldlWithKey
                            (\acc pos pieces ->
                                case pieces of
                                    [] -> error $ "found a board site with empty piecelist! "
                                        <> show pos <> "\nboard with empty site: "
                                        <> show board <> "\n"
                                    (pc:_) -> if f pc
                                        then pos:acc
                                        else acc)
                            []
                            (unBoard board)

findTopPiecesBySpecies spec = findTopPieces (\pc -> spec == pieceSpecies pc)

-- | N.B. this is really a check that the given piece is not an articulation point
-- in the board graph; a "free" position found by this fn could still be
-- prevented from moving by being surrounded etc.
pieceIsFree :: Board -> AxialPoint -> Bool
pieceIsFree board pos = isOneHive (board `removeTopPieceAt` pos)

allFreePiecePositions :: Board -> [AxialPoint]
allFreePiecePositions board = filter (pieceIsFree board) $ allOccupiedPositions board

allFreePieces :: Board -> [Piece]
allFreePieces board =
    map (fromJust . topPieceAt board) $
    filter (pieceIsFree board) $
    allOccupiedPositions board

freePiecePositionsForTeam :: Team -> Board -> [AxialPoint]
freePiecePositionsForTeam team board =
    filter (\p -> pieceTeam (unsafeTopPieceAt board p) == team && pieceIsFree board p)
        $ allOccupiedPositions board

-- convert a board (map of axial coords to Pieces) into a Data.Graph style adjacency list
-- each vertex is a piece's axial coordinates
-- each piece has edges to its occupied neighbors
-- adjlist format is [(node, key, [keys of other nodes this node has directed edges to])]
boardToAdjacencyList :: Board -> [(AxialPoint, AxialPoint, [AxialPoint])]
boardToAdjacencyList board@(Board bm) = map convert $ Map.keys bm
  where convert pos  = (pos, pos, occupiedNeighbors board pos)

-- | Is the board a contiguous set of pieces? (Does it satisfy the One Hive Rule?)
-- firstly, we must have no more than one connected component in the board graph
isOneHive :: Board -> Bool
isOneHive b =
    case connectedComponents $ boardToAdjacencyList b of
        [_] -> True  -- nonempty must have exactly one connected component o' pieces
        _   -> False -- empty or multiple disconnected islands of pieces -> bad board

isValidBoard :: Board -> Bool
isValidBoard b =
    isOneHive b
    && length allPieces == length (nub allPieces)
    && stackedPiecesAreStackable
  where
    allPieces = allPiecesOnBoard b
    isStackable pc = let species = pieceSpecies pc
                      in species == Beetle || species == Mosquito
    stackedPiecesAreStackable = all (all isStackable . init) $
                                    map (piecesAt b) $
                                    filter (isStacked b) $
                                        allOccupiedPositions b

connectedComponents :: Ord key => [(node,key,[key])] -> [[node]]
connectedComponents adjlist = deforest $ Graph.components g
  where
    (g,vertexInfo,_) = Graph.graphFromEdges adjlist
    deforest = map (map (fst3 . vertexInfo) . Tree.flatten)
    fst3 (x,_,_) = x

--------------------------------------------------------------------------------
-- Debug Helpers and REPL scratch

instance Show a => Show (Graph.SCC a) where
    show (Graph.AcyclicSCC x) = "AcyclicSCC " ++ show x
    show (Graph.CyclicSCC xs) = "CyclicSCC " ++ show xs

-- XXX total hack, does not support stacks!
boardFromAscList = Board . Map.fromAscList . map (\((p,q), name) -> (Axial p q, [piece name]))

emptyBoard = Board mempty

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

-- https://en.wikipedia.org/wiki/Strongly_connected_component
al = [('a', 'a', ['b'])
     ,('b', 'b', ['c','e','f'])
     ,('c', 'c', ['d','g'])
     ,('d', 'd', ['c','h'])
     ,('e', 'e', ['a','f'])
     ,('f', 'f', ['g'])
     ,('g', 'g', ['f'])
     ,('h', 'h', ['d','g'])

     ,('x', 'x', ['y'])
     ,('y', 'y', [])
     ]


