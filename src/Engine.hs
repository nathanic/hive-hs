module Engine where

import Control.Category ((>>>))
import Control.Monad (guard)
import Data.List (find, nub, delete)
import qualified Data.List as List
import Data.Map.Strict (Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Graph as Graph

import HexGrid (AxialPoint(..))
import qualified HexGrid as Grid
import Piece
import Board

-- Start with the types!

data Game = Game { gameId :: Integer -- TODO: fancier ID type?
                 , gametitle :: String
                 , gameBoard :: Board
                 , gameUnplaced :: [Piece] -- Set?
                 , gameMoves :: [Move] -- move history
                 , gamePossibleMoves :: Map Piece [Move]
                 , gameSpawns :: [AxialPoint]
                 , gameTurn :: Team
                 , gameWinner :: Maybe Team
                 -- TODO: players' user account reference of some kind?
                 } deriving (Eq, Show)

data Move = Move { movePiece :: Piece
                 , moveCoords :: AxialPoint
                 -- , moveName :: String -- e.g. "bA2 /wG1"
                 } deriving (Eq,Show)

-- allowedMoves :: Board -> Piece -> Graph.AxialPoint -> [Move]
-- allowedMoves board (Piece QueenBee _ _) pos


-- | determine if two adjacent positions are planar-passable (not gated)
-- | XXX: only considers the bottommost plane! does not consider movement atop the hive
isPlanarPassable :: Board -> AxialPoint -> AxialPoint -> Bool
isPlanarPassable board pqFrom pqTo =
    not (board `isOccupiedAt` pqTo)
    && any (board' `isOccupiedAt`) (Grid.neighbors pqTo)
    -- one gate position or the other must be occupied, but not both
    && ((board' `isOccupiedAt` gate1)
        `xor` (board' `isOccupiedAt` gate2))
  where
    -- board' removes considered piece from the board
    board' = board `removePiecesAt` pqFrom
    (gate1,gate2) = Grid.gatePositions pqFrom pqTo
    xor = (/=)

planarPassableNeighbors :: Board -> AxialPoint -> [AxialPoint]
planarPassableNeighbors board pq = filter (isPlanarPassable board pq) $ Grid.neighbors pq

antMoves :: Board -> AxialPoint -> [AxialPoint]
antMoves board origin = delete origin reachable -- disallow starting pos
  where
    -- consider the board without this ant on it
    board' = removeTopPieceAt board origin
    -- find all empty hexes adjacent to occupied hexes (and dedupe)
    emptyBorderPositions = nub $ allOccupiedPositions board' >>= unoccupiedNeighbors board'
    -- build an adjacency list (graph) of these empty positions
    -- every empty border cell is a vertex, and has edges to every adjacent empty border cell
    emptyBorderAdjList = map (\pq -> (pq, pq, unoccupiedNeighbors board' pq)) emptyBorderPositions
    -- calculate strongly connected components, maximal islands of connectivity
    components = map Graph.flattenSCC $ Graph.stronglyConnComp emptyBorderAdjList
    -- we know for a fact that one of the components must have our origin
    -- containing the coordinates of all hexes reachable therefrom
    Just reachable = find (elem origin) components


beetleMoves :: Board -> AxialPoint -> [AxialPoint]
beetleMoves board origin =
    filter (\nabe -> board `isOccupiedAt` nabe || isPlanarPassable board origin nabe)
      $ Grid.neighbors origin

grasshopperMoves :: Board -> AxialPoint -> [AxialPoint]
grasshopperMoves board origin = do
    dir <- Grid.allDirections
    let nabe = Grid.neighbor dir origin
    guard (board `isOccupiedAt` nabe)
    -- scan in this direction until we see an unoccupied hex
    return $ head
           $ dropWhile (board `isOccupiedAt`)
           $ iterate (Grid.neighbor dir) nabe

-- oh monads, how i had missed you
ladybugMoves board origin = delete origin . nub $
    occupiedNeighbors board' origin
        >>= occupiedNeighbors board'
        >>= unoccupiedNeighbors board'
  where board' = board `removeTopPieceAt` origin

mosquitoMoves board origin = aggMoves $ occupiedNeighbors board origin
  where
    apply moves = moves board origin
    aggMoves = map (topPieceAt board
                        >>> fromJust
                        >>> pieceSpecies
                        >>> movesForSpecies
                        >>> apply
                        >>> Set.fromList)
                >>> Set.unions
                >>> Set.delete origin
                >>> Set.toList

-- pillbug will likely require special processing outside these handlers
pillbugMoves = planarPassableNeighbors

queenBeeMoves :: Board -> AxialPoint -> [AxialPoint]
queenBeeMoves = planarPassableNeighbors

spiderMoves :: Board -> AxialPoint -> [AxialPoint]
spiderMoves board origin = nub $ do
    dir <- Grid.allDirections
    let nabe = Grid.neighbor dir origin
    guard $ isPlanarPassable board origin nabe
    dir <- filter (/= dir) Grid.allDirections
    let nabe2 = Grid.neighbor dir nabe
    guard $ isPlanarPassable board nabe nabe2
    dir <- filter (/= dir) Grid.allDirections
    let nabe3 = Grid.neighbor dir nabe2
    guard $ isPlanarPassable board nabe2 nabe3
    return nabe3

-- | Precondition: piece at origin is free to move
movesForPieceAtPosition :: Board -> AxialPoint -> [AxialPoint]
movesForPieceAtPosition board origin =
    maybe (\_ _ -> [])
        (movesForSpecies . pieceSpecies)
        (topPieceAt board origin)
        board
        origin


-- there is probably some hot shit haskelly way to dispatch this
-- but honestly typing it out wasn't that painful
movesForSpecies :: Species -> Board -> AxialPoint -> [AxialPoint]
movesForSpecies species =
    case species of
        Ant -> antMoves
        Beetle -> beetleMoves
        Grasshopper -> grasshopperMoves
        Ladybug -> ladybugMoves
        Mosquito -> mosquitoMoves
        Pillbug -> pillbugMoves
        QueenBee -> queenBeeMoves
        Spider -> spiderMoves


{-


 -}
