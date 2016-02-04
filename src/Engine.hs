module Engine where

import Control.Monad (guard)
import Data.List (find, nub, delete)
import Data.Map.Strict (Map)
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
isPlanarPassible :: Board -> AxialPoint -> AxialPoint -> Bool
isPlanarPassible board pqFrom pqTo =
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

planarPassibleNeighbors :: Board -> AxialPoint -> [AxialPoint]
planarPassibleNeighbors board pq = filter (isPlanarPassible board pq) $ Grid.neighbors pq

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
    filter (\nabe -> board `isOccupiedAt` nabe || isPlanarPassible board origin nabe)
      $ Grid.neighbors origin

grasshopperMoves :: Board -> AxialPoint -> [AxialPoint]
grasshopperMoves board pq = do
    dir <- Grid.allDirections
    let nabe = Grid.neighbor dir pq
    guard (board `isOccupiedAt` nabe)
    -- scan in this direction until we see an unoccupied hex
    return $ head
           $ dropWhile (board `isOccupiedAt`)
           $ iterate (Grid.neighbor dir) nabe

-- oh monads, how i had missed you
ladybugMoves board pq =
    occupiedNeighbors board' pq
        >>= occupiedNeighbors board'
        >>= unoccupiedNeighbors board'
  where board' = board `removeTopPieceAt` pq

mosquitoMoves = undefined
-- this will involve some rewriting of the board as different pieces
-- and unioning up all the possibilities

-- pillbug will likely require special processing outside these handlers
pillbugMoves = planarPassibleNeighbors

queenBeeMoves :: Board -> AxialPoint -> [AxialPoint]
queenBeeMoves = planarPassibleNeighbors

spiderMoves :: Board -> AxialPoint -> [AxialPoint]
spiderMoves board pq = do
    dir <- Grid.allDirections
    let nabe = Grid.neighbor dir pq
    guard $ isPlanarPassible board pq nabe
    dir <- filter (/= dir) Grid.allDirections
    let nabe2 = Grid.neighbor dir nabe
    guard $ isPlanarPassible board nabe nabe2
    dir <- filter (/= dir) Grid.allDirections
    let nabe3 = Grid.neighbor dir nabe
    guard $ isPlanarPassible board nabe2 nabe3
    return nabe3

-- | Precondition: piece at pq is free to move
movesForPieceAtPosition :: Board -> AxialPoint -> [AxialPoint]
movesForPieceAtPosition board origin = calcMoves board origin
  where
    -- there is probably some hot shit haskelly way to dispatch this
    -- but honestly typing it out wasn't that painful
    calcMoves = case pieceSpecies <$> topPieceAt board origin of
        Just Ant -> antMoves
        Just Beetle -> beetleMoves
        Just Grasshopper -> grasshopperMoves
        Just Ladybug -> ladybugMoves
        Just Mosquito -> mosquitoMoves
        Just Pillbug -> pillbugMoves
        Just QueenBee -> queenBeeMoves
        Just Spider -> spiderMoves
        Nothing -> \_ _ -> []


{-

queenBeeMoves example1 (Axial 3 4)



 -}
