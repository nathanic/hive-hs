module Engine where

import Control.Monad (guard)
import Data.Map.Strict (Map)

import HexGrid (AxialPoint)
import qualified HexGrid as Grid
import Piece
import Board (Board, isOccupiedAt, isNotOccupiedAt, occupiedNeighbors, piecesAt, topPieceAt, removePiecesAt, removeTopPieceAt)

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
-- | NOTE: only considers the bottommost plane! does not consider movement atop the hive
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
antMoves = undefined
-- this one involves some graph theory
-- probably have to look into fgl
-- convert board into graph
-- then successors query etc.

beetleMoves :: Board -> AxialPoint -> [AxialPoint]
beetleMoves board pq =
    filter (\nabe -> board `isOccupiedAt` nabe || isPlanarPassible board pq nabe)
      $ Grid.neighbors pq

grasshopperMoves :: Board -> AxialPoint -> [AxialPoint]
grasshopperMoves board pq = do
    dir <- Grid.allDirections
    let nabe = Grid.neighbor dir pq
    guard (board `isOccupiedAt` nabe)
    -- scan in this direction until we see an unoccupied hex
    return $ head
           $ dropWhile (board `isOccupiedAt`)
           $ iterate (Grid.neighbor dir) nabe

ladybugMoves board pq = do
   nabe1 <- occupiedNeighbors board' pq
   nabe2 <- occupiedNeighbors board' nabe1
   nabe3 <- Grid.neighbors nabe2
   guard (board' `isNotOccupiedAt` nabe3)
   return nabe3
 where
    board' = board `removePiecesAt` pq
    -- XXX: or just remove top piece? but i guess ladybugs can never be on top
    -- considering making board more opaque so i have to be more mindful of stacks

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

-- TODO: a thingy dispatch on the Species type to the above handlers

