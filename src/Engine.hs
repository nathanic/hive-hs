module Engine where

import Control.Monad (guard)
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import HexGrid (AxialPoint)
import qualified Data.Map.Strict as Map
import qualified HexGrid as Grid

-- game engine

data Species = Ant | Beetle | Grasshopper | Ladybug | Mosquito | Pillbug | QueenBee | Spider
  deriving (Eq,Show)

data Team = White | Black
  deriving (Eq,Show)

data Piece = Piece
    { pieceSpecies :: Species
    , pieceTeam :: Team
    , pieceName :: String
    } deriving (Eq,Show)

type Board = Map AxialPoint [Piece]

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

-- | the set of all possible pieces.
allPieces :: [Piece]
allPieces = map pieceFromName
             ["bA1","bA2","bA3"
             ,"bB1","bB2"
             ,"bG1","bG2","bG3"
             ,"bL"
             ,"bM"
             ,"bP"
             ,"bQ"
             ,"bS1","bS2"
             ,"wA1","wA2","wA3"
             ,"wB1","wB2"
             ,"wG1","wG2","wG3"
             ,"wL"
             ,"wM"
             ,"wP"
             ,"wQ"
             ,"wS1","wS2"
             ]

pieceFromName :: String -> Piece
pieceFromName name = Piece species team name
  where
    t:s:_ = name
    team = case t of
              'b' -> Black
              'w' -> White
              _   -> error "pieceFromName: invalid team"
    species = case s of
                'A' -> Ant
                'B' -> Beetle
                'G' -> Grasshopper
                'L' -> Ladybug
                'M' -> Mosquito
                'P' -> Pillbug
                'Q' -> QueenBee
                'S' -> Spider
                _   -> error "pieceFromName: invalid species"

-- allowedMoves :: Board -> Piece -> Graph.AxialPoint -> [Move]
-- allowedMoves board (Piece QueenBee _ _) pos

isOccupiedAt :: Board -> AxialPoint -> Bool
isOccupiedAt board pq = isJust $ Map.lookup pq board

isNotOccupiedAt board pq = not $ board `isOccupiedAt` pq

occupiedNeighbors :: Board -> AxialPoint -> [AxialPoint]
occupiedNeighbors board pq = filter (board `isOccupiedAt`) $ Grid.neighbors pq

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
    board' = Map.delete pqFrom board 
    (gate1,gate2) = Grid.gatePositions pqFrom pqTo
    xor = (/=)

planarPassibleNeighbors :: Board -> AxialPoint -> [AxialPoint]
planarPassibleNeighbors board pq = filter (isPlanarPassible board pq) $ Grid.neighbors pq

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


