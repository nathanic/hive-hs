module Engine where

import Control.Category ((>>>))
import Control.Monad (guard)
import Data.List (find, nub, delete, foldl')
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set

import HexGrid (AxialPoint(..))
import qualified HexGrid as Grid
import Piece
import Board

-- Start with the types!

data Game = Game { gameId :: Integer -- TODO: fancier ID type?
                 , gameTitle :: String
                 , gameBoard :: Board
                 , gameUnplaced :: [Piece] -- Set?
                 , gameMoves :: [Move] -- move history
                 , gamePossibleMoves :: Map Piece [AxialPoint]
                 , gameSpawns :: [AxialPoint]
                 , gameTurn :: Team
                 , gameWinner :: Maybe Team
                 -- TODO: players' user account reference of some kind?
                 } deriving (Eq, Show)

data Move = Move { movePiece :: Piece
                 , moveCoords :: AxialPoint
                 -- , moveName :: String -- e.g. "bA2 /wG1"
                 } deriving (Eq,Show)


-- | determine if two adjacent positions are planar-passable (not gated)
-- | XXX: only considers the bottommost plane! does not consider movement atop the hive
isPlanarPassable :: Board -> AxialPoint -> AxialPoint -> Bool
isPlanarPassable board from to =
    not (board `isOccupiedAt` to)
    && any (board' `isOccupiedAt`) (Grid.neighbors to)
    -- one gate position or the other must be occupied, but not both
    && (board' `isOccupiedAt` gate1) /= (board' `isOccupiedAt` gate2)
  where
    -- board' removes considered piece from the board
    board' = removeTopPiece from board
    (gate1,gate2) = Grid.gatePositions from to

planarPassableNeighbors :: Board -> AxialPoint -> [AxialPoint]
planarPassableNeighbors board pos = filter (isPlanarPassable board pos) $ Grid.neighbors pos

antMoves :: Board -> AxialPoint -> [AxialPoint]
antMoves board origin = delete origin reachable -- disallow starting pos
  where
    -- consider the board without this ant on it
    board' = removeTopPieceAt board origin
    -- find all empty hexes adjacent to occupied hexes (and dedupe)
    emptyBorderPositions = nub $ allOccupiedPositions board' >>= unoccupiedNeighbors board'
    -- build an adjacency list (graph) of these empty positions
    -- every empty border cell is a vertex, and has edges to every adjacent empty border cell
    emptyBorderAdjList = map (\pos -> (pos, pos, unoccupiedNeighbors board' pos)) emptyBorderPositions
    -- calculate connected components, islands of connectivity
    components = connectedComponents emptyBorderAdjList
    -- we know for a fact that one of the components must have our origin
    -- containing the coordinates of all hexes reachable therefrom
    Just reachable = find (elem origin) components

beetleMoves :: Board -> AxialPoint -> [AxialPoint]
beetleMoves board origin = filter kosher $ Grid.neighbors origin
  where
    kosher nabe = board' `isOccupiedAt` nabe || isPlanarPassable board' origin nabe
    board' = removeTopPieceAt board origin

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

mosquitoMoves board origin
    | board `isStacked` origin = beetleMoves board origin
    | otherwise                = Set.toList . Set.delete origin . Set.unions $ movesets
        -- XXX ideally, shouldn't need to delete, as other handlers should do that...
        where
            movesets = [Set.fromList $ movesForSpecies species board origin
                            | nabe <- occupiedNeighbors board origin
                            , let species = pieceSpecies $ board `unsafeTopPieceAt` nabe
                            , species /= Mosquito]


-- the pillbug itself moves like a queen, but see below for special powers
pillbugMoves = planarPassableNeighbors

-- pillbug requires out-of-band handling since it alone can move other pieces around
pillbugProcessing :: Game -> Map Piece [AxialPoint]
pillbugProcessing game = Map.unionsWith (<>) (handlePillbug <$> pillbugPoses)
  where
    Game { gameBoard = board, gameMoves = history } = game
    pillbugPoses = findTopPiecesBySpecies Pillbug board
    handlePillbug pillbugPos = Map.fromList $ zip victims (repeat targets)
      where
          victims = [ piece
                      | pos <- occupiedNeighbors board pillbugPos
                      , let Just piece = board `topPieceAt` pos
                      , didntJustMove piece]
          targets = unoccupiedNeighbors board pillbugPos -- XXX BUG does not check upper planar passability
    -- XXX wait, is it the last TWO moves? need to check rules
    didntJustMove piece = null history || piece /= movePiece (last history)


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

allMovesForGame :: Game -> Map Piece [AxialPoint]
allMovesForGame game = Map.unionWith (<>) movemap (pillbugProcessing game)
  where
    board = gameBoard game
    freePoses = allFreePiecePositions board
    movemap = foldl' (\acc pos -> Map.insert (board `unsafeTopPieceAt` pos)
                                   (movesForPieceAtPosition board pos)
                                   acc)
                mempty
                freePoses

spawns :: Team -> Board -> [AxialPoint]
spawns team board = do
    friendly <- findTopPieces (\p -> pieceTeam p == team) board
    emptyNabe <- unoccupiedNeighbors board friendly
    guard $ not $ any (\pos -> pieceTeam (unsafeTopPieceAt board pos) == opposing team)
          $ occupiedNeighbors board emptyNabe
    return emptyNabe

isValidMove :: Game -> Piece -> AxialPoint -> Bool
isValidMove game piece pos = gameTurn game == pieceTeam piece && possible
  where possible = elem pos $ fromMaybe [] (Map.lookup piece $ gamePossibleMoves game)

applyMoveToBoard :: Piece -> AxialPoint -> Board -> Board
applyMoveToBoard piece to board =
    case findTopPieces (== piece) board of
        [] -> addPiece piece to board
        [from] -> movePieceTo board from to
        _ -> error "broken game! multiple instances of the same piece in the board."

applyMove :: Piece -> AxialPoint -> Game -> Either String Game
applyMove piece pos game
    | isValidMove game piece pos = Right game'
    | otherwise                  = Left "invalid move!"
  where
    Game { gameBoard = board
         , gameMoves = history
         , gameTurn = turn
         , gameUnplaced = unplaced
         } = game
    game' = game { gameBoard = board'
                 , gameMoves = history <> [Move piece pos]
                 , gamePossibleMoves = allMovesForGame game' -- circular!
                 , gameSpawns = spawns (opposing turn) board'
                 , gameUnplaced = delete piece unplaced
                 , gameTurn = opposing turn
                 }
    board' = applyMoveToBoard piece pos board



-- So, for the client side, I'm going to write my first purescript ever.
-- I was originally thinking canvas, but I want to try just html/css
-- I want to target mobile, where I feel like fancy css is noticably faster than canvas
-- This looks awesome:
-- https://github.com/web-tiki/responsive-grid-of-hexagons
-- modify that to suit my needs
-- create a grid of hexes with axial ids like "hex-1-3"
-- render function maps from the game state to classes/attrs of these hex cells
--
-- maybe use bodil's signal lib?

--------------------------------------------------------------------------------
-- Debug helpers

findUnplaced board = allPieces List.\\ allPiecesOnBoard board

dummyGameFromBoard :: Board -> Game
dummyGameFromBoard board = Game { gameId = 0
                                , gameTitle = "dummy"
                                , gameBoard = board
                                , gameUnplaced = findUnplaced board
                                , gameMoves = []
                                , gamePossibleMoves = mempty
                                , gameSpawns = []
                                , gameTurn = White
                                , gameWinner = Nothing
                                }

