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
import qualified Text.ParserCombinators.Parsec as Parsec

import HexGrid (AxialPoint(..))
import qualified HexGrid as Grid
import Piece
import Board
import Move

-- Start with the types!

data Game = Game { gameId :: Integer -- TODO: fancier ID type?
                 , gameTitle :: String
                 , gameBoard :: Board
                 , gameUnplaced :: [Piece] -- Set?
                 , gameMoves :: [AbsoluteMove] -- move history
                 , gamePossibleMoves :: Map Piece [AxialPoint]
                 , gameSpawns :: [AxialPoint]
                 , gameTurn :: Team
                 , gameWinner :: Maybe Team
                 -- TODO: players' user account reference of some kind?
                 } deriving (Eq, Show)



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

-- could do [AbsoluteMove] instead but a hashmap is just more convenient for the client
allMovesForGame :: Game -> Map Piece [AxialPoint]
allMovesForGame game = Map.unionWith (<>) movemap (pillbugProcessing game)
  where
    board = gameBoard game
    freePoses = allFreePiecePositions board
    movemap = foldl' buildMoveMap mempty freePoses
    buildMoveMap acc pos =
        case movesForPieceAtPosition board pos of
            [] -> acc -- XXX can this ever happen?  we are only folding over
                      -- free pieces, and free pieces must always have moves
                      -- that'd be an interesting quickcheck property:
                      -- generate random RelativeMoves, play the games
                      -- and at every point assert that every free piece
                      -- finds a nonzero amount of moves
            moves -> Map.insert (board `unsafeTopPieceAt` pos) moves acc

-- TODO: enforce queen rules
-- must be placed within X turns
-- no piece is free to move unless that team's queen is on the board
-- maybe also that no opening with the queen rule?

spawns :: Team -> Board -> [AxialPoint]
spawns team board =
    case (team, findTopPieces ((== team) . pieceTeam) board) of
        -- NB this assumes White always goes first
        (White, []) -> [Axial 0 0]
        -- but we do support any initial placement pos for white
        (Black, []) -> Grid.neighbors $ head $ findTopPieces (const True) board
        -- spawns are empty hexes that are both bordering friendlies and not bordering foes
        (_, friendlies) -> [ emptyNabe
                           | friendly <- friendlies
                           , emptyNabe <- unoccupiedNeighbors board friendly
                           , not $ any ((== opposing team) . pieceTeam . unsafeTopPieceAt board)
                                 $ occupiedNeighbors board emptyNabe
                           ]

-- it would be nice if this gave feedback on exactly why the move is invalid
isValidMove :: Game -> AbsoluteMove -> Bool
isValidMove game (AbsoluteMove piece pos) =
    gameTurn game == pieceTeam piece && (firstMove || possibleMove || spawnMove)
  where
    firstMove = null $ gameMoves game
    possibleMove = elem pos $ fromMaybe [] (Map.lookup piece $ gamePossibleMoves game)
    spawnMove = elem piece (gameUnplaced game) && elem pos (gameSpawns game)

applyMoveToBoard :: AbsoluteMove -> Board -> Board
applyMoveToBoard (AbsoluteMove piece to) board =
    case findTopPieces (== piece) board of
        [] -> addPiece piece to board
        [from] -> movePieceTo board from to
        _ -> error "broken game! multiple instances of the same piece in the board."

applyMove :: AbsoluteMove -> Game -> Either String Game
applyMove move@(AbsoluteMove piece pos) game
    | isValidMove game move = Right game'
    | otherwise             = Left "invalid move!"
  where
    Game { gameBoard = board
         , gameMoves = history
         , gameTurn = turn
         , gameUnplaced = unplaced
         } = game
    game' = game { gameBoard = board'
                 , gameMoves = history <> [AbsoluteMove piece pos]
                 , gamePossibleMoves = allMovesForGame game' -- circular!
                 , gameSpawns = spawns (opposing turn) board'
                 , gameUnplaced = delete piece unplaced
                 , gameTurn = opposing turn
                 }
    board' = applyMoveToBoard move board


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


g5 = dummyGameFromBoard example5
Right g5' = applyMove (AbsoluteMove (piece "wS1") (Axial 0 0)) g5

g = dummyGameFromBoard emptyBoard
Right g' = applyMove (AbsoluteMove (piece "wS1") (Axial 0 0)) g

