
module Hive.Engine where

import Control.Arrow ((&&&))
import Control.Category ((>>>))
import Control.Monad (guard, foldM, when)
import Control.Exception

import Data.List (find, nub, nubBy, delete, foldl', maximumBy, sort, inits)
import qualified Data.List as List
import Data.Function (on)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust, fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set

import Hive.HexGrid (AxialPoint(..))
import qualified Hive.HexGrid as Grid
import Hive.Piece
import Hive.Board
import Hive.Move

import Debug.Trace

data GameState = Playing | Won Team | Draw
  deriving (Eq, Show)

data Game = Game { gameBoard :: Board
                 , gameUnplaced :: [Piece] -- Set?
                 , gameMoves :: [AbsoluteMove] -- move history
                 -- maybe the name should reflect that this is only board moves (not spawns)
                 , gamePossibleMoves :: Map Piece [AxialPoint]
                 , gameSpawnablePieces :: [Piece]
                 , gameSpawnPositions :: [AxialPoint]
                 , gameTurn :: Team
                 , gameState :: GameState
                 } deriving (Eq, Show)

-- | Determine if two adjacent positions are planar-passable (not gated, and not off the hive).
-- NB: This only considers the bottommost plane! does not consider movement atop the hive.
-- for that, see isUpperPassable.
isPlanarPassable :: Board -> AxialPoint -> AxialPoint -> Bool
isPlanarPassable board from to =
    not (board `isOccupiedAt` to)
    && any (board' `isOccupiedAt`) (Grid.neighbors to)
    -- one gate position or the other must be occupied, but not both
    && (board' `isOccupiedAt` gate1) /= (board' `isOccupiedAt` gate2)
  where
    -- board' removes considered piece from the board
    board' = board `removeTopPieceAt` from
    (gate1,gate2) = Grid.gatePositions from to

-- | If a piece is atop the hive, is it free to move from `from` to `to` given its z-level?
isUpperPasssable :: Board -> AxialPoint -> AxialPoint -> Bool
isUpperPasssable board from to = error "TODO: this. upper passability"

-- it'd be nice to have a grand unified passability model

planarPassableNeighbors :: Board -> AxialPoint -> [AxialPoint]
planarPassableNeighbors board pos = filter (isPlanarPassable board pos) $ Grid.neighbors pos

upperPassableNeighbors :: Board -> AxialPoint -> [AxialPoint]
upperPassableNeighbors board pos = filter (isPlanarPassable board pos) $ Grid.neighbors pos

--------------------------------------------------------------------------------
-- | Per-Species Movement Rules
--------------------------------------------------------------------------------
-- Laws for Move Functions:
--    - never allow the origin point, because that's not really a move.
--    - never examine your own Piece, so that the Mosquito can emulate you.
-- Common Patterns:
--    - consider the board with the moving piece removed, as this is usually
--    the right board graph to consider, since the moving piece won't be at the
--    origin anymore. duh.
--    - many can just get away with planarPassableNeighbors or monadic
--    compositions thereof, possibly with some filtering.
--    - a few have to construct graphs (from adjlists) and query connected components.

-- | Ants can make as many planar-passable moves as they please around the
-- hive. The origin point is disallowed, since that's not really a move.
antMoves :: Board -> AxialPoint -> [AxialPoint]
antMoves board origin = delete origin reachable -- disallow starting pos
  where
    -- consider the board without this ant on it
    board' = removeTopPieceAt board origin
    -- find all empty hexes adjacent to occupied hexes (and dedupe)
    emptyBorderPositions = nub $ allOccupiedPositions board' >>= unoccupiedNeighbors board'
    -- build an adjacency list (graph) of these empty positions
    -- every empty border cell is a vertex, and has edges to every adjacent empty border cell
    emptyBorderAdjList = map (\pos -> (pos, pos, planarPassableNeighbors board' pos)) emptyBorderPositions

    -- calculate connected components, islands of connectivity
    components = connectedComponents emptyBorderAdjList
    -- we know for a fact that one of the components must have our origin
    -- containing the coordinates of all hexes reachable therefrom
    Just reachable = find (elem origin) components


-- BUG: does not consider upper level gates
-- BUG: does not allow dropping into a gated position from above
beetleMoves :: Board -> AxialPoint -> [AxialPoint]
beetleMoves board origin = filter kosher $ Grid.neighbors origin
  where
    kosher nabe
        | isStacked board' origin = True -- isUpperPasssable board origin nabe
        | otherwise               = board' `isOccupiedAt` nabe || isPlanarPassable board' origin nabe
    board' = removeTopPieceAt board origin

-- | Grasshoppers leapfrog in straight directions over arbitrarily many bugs,
-- stopping at the first free hex they encounter. Hopping ignores normal
-- passability rules.
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
-- TODO: support mosquitos that can act as pillbugs
pillbugProcessing :: Game -> Map Piece [AxialPoint]
pillbugProcessing game = Map.unionsWith (<>) (handlePillbug <$> pillbugPoses)
  where
    Game { gameBoard = board, gameMoves = history } = game
    pillbugPoses = findTopPiecesBySpecies Pillbug board
    handlePillbug pillbugPos = Map.fromList $ zip victims (repeat targets)
      where
          victims = [ piece
                      | pos <- occupiedNeighbors board pillbugPos
                      , pieceIsFree board pos
                      , let Just piece = board `topPieceAt` pos
                      , didntJustMove piece]
          targets = unoccupiedNeighbors board pillbugPos -- XXX BUG does not check upper planar passability
    didntJustMove piece = null history || piece /= movePiece (last history)

queenBeeMoves :: Board -> AxialPoint -> [AxialPoint]
queenBeeMoves = planarPassableNeighbors

-- XXX something is still wrong with this
-- just had a spider move out into empty space, translating SW by one hex
-- from (-1,-4) to (-2, -3)
spiderMoves :: Board -> AxialPoint -> [AxialPoint]
spiderMoves board origin = nub $ do
    let board' = removeTopPieceAt board origin
    dir <- Grid.allDirections
    let nabe = Grid.neighbor dir origin
    guard $ isPlanarPassable board' origin nabe
    dir <- filter (/= Grid.opposite dir) Grid.allDirections
    let nabe2 = Grid.neighbor dir nabe
    guard $ isPlanarPassable board' nabe nabe2
    dir <- filter (/= Grid.opposite dir) Grid.allDirections
    let nabe3 = Grid.neighbor dir nabe2
    guard $ isPlanarPassable board' nabe2 nabe3
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
-- (this is just the board-to-board moves, does not include spawns)
allBoardMovesForGame :: Game -> Map Piece [AxialPoint]
allBoardMovesForGame game
    | queenIsUnplaced = mempty
    | otherwise       = Map.unionWith (<>) movemap (pillbugProcessing game)
  where
    queenIsUnplaced = isJust $ find isQueenBee $ thisTeamUnplaced game
    board = gameBoard game
    thisTeam = gameTurn game
    freePoses = freePiecePositionsForTeam thisTeam board
    movemap = foldl' buildMoveMap mempty freePoses
    buildMoveMap acc pos =
        case movesForPieceAtPosition board pos of
            [] -> acc
                    -- sooooo, i think this happens when a piece is "free" in terms of
                    -- the board graph (is not an articulation point), but can't move due to being gated
                    -- TODO: maybe rename stuff to clarify what sense of "free" we are talking about
            moves -> Map.insert (board `unsafeTopPieceAt` pos) moves acc


spawnPositions :: Team -> Board -> [AxialPoint]
spawnPositions team board =
    case (team, findTopPieces ((== team) . pieceTeam) board) of
        -- NB this assumes White always goes first
        (White, []) -> [Axial 0 0]
        -- but we do support any initial placement pos for white
        -- (Black, []) -> Grid.neighbors $ (trace "\n\nspawn calcs\n\n" head) $ findTopPieces (const True) board
        (Black, []) -> case findTopPieces (const True) board of
                                [] -> error $ "findTopPieces (const True) found nothing on black turn! board: " <> show board
                                (pos:_) -> Grid.neighbors pos
        -- spawns are empty hexes that are both bordering friendlies and not bordering foes
        (_, friendlies) -> [ emptyNabe | emptyNabe <- nub $ friendlies >>= unoccupiedNeighbors board
                                       , not $ hexTouchesTeam board (opposing team) emptyNabe
                                       ]

hexTouchesTeam :: Board -> Team -> AxialPoint -> Bool
hexTouchesTeam board team pos =
    any ((== team) . pieceTeam . unsafeTopPieceAt board) $
        occupiedNeighbors board pos

-- | removeDupePieces [wQ, wA1, wA2, wA3] == [wQ, wA1]
removeDupePieces :: [Piece] -> [Piece]
removeDupePieces = nubBy ((==) `on` pieceTeam &&& pieceSpecies)

spawnablePieces :: Game -> [Piece]
spawnablePieces game
    | turnNo == 1                    = filter (not . isQueenBee) unplacedFriendlies
    | turnNo == 4 && queenIsUnplaced = filter isQueenBee unplacedFriendlies
    | otherwise                      = unplacedFriendlies
  where
    turnNo = length (gameMoves game) `div` 2 + 1 -- this player's 1-based turn #
    unplacedFriendlies = removeDupePieces $ thisTeamUnplaced game
    queenIsUnplaced = isJust $ find isQueenBee unplacedFriendlies
    isFriendly = (== gameTurn game) . pieceTeam

-- TODO: move these somewhere they belong
isQueenBee = (== QueenBee) . pieceSpecies
thisTeamUnplaced game = filter ((== gameTurn game) . pieceTeam) $ gameUnplaced game

-- it would be nice if this gave feedback on exactly why the move is invalid
isValidMove :: Game -> AbsoluteMove -> Bool
isValidMove game Pass = True -- XXX passing is only valid if you have no possible moves
isValidMove game (Move piece pos) =
    gameTurn game == pieceTeam piece && (firstMove || boardMove || spawnMove)
  where
    firstMove = null $ gameMoves game
    boardMove = elem pos $ fromMaybe [] (Map.lookup piece $ gamePossibleMoves game)
    spawnMove = elem piece (gameUnplaced game) && elem pos (gameSpawnPositions game)

applyMoveToBoard :: AbsoluteMove -> Board -> Board
applyMoveToBoard Pass board = board
applyMoveToBoard (Move piece to) board =
    case findTopPieces (== piece) board of
        [] -> addPiece piece to board
        [from] -> movePieceTo board from to
        _ -> error "broken game! multiple instances of the same piece in the board."

applyMove :: AbsoluteMove -> Game -> Either String Game
applyMove Pass game = Right game
applyMove move@(Move piece pos) game
    | isValidMove game move = Right game'
    | otherwise             = Left $ "invalid move: " <> show move
                                        <> "\ngame for invalid move: " <> show game
                                        <> "\n"
  where
    Game { gameBoard = board
         , gameMoves = history
         , gameTurn = turn
         , gameUnplaced = unplaced
         } = game
    game' = game { gameBoard = board'
                 , gameMoves = history <> [Move piece pos]
                 --- XXX BUG TODO it is possible, though rare, for a particular player to have no free pieces (e.g. the other player dominates the outside of the hive)
                 -- we need a way to represent this situatino properly
                 -- i imagine the UI will have to show some kind of indication
                 -- that a player was force to pass, and then just move on to
                 -- the next turn for the other player.
                 , gamePossibleMoves = allBoardMovesForGame game'    -- tied the knot!
                 , gameSpawnablePieces = spawnablePieces game'  -- yay circularity
                 , gameSpawnPositions = spawnPositions (opposing turn) board'
                 , gameUnplaced = delete piece unplaced
                 , gameTurn = opposing turn
                 , gameState = detectGameState game'
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


detectGameState :: Game -> GameState
detectGameState game =
    case findSurroundedQueens (gameBoard game) of
        [_, _] -> Draw
        [q]    -> Won (pieceTeam q)
        _      -> Playing

findSurroundedQueens :: Board -> [Piece]
findSurroundedQueens board = [ queen | (pos,stack) <- findPieces isQueenBee board
                                     , null (unoccupiedNeighbors board pos)
                                     , let Just queen = find isQueenBee stack ]

--------------------------------------------------------------------------------
-- Debug helpers

findUnplaced board = allPieces List.\\ allPiecesOnBoard board


newGame = Game { gameBoard = emptyBoard
               , gameUnplaced = allPieces
               , gameMoves = []
               , gamePossibleMoves = allBoardMovesForGame newGame
               , gameSpawnPositions = [Axial 0 0]
               , gameSpawnablePieces = filter (not . isQueenBee) $ thisTeamUnplaced newGame
               , gameTurn = White
               , gameState = Playing
               }


--------------------------------------------------------------------------------
-- most of this is stuff that doesn't really belong here

traceM_ s = trace (s <> "\n") $ return ()

isValidRelativeMove game (RelativeFirst _) = gameBoard game == emptyBoard
isValidRelativeMove game rel@(RelativeMove mover target dir) = trace ("trying move: " ++ show rel ++ "\n") $
    elem target (allPiecesOnBoard board) && isValidMove game absMove
  where
    board = gameBoard game
    absMove = interpretMove board rel


allPossibleAbsoluteMoves :: Game -> [AbsoluteMove]
allPossibleAbsoluteMoves game = spawnMoves <> boardMoves
  where
    spawnMoves = [Move pc pos
                 | pc <- gameSpawnablePieces game
                 , pos <- gameSpawnPositions game
                 ]
    boardMoves = [Move pc pos
                 | (pc,poses) <- Map.toList $ gamePossibleMoves game
                 , pos <- poses
                 ]
    -- might just go to this as a base representation
    -- using a flat list of [AbsoluteMove] would eliminate the need for gameSpawnablePieces
    -- would also make representing special pillbug notation easier

-- given a Game, return a list of all intermediate gamestates from newGame to this Game
-- the resulting list is ordered from firs tmove to last, and starts AFTER the first move.
-- i figured always having newGame be in the list doesn't really convey any information.
-- passing in an an empty game will produce an empty list.
decomposeGame :: Game -> [Game]
decomposeGame origGame =
    -- this def was more fun to write
    -- map (fromRight . flip applyMovesToGame newGame) $ tail $ inits $ gameMoves origGame
    -- but this one reduces recomputation and will use sharing
    tail . reverse $ foldl doMove [newGame] (gameMoves origGame)
    where
        doMove games move = fromRight (applyMove move (head games)) : games

applyMovesToGame :: [AbsoluteMove] -> Game -> Either String Game
applyMovesToGame moves game = foldM (flip applyMove) game moves

fromRight (Right x) = x
fromRight err = error "fromRight (Left _)\n"

-- | Convert an AbsoluteMove into a RelativeMove
-- given a Game where that move can be played in the next turn.
relativizeMove :: Game -> AbsoluteMove -> Maybe RelativeMove
relativizeMove game Pass = Just RelativePass
relativizeMove game move@(Move piece pos)
    | null (gameMoves game) = Just $ RelativeFirst piece
    | otherwise             = do
        -- traceM_ $ "[rM] pos: " <> show pos
        guard $ move `elem` allPossibleAbsoluteMoves game
        let board = gameBoard game
        -- we can't  allow a beetle move to reference the piece underneath it
        -- when moving off of that piece
        let rmOrigin :: [AxialPoint] -> Maybe AxialPoint
            rmOrigin = case findTopPieces (== piece) board of
                            [origin] -> find (/= origin)
                            []       -> (trace $ "spawn move: " <> show move <> "\n")
                                        listToMaybe -- this must be a spawn move
                            origins  -> error $ "found multiple origins for Piece"
                                                <> show piece <> ": "
                                                <> show origins <> "\n"
        -- traceM_ $ "[rM] origin: " <> show origin
        -- let origin = Axial 999 999 -- TEMP for testing
        target <- rmOrigin $ occupiedNeighbors board pos
        -- traceM_ $ "[rM] target: " <> show target
        targetPiece <- topPieceAt board target
        -- traceM_ $ "[rM] targetPiece: " <> show targetPiece
        dir <- Grid.findDirectionFromAxialPoints target pos
        -- traceM_ $ "[rM] dir: " <> show dir
        pure $ RelativeMove piece targetPiece dir

transcript :: Game -> [Maybe RelativeMove]
transcript Game{gameMoves=[]}        = []
transcript Game{gameMoves=[m]}       = [relativizeMove newGame m]
transcript game                      =
    [ (trace $ "\ng = " <> show (gameMoves g) <> "\n" <> " nextMove = " <> show nextMove <> "\n")
      relativizeMove g nextMove | (g, g') <- zip games (tail games)
                                , let nextMove = last . gameMoves $ g' ]
  where
    games = newGame : decomposeGame game


transcript' :: Game -> [Maybe String]
transcript' = ((describeMove <$>) <$>) . transcript

gameFromTranscript :: [String] -> Either String Game
gameFromTranscript moves = applyTranscript moves newGame

applyTranscript :: [String] -> Game -> Either String Game
applyTranscript moves game = foldM doMove game relmoves
  where
    Right relmoves = mapM parseMove moves
    doMove g relmove = applyMove (interpretMove (gameBoard g) relmove) g

