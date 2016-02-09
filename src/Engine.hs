module Engine where

import Control.Category ((>>>))
import Control.Monad (guard, foldM)

import Data.List (find, nub, delete, foldl', maximumBy, sort)
import qualified Data.List as List
import Data.Function (on)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set

import HexGrid (AxialPoint(..))
import qualified HexGrid as Grid
import Piece
import Board
import Move

-- TODO: move test stuff out to test dirs
import Test.QuickCheck
import Debug.Trace

-- Start with the types!

data Game = Game { gameBoard :: Board
                 , gameUnplaced :: [Piece] -- Set?
                 , gameMoves :: [AbsoluteMove] -- move history
                 -- maybe the name should reflect that this is only board moves (not spawns)
                 , gamePossibleMoves :: Map Piece [AxialPoint]
                 , gameSpawnablePieces :: [Piece]
                 , gameSpawnPositions :: [AxialPoint]
                 , gameTurn :: Team
                 , gameWinner :: Maybe Team
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
    board' = board `removeTopPieceAt` from
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
allMovesForGame game
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
        (_, friendlies) -> [ emptyNabe
                           | friendly <- friendlies
                           , emptyNabe <- unoccupiedNeighbors board friendly
                           , not $ any ((== opposing team) . pieceTeam . unsafeTopPieceAt board)
                                 $ occupiedNeighbors board emptyNabe
                           ]


spawnablePieces :: Game -> [Piece]
spawnablePieces game
    | turnNo == 1                    = filter (not . isQueenBee) unplacedFriendlies
    | turnNo == 4 && queenIsUnplaced = filter isQueenBee unplacedFriendlies
    | otherwise                      = unplacedFriendlies
  where
    turnNo = length (gameMoves game) `div` 2 + 1 -- this player's 1-based turn #
    unplacedFriendlies = thisTeamUnplaced game
    queenIsUnplaced = isJust $ find isQueenBee unplacedFriendlies
    isFriendly = (== gameTurn game) . pieceTeam

-- TODO: move these somewhere they belong
isQueenBee = (== QueenBee) . pieceSpecies
thisTeamUnplaced game = filter ((== gameTurn game) . pieceTeam) $ gameUnplaced game

-- it would be nice if this gave feedback on exactly why the move is invalid
isValidMove :: Game -> AbsoluteMove -> Bool
isValidMove game (AbsoluteMove piece pos) =
    gameTurn game == pieceTeam piece && (firstMove || boardMove || spawnMove)
  where
    firstMove = null $ gameMoves game
    boardMove = elem pos $ fromMaybe [] (Map.lookup piece $ gamePossibleMoves game)
    spawnMove = elem piece (gameUnplaced game) && elem pos (gameSpawnPositions game)

applyMoveToBoard :: AbsoluteMove -> Board -> Board
applyMoveToBoard (AbsoluteMove piece to) board =
    case findTopPieces (== piece) board of
        [] -> addPiece piece to board
        [from] -> movePieceTo board from to
        _ -> error "broken game! multiple instances of the same piece in the board."

applyMove :: AbsoluteMove -> Game -> Either String Game
applyMove move@(AbsoluteMove piece pos) game
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
                 , gameMoves = history <> [AbsoluteMove piece pos]
                 , gamePossibleMoves = allMovesForGame game'    -- tied the knot!
                 , gameSpawnablePieces = spawnablePieces game'  -- yay circularity
                 , gameSpawnPositions = spawnPositions (opposing turn) board'
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


newGame = Game { gameBoard = emptyBoard
               , gameUnplaced = allPieces
               , gameMoves = []
               , gamePossibleMoves = allMovesForGame newGame
               , gameSpawnPositions = [Axial 0 0]
               , gameSpawnablePieces = filter (not . isQueenBee) $ thisTeamUnplaced newGame
               , gameTurn = White
               , gameWinner = Nothing
               }


--------------------------------------------------------------------------------
-- QuickCheck stuff that doesn't really belong here
-- but i'm rusty on qc so i'm just trying it out here for now
-- eventually planning on Tasty, maybe with hspec and qc

traceM_ s = trace s $ return ()

isValidRelativeMove game (RelativeFirst _) = gameBoard game == emptyBoard
isValidRelativeMove game rel@(RelativeMove mover target dir) = trace ("trying move: " ++ show rel ++ "\n") $
    elem target (allPiecesOnBoard board) && isValidMove game absMove
  where
    board = gameBoard game
    absMove = interpretMove board rel

instance Arbitrary Game where
    arbitrary = do
        moveCount <- arbitrarySizedNatural
        doMoves moveCount newGame
      where
        doMoves 0 g = return g
        doMoves n g@Game{gameBoard=board} = do
            move <- elements $ allPossibleAbsoluteMoves g
            case applyMove move g of
                Left err -> do
                    traceM_ $ "failed to apply move " <> show move
                                <> " to game " <> show g <> "\n"
                    discard
                Right g' -> doMoves (n-1) g'

allPossibleAbsoluteMoves :: Game -> [AbsoluteMove]
allPossibleAbsoluteMoves game = spawnMoves <> boardMoves
  where
    spawnMoves = [AbsoluteMove pc pos
                 | pc <- gameSpawnablePieces game
                 , pos <- gameSpawnPositions game
                 ]
    boardMoves = [AbsoluteMove pc pos
                 | (pc,poses) <- Map.toList $ gamePossibleMoves game
                 , pos <- poses
                 ]
    -- might just go to this as a base representation
    -- using a flat list of [AbsoluteMove] would eliminate the need for gameSpawnablePieces
    -- would also make representing special pillbug notation easier

prop_piecesConserved = property $ \g ->
    sort allPieces == sort (allPiecesOnBoard (gameBoard g) <> gameUnplaced g)

prop_freePiecesAlwaysHaveMoves = property $ \g ->
    let board = gameBoard g
        freePoses = allFreePiecePositions board
     in all (not . null) $ map (movesForPieceAtPosition board) freePoses

prop_validMovesLeadToValidBoards = property $ \Game {gameBoard=board} ->
    board == emptyBoard || isValidBoard board

-- TODO: fn to walk through a Game and produce a human-readable transcript

-- ooh, it should be possible, givem a Game, to replay it and enforce a Property
-- on the Game at each step

enforceForEntireReplay :: Game -> (Game -> Bool) -> Bool
enforceForEntireReplay game pred = all pred $ decomposeGame game

decomposeGame :: Game -> [Game]
decomposeGame origGame = 
    -- map (fromRight . flip applyMovesToGame) $ reverse $ tails $ (gameMoves game)
    foldl' doMove [] (gameMoves origGame) 
    where
        doMove []    move = [fromRight $ applyMove move newGame]
        doMove games move = fromRight (applyMove move (last games)) : games

applyMovesToGame :: [AbsoluteMove] -> Game -> Either String Game
applyMovesToGame moves game = foldM (flip applyMove) game moves

fromRight (Right x) = x
fromRight err = error "fromRight (Left x)\n"

-- repl testing; grab the largest game from a sample batch
sampleBigGame :: IO Game
sampleBigGame = maximumBy (compare `on` length . gameMoves) <$> sample' arbitrary

