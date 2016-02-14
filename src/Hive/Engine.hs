
module Hive.Engine where

import Control.Category ((>>>))
import Control.Monad (guard, foldM, when)
import Control.Exception

import Data.List (find, nub, delete, foldl', maximumBy, sort, inits)
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

-- TODO: move QC stuff out to test dirs and use tasty
import Test.QuickCheck

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
    emptyBorderAdjList = map (\pos -> (pos, pos, planarPassableNeighbors board' pos)) emptyBorderPositions

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
        (_, friendlies) ->
                           -- [ emptyNabe
                           -- | friendly <- friendlies
                           -- , emptyNabe <- unoccupiedNeighbors board friendly
                           -- , not $ any ((== opposing team) . pieceTeam . unsafeTopPieceAt board)
                           --       $ occupiedNeighbors board emptyNabe
                           -- ]
                           let friendlyEmpties  = Set.fromList $
                                                    findTopPieces ((== team). pieceTeam) board
                                                        >>= unoccupiedNeighbors board
                               foeEmpties       = Set.fromList $
                                                    findTopPieces ((== opposing team). pieceTeam) board
                                                        >>= unoccupiedNeighbors board

                            in Set.toList $ Set.difference friendlyEmpties foeEmpties

hexTouchesTeam :: Board -> Team -> AxialPoint -> Bool
hexTouchesTeam board team pos =
    any ((== team) . pieceTeam . unsafeTopPieceAt board)
        $ occupiedNeighbors board pos



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
fromRight err = error "fromRight (Left x)\n"


-- need a relative transcript thing now to debug this
-- TODO: don't have beetle moves reference the piece it's sitting on top of
relativizeMove :: Game -> AbsoluteMove -> Maybe RelativeMove
relativizeMove game Pass = Just RelativePass
relativizeMove game (Move piece pos)
    | 1 == length (gameMoves game) = Just $ RelativeFirst piece
    | otherwise                    = do
        let board = gameBoard game
        -- traceM_ $ "[rM] pos: " <> show pos
        target <- listToMaybe $ occupiedNeighbors board pos
        -- traceM_ $ "[rM] target: " <> show target
        targetPiece <- topPieceAt board target
        -- traceM_ $ "[rM] targetPiece: " <> show targetPiece
        dir <- Grid.findDirectionFromAxialPoints target pos
        -- traceM_ $ "[rM] dir: " <> show dir
        return $ RelativeMove piece targetPiece dir

transcript :: Game -> [Maybe RelativeMove]
transcript game = [ relativizeMove game (last . gameMoves $ game)
                    | game <- decomposeGame game ]

transcript' :: Game -> [Maybe String]
transcript' = ((describeMove <$>) <$>) . transcript



prop_piecesConserved g =
    sort allPieces == sort (allPiecesOnBoard (gameBoard g) <> gameUnplaced g)

prop_freePiecesAlwaysHaveMoves g =
    let board = gameBoard g
        freePoses = allFreePiecePositions board
     in all (not . null) $ map (movesForPieceAtPosition board) freePoses

prop_validMovesLeadToValidBoards Game{gameBoard=board} =
    board == emptyBoard || isValidBoard board

-- other ideas (not necessarily quickcheck)
-- all pieces in a ring are free
-- there are always free pieces on the board (after initial move)
-- take scenarios from hive book and enforce found moves
-- test for upper gated beetle scenario
-- and impact of that on pillbug

enforceForEntireReplay :: (Game -> Bool) -> Game -> Bool
enforceForEntireReplay pred game = all pred $ decomposeGame game

instance Arbitrary Game where
    -- wtf is going on?  it seems like trace from within Arbitrary doesn't work
    -- and throwing exceptions and calling fail do nothing
    arbitrary = do
        -- throw $ AssertionFailed "omg"
        -- fail "wtf"
        -- assert False $ return ()
        moveCount <- arbitrarySizedNatural
        traceM_ $ "generating an arbitrary game with " <> show moveCount <> " moves."
        doMoves moveCount newGame
      where
        doMoves 0 g = return g
        doMoves n g@Game{gameBoard=board} = do
            let moves = allPossibleAbsoluteMoves g
            if null moves then
                return $ error $ "there are no possible moves for game:\n" <> show g
                -- traceM_ $ "!!! there are no possible moves for game:\n" <> show g
                -- discard
              else do
                move <- elements moves
                case applyMove move g of
                    Left err -> do
        -- XXX worrisomely, i DO see discarded cases sometimes...
                        traceM_ $ "failed to apply move " <> show move
                                    <> " to game " <> show g <> "\n"
                        discard
                    Right g' -> doMoves (n-1) g'
    -- this is trying to do some structural stuff that requires Arbitrary on
    -- everything in a Game, which won't work out for my approach of choosing
    -- among precalculated valid moves
    -- shrink = genericShrink
    -- i think to do shrink right, we'd have to just chop off some moves from
    -- the history and recalculate the games up to that truncation
    -- tried this but it didn't work out; QC just kept calculating shrinks
    -- shrink g = safeChop $ decomposeGame g

-- safeChop [] = []
-- safeChop (_:xs) = xs


-- repl testing; grab the largest game from a sample batch
sampleBigGame :: IO Game
sampleBigGame = maximumBy (compare `on` length . gameMoves) <$> sample' arbitrary
