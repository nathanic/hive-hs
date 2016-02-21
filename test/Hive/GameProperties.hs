module Hive.GameProperties
    -- ( gameProperties )
  where

import qualified Test.Tasty as Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.MVar
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, replicateM)

import Data.List (find, sort, maximumBy, minimumBy, (\\))
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set

import Text.ParserCombinators.Parsec
import Text.Parsec.Error (Message(..),newErrorMessage)
import Text.Parsec.Pos (initialPos)

import Debug.Trace (trace)

import Hive.Board
import Hive.HexGrid (AxialPoint(..))
import qualified Hive.HexGrid as Grid
import Hive.Engine
import Hive.Move
import Hive.Piece

import Hive.TestUtil


-- other things to check:
    -- all pieces in a ring are free?
    -- pieces covered by a beetle/mosquito are not free
    -- handling of situations where a player must pass
    -- game is won when a queen is surrounded
    -- game is a draw when both queens are surrounded


gameProperties = Tasty.testGroup "QuickCheck properties"
    [ testProperty "the total set of Pieces is constant throughout a Game" $
        \ g -> sort allPieces == sort (allPiecesOnBoard (gameBoard g) <> gameUnplaced g)
    , testProperty "valid moves lead to valid boards" $
        \ Game{gameBoard=board} -> board == emptyBoard || isValidBoard board
    , testProperty "relativized moves never reference the same piece twice, nor the piece underneath the mover" $
        adaptPropForTranscription prop_movesDoNotSelfReference
    -- TODO: only stackable pieces are ever stacked?
    ]


prop_movesDoNotSelfReference origGame = all isLegitMove (decomposeGameWithTranscript origGame)
  where
    isLegitMove (_,Nothing)                                      =
        error "failed to transcribe move from Arbitrary Game"
    isLegitMove (Game{..}, Just (RelativeMove mover target dir)) =
        mover /= target
    isLegitMove _                                                =
        True -- whatevs

-- maybe another one that checks that we do not reference within our own stack
-- if we have any other choice...

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
            -- okay, so this one can definitely happen and should not really be an error
            -- it means all the free pieces are controlled by the opposing team, so
            -- this player must pass
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
    -- shrink by returning previous instances of the game with the final move(s) removed
    shrink g = let games = decomposeGame g
                -- in safeButLast $ safeDrop (3 * length games `div` 4) games
                in safeTail . safeButLast $ games


--------------------------------------------------------------------------------
-- | Automatic Transcription Support for Game Counterexamples
--------------------------------------------------------------------------------

data Transcribed = Transcribed Game String
    deriving (Eq)

instance Show Transcribed where
  show (Transcribed g ts) = "Transcribed " <> show g <> "\n" <> ts

-- | Wrap a Game with a human-readable transcript
transcribedGame :: Game -> Transcribed
transcribedGame g = Transcribed g (niceTranscript g)

niceTranscript = unlines . map (fromMaybe "Nothing") . transcript'

-- | Wrap a Game predicate to support automatic transcription
adaptPropForTranscription :: (Game -> Bool) -> (Transcribed -> Bool)
adaptPropForTranscription p = \(Transcribed g _) -> p g

instance Arbitrary Transcribed where
    arbitrary = transcribedGame <$> arbitrary
    shrink (Transcribed g _) = transcribedGame <$> shrink g


-- repl testing; grab the largest game from a sample batch
sampleBigGame :: IO Game
sampleBigGame = maximumBy (compare `on` length . gameMoves) <$> sample' arbitrary

-- repl testing a counterexample Game
-- jank = fromRight . gameFromTranscript $
--     [ "wM"
--     , "bM -wM"
--     , "wG1 wM\\"
--     , "bQ -bM"
--     , "wB1 wG1-"
--     , "bL /bQ"
--     , "wQ wB1-"
--     , "bS1 -bQ"
--     , "wA1 /wB1"
--     , "bB1 \bS1"
--     , "wB2 /wG1"
--     , "bB1 -bQ"
--     ]

-- jank = fromRight $ applyMovesToGame [Move wS1 (Axial 0 0),Move bB1 (Axial 0 (-1)),Move wP (Axial 0 1),Move bB2 (Axial 0 (-2)),Move wS2 (Axial (-1) 2),Move bS1 (Axial (-1) (-1)),Move wQ (Axial 1 0),Move bQ (Axial (-2) (-1)),Move wA1 (Axial (-1) 1),Move bB2 (Axial (-1) (-1)),Move wG1 (Axial (-2) 1),Move bB2 (Axial (-2) (-1))] newGame
{- λ∫ putStrLn $ niceTranscript jank
    wS1
    bB1 \wS1
    wP wS1\
    bB2 \bB1
    wS2 /wP
    bS1 /bB2
    wQ wP/
    bQ -bS1
    wA1 /wS1
    bB2 -bB1
    wG1 -wA1
    bB2 -bB2  -- wtf is this? can't reference self!
-}

preJank = fromRight $ applyMovesToGame [Move wL (Axial 0 0),Move bM (Axial 0 1),Move wQ (Axial (-1) 0),Move bB1 (Axial (-1) 2),Move wP (Axial (-2) 0),Move bQ (Axial 0 2),Move wG1 (Axial 1 (-1)),Move bB1 (Axial 0 1),Move wA1 (Axial 2 (-2))] newGame
jank = fromRight $ applyMovesToGame [Move wL (Axial 0 0),Move bM (Axial 0 1),Move wQ (Axial (-1) 0),Move bB1 (Axial (-1) 2),Move wP (Axial (-2) 0),Move bQ (Axial 0 2),Move wG1 (Axial 1 (-1)),Move bB1 (Axial 0 1),Move wA1 (Axial 2 (-2)),Move bB1 (Axial 0 2)] newGame
{- λ∫ putStrLn $ niceTranscript jank
    wL
    bM wL\
    wQ -wL
    bB1 /bM
    wP -wQ
    bQ bB1-
    wG1 wL/
    bB1 \bQ
    wA1 wG1/
    Nothing -- then fixed self reference, but now the move just can't be relativized
-}
jankMove = last $ gameMoves jank
jankRel = relativizeMove preJank jankMove

-- so this is really, really sick...
-- but we're going to parse games out of quickcheck output
-- because it will save time debugging and because parsec is fun.
-- niceTranscript <$> findShortestCounterExample
findShortestCounterExample n p = minimumBy (compare `on` length . gameMoves) <$> findCounterExamples n p
findCounterExamples n p = replicateM n (fromRight <$> findCounterExampleGame p)

-- so i tried this, but it didn't work out because ghci doesn't really seem to allow threading
-- it's a pity, parallel replicateM sounds nice
scatterGather :: Int -> (IO a) -> IO [a]
scatterGather n action = do
    mvars <- replicateM n newEmptyMVar
    forM_ mvars $ \var -> do
        forkOS $ action >>= putMVar var
    forM mvars $ \var -> takeMVar var

findCounterExampleGame :: (Testable prop) => prop -> IO (Either ParseError Game)
findCounterExampleGame p = scrapeGameFromResult <$> quickCheckWithResult stdArgs{maxSuccess=10000} p

scrapeGameFromResult :: Result -> Either ParseError Game
scrapeGameFromResult Failure{..} = parse p_game "<QC>" output
scrapeGameFromResult _ = Left $ newErrorMessage (Message "QC didn't return Failure, nothing to scrape!!") (initialPos "<QC>")

p_game = do
    garbageTill $ string "gameMoves = "
    moves <- bracketed p_moves
    pure . fromRight $ applyMovesToGame moves newGame
garbageTill p = manyTill anyChar $ try p
bracketed = surrounded (char '[') (char ']')
surrounded bef aft it = bef *> it <* aft
p_moves = p_move `sepBy` (char ',' >> many space)
p_move = do
    string "Move "
    pc <- p_piece
    string " ("
    dest <- p_axialPoint
    string ")"
    pure $ Move pc dest
p_piece = do
    name <- choice (try . string . pieceName <$> allPieces)
    pure . fromJust $ find (\p -> pieceName p == name) allPieces
p_axialPoint = do
    string "Axial "
    p <- p_num
    char ' '
    q <- p_num
    pure $ Axial p q
p_num = read <$> (try p_neg <|> p_pos)
p_neg = do
    string "(-"
    digs <- p_pos
    string ")"
    pure $ '-' : digs
p_pos = many1 digit


