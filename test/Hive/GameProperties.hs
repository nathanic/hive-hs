module Hive.GameProperties
    -- ( gameProperties )
  where

import qualified Test.Tasty as Tasty
import Test.Tasty.QuickCheck

import Data.List (find, sort, maximumBy, (\\))
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace (trace)

import Hive.Board
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


prop_movesDoNotSelfReference game@Game{..} = all isLegitMove $ transcript game
  where
    isLegitMove Nothing = error "failed to transcribe move from Arbitrary Game"
    isLegitMove (Just (RelativeMove mover target _)) =
        -- don't let the target be any of the pieces at the mover's position
        -- if this is a spawn move, findPieces will give an empty list, and
        -- we'll default to (and []) == True
        and [ if target /= pc
                then True
                else trace ("target " <> show target <> " is in origin stack of mover " <> show mover <> "\n") False
                | (pos, stack) <- findPieces (== mover) gameBoard
                           , pc <- stack ]
    isLegitMove _ = True -- whatevs


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


-- it'd be nice to see a game transcript in the QC output
data Transcribed = Transcribed Game (Maybe String)
    deriving (Eq)

instance Show Transcribed where
  show (Transcribed g Nothing) = "Transcribed " <> show g <> " Nothing"
  show (Transcribed g (Just ts)) = "Transcribed " <> show g <> "\n" <> ts

transcribedGame g = Transcribed g (niceTranscript g)
  where
    niceTranscript = (unlines <$>) . sequence . transcript'

adaptPropForTranscription :: (Game -> Bool) -> (Transcribed -> Bool)
adaptPropForTranscription p = \(Transcribed g _) -> p g

instance Arbitrary Transcribed where
    arbitrary = do
        g <- arbitrary
        pure $ transcribedGame g
    shrink (Transcribed g _) = map transcribedGame $ shrink g


-- repl testing; grab the largest game from a sample batch
sampleBigGame :: IO Game
sampleBigGame = maximumBy (compare `on` length . gameMoves) <$> sample' arbitrary


-- repl testing
jank = fromRight . gameFromTranscript $
    [ "wM"
    , "bM -wM"
    , "wG1 wM\\"
    , "bQ -bM"
    , "wB1 wG1-"
    , "bL /bQ"
    , "wQ wB1-"
    , "bS1 -bQ"
    , "wA1 /wB1"
    , "bB1 \bS1"
    , "wB2 /wG1"
    , "bB1 -bQ"
    ]

