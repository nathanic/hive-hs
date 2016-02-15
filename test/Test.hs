import qualified Test.Tasty as Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Data.List (find, sort, maximumBy, (\\))
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set

import Hive.Board
import Hive.Engine
import Hive.HexGrid (AxialPoint(..))
import qualified Hive.HexGrid as Grid
import Hive.Piece

-- handy commands to remember:
-- $ stack test --file-watch
-- $ stack repl :hive-test-suite

main :: IO ()
main = do
    behaviors <- testSpec "Piece Movement Behaviors" pieceMovementSpec
    Tasty.defaultMain $ Tasty.testGroup "All Tests" [behaviors, qcProperties]

-- figure 1.6 from PHLAC (Play Hive Like A Champion by Randy Ingersoll)
-- demonstrating a simple gate barring direct access to position "A" at (1,1)
boardWithGate = makeBoard [(0, 0, wA1)
                          ,(1, 0, wS1)
                          ,(0, 1, wQ)
                          ,(0, 2, wG1)
                          ,(1, 2, bG1)
                          ,(2, 2, bQ)
                          ,(2, 1, bA1)
                          ]
gatedHex = Axial 1 1

-- figure 1.9 from PHLAC
-- demonstrating how the black queen can't move NW to an adjacent hex
-- because it would require briefly losing contact with the hive.
boardConstantContact = makeBoard [(0,0,wA1)
                                 ,(1,0,wS1)
                                 ,(0,1,wQ)
                                 ,(0,2,wG1)
                                 ,(1,2,bG1)
                                 ,(2,2,bA1)
                                 ,(2,1,bQ)
                                 ]
boardWithDoor = undefined

-- figure 2.18 from PHLAC
-- upper gate blocks wP from moving wQ to (-1,2)
boardWithUpperGate = makeBoard [ (0,0,bA2)
                               , (0,1,wA1)
                               , (0,1,bB1)
                               , (1,1,bQ)
                               , (4,1,bA1)
                               , (1,2,wP)
                               , (2,2,wG1)
                               , (3,2,bS1)
                               , (4,2,bG1)
                               , (-1,3,wS1)
                               , (-1,3,wB1)
                               , (2,3,bQ)
                               ]


-- place the piece at the center of boardWithGate and see if it can get out
isStuckWhenGatedIn :: Piece -> Bool
isStuckWhenGatedIn pc = null $ movesForPieceAtPosition board gatedHex
  where board = addPiece pc gatedHex boardWithGate

-- place the piece at the center of boardWithGate, wall in the gate, and see if it can get out
isStuckWhenSurrounded :: Piece -> Bool
isStuckWhenSurrounded pc = null $ movesForPieceAtPosition board gatedHex
  where board = addPiece pc gatedHex $ addPiece bB1 (Axial 2 0) boardWithGate


pieceMovementSpec :: Spec
pieceMovementSpec = parallel $ do
    describe "Ant" $ do
        it "moves all around the outside of the hive but not into gates" $
            antMoves boardWithGate (Axial 0 0)
                `shouldMatchList` [ Axial 1 (-1) -- \wS1
                                  , Axial 2 (-1) -- wS1/
                                  , Axial 2 0    -- wS1-
                                  , Axial 3 0    -- bA1/
                                  , Axial 3 1    -- bA1-
                                  , Axial 3 2    -- bQ-
                                  , Axial 2 3    -- bQ\
                                  , Axial 1 3    -- /bQ
                                  , Axial 0 3    -- /bG1
                                  , Axial (-1) 3 -- /wG1
                                  , Axial (-1) 2 -- -wG1
                                  , Axial (-1) 1 -- -wQ
                                  ]

        it "passes through doors"
            pending
        it "cannot move inside enclosed cavities"
            pending
        it "is stuck if surrounded" $
            bA3 `shouldSatisfy` isStuckWhenSurrounded
        it "is stuck if gated in" $
            bA3 `shouldSatisfy` isStuckWhenGatedIn
    describe "Beetle" $ do
        it "can move atop the hive" $ do
            -- surround the beetle on all sides
            let board = addPieces [(2,0,wA2), (1,1,bB2)] boardWithGate
            beetleMoves board gatedHex `shouldMatchList` Grid.neighbors gatedHex
        it "cannot pass through higher level gates"
            pending
        it "cannot slide through regular gates either" $ do
            let board = addPieces [(2,0,bB2)] boardWithGate
            beetleMoves board (Axial 2 0)
                `shouldMatchList` [ Axial 1 0    -- atop wS1
                                  , Axial 2 (-1) -- wS1/
                                  , Axial 2 1    -- atop bA1
                                  , Axial 3 0    -- bA1/
                                  ]
        it "can drop down into gated areas from above" $ do
            -- stick a beetle on top of the spider, it should be able to reach into gatedHex
            let board = addPiece bB2 (Axial 1 0) boardWithGate
            beetleMoves board (Axial 1 0) `shouldMatchList` Grid.neighbors (Axial 1 0) -- neighbors include gatedHex
        it "is NOT stuck if surrounded" $
            bB2 `shouldSatisfy` isStuckWhenSurrounded
        it "is NOT stuck if gated in" $
            wB2 `shouldNotSatisfy` isStuckWhenGatedIn
    describe "Grasshopper" $ do
        it "moves in straight lines only"
            pending
        it "is NOT stuck if surrounded" $
            bG3 `shouldNotSatisfy` isStuckWhenSurrounded
        it "is NOT stuck if gated in" $
            bG3 `shouldNotSatisfy` isStuckWhenGatedIn
    describe "Ladybug" $ do
        it "goes 2 hexes on top and then must drop down"
            pending
        it "is NOT stuck if surrounded" $
            bL `shouldNotSatisfy` isStuckWhenSurrounded
        it "is NOT stuck if gated in" $
            wL `shouldNotSatisfy` isStuckWhenGatedIn
    describe "Mosquito" $ do
        it "can jump like a grasshopper or move like a ladybug"
            pending
        -- it "can dance like a butterfly or sting like a bee" pending
        it "has no moves when its only neighbor is another mosquito" $ do
            let board = addPieces [(2,2,wM), (3,2,bM)] boardWithGate
            mosquitoMoves board (Axial 3 2) `shouldBe` []
        it "must remain in beetle mode if it starts the turn atop the hive" $ do
            let board = addPiece wM (Axial 1 0) boardWithGate
            mosquitoMoves board (Axial 1 0) `shouldMatchList` Grid.neighbors (Axial 1 0)
    describe "Pillbug" $ do
        let Right game = gameFromTranscript [ "wS1"
                                            , "bS1 wS1\\"
                                            , "wQ \\wS1"
                                            , "bQ bS1\\"
                                            , "wP -wS1"
                                            , "bG1 bS1-"
                                            , "wA1 -wQ"
                                            , "bA1 bQ-"
                                            , "wS2 /wP"
                                            , "bA1 wA1/"
                                            , "wB1 -wS2"
                                            , "bB1 \\bA1"
                                            , "wB1 /wP"
                                            , "bB1 \\wA1"
                                            , "wG1 wQ-"
                                            , "bB1 -wQ"
                                            ]
            board = gameBoard game
            movesByPiece = allBoardMovesForGame game
        -- full pillbug processing requires a Game, not merely a Board
        -- we're going to have to build a desired game state from a set of actual legal moves
        it "can't move pieces past an upper level gate" $
            Map.lookup wQ movesByPiece `shouldBe` Just [Axial (-1) 1]
        it "can't move a piece that moved last turn"
            pending
        it "can't move a piece that is under another piece"
            pending
        it "moves like a queen"
            pending
        it "can move enemy pieces around"
            -- let pBNE = Axial 0 (-1)         -- swap out wQ for bS2
                -- board' = addPiece bS2 pBNE $ removeTopPieceAt pBNE board
            -- Map.lookup wQ movesByPiece `shouldBe` Just [Axial (-1) 1]
            pending
        it "can only move pieces that are at ground level" $
            fromJust (Map.lookup bB1 movesByPiece) `shouldNotContain` [Axial (-1) 1]
        it "is stuck if surrounded" $
            bP `shouldSatisfy` isStuckWhenSurrounded
        it "is stuck if gated in" $
            --- beware, this is just using pillbugMoves, and not pillbugProcessing
            bP `shouldSatisfy` isStuckWhenGatedIn
    describe "QueenBee" $ do
        it "remains in constant contact with the hive" $
            -- notably, the results do NOT include (2,0) which is one hex away but unreachable
            queenBeeMoves boardConstantContact (Axial 2 1)
                `shouldMatchList` [Axial 1 1, Axial 3 1]
        it "is stuck if surrounded" $
            bQ `shouldSatisfy` isStuckWhenSurrounded
        it "is stuck if gated in" $
            -- XXX technically this makes an unsound board since there is already a bQ placed
            -- but due to how the move calculations are implemented it *shouldn't* matter...
            bQ `shouldSatisfy` isStuckWhenGatedIn
    describe "Spider" $ do
        it "normally has only 2 moves"
            pending
        it "gets extra moves at a door"
            pending
        it "cannot pass through gates" $ do
            -- bS1 bA1-
            let board = addPiece bS1 (Axial 2 2) boardWithGate
            spiderMoves board (Axial 2 2) `shouldNotContain` [gatedHex]
        it "is stuck if surrounded" $
            wS2 `shouldSatisfy` isStuckWhenSurrounded
        it "is stuck if gated in" $
            wS2 `shouldSatisfy` isStuckWhenGatedIn

-- other things to check:
    -- all pieces in a ring are free?
    -- pieces covered by a beetle/mosquito are not free
    -- handling of situations where a player must pass
    -- game is won when a queen is surrounded
    -- game is a draw when both queens are surrounded

-- other specs: quickcheck tests for oneHiveRule etc.
-- maybe try smallcheck?


qcProperties = Tasty.testGroup "QuickCheck properties"
    [ testProperty "the total set of Pieces is constant throughout a Game" $
        \ g -> sort allPieces == sort (allPiecesOnBoard (gameBoard g) <> gameUnplaced g)
    , testProperty "valid moves lead to valid boards" $
        \ Game{gameBoard=board} -> board == emptyBoard || isValidBoard board
    ]


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



-- some helpers for building test Boards

addPieces :: [(Int,Int,Piece)] -> Board -> Board
addPieces = flip $ foldl (\b (p, q, pc) -> addPiece pc (Axial p q) b)

makeBoard :: [(Int,Int,Piece)] -> Board
makeBoard spec = addPieces spec emptyBoard
