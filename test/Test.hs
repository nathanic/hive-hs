import qualified Test.Tasty
import Test.Tasty.Hspec

import Data.List (find, (\\))
import Data.Maybe (isNothing)

import Hive.Board
import Hive.Engine
import Hive.HexGrid (AxialPoint(..))
import qualified Hive.HexGrid as Grid
import Hive.Piece

main :: IO ()
main = do
    test <- testSpec "Piece Movement" pieceMovementSpec
    Test.Tasty.defaultMain test

makeBoard = foldl (\b (p, q, pc) -> addPiece pc (Axial p q) b) emptyBoard

-- fig 1.6 from PHLAC (Play Hive Like A Champion by Randy Ingersoll)
-- demonstrating a simple gate barring direct access to position "A" at (1,1)
boardWithGate = makeBoard [(0, 0, wA1)
                          ,(1, 0, wS1)
                          ,(0, 1, wQ)
                          ,(0, 2, wG1)
                          ,(1, 2, bG1)
                          ,(2, 2, bQ)
                          ,(2, 1, bA1)
                          ]

boardWithDoor = undefined
boardWithUpperGate = undefined

pieceMovementSpec :: Spec
pieceMovementSpec = parallel $ do
    describe "Ant" $ do
        it "moves all around the outside of the hive" 
            pending
        it "passes through doors"
            pending
        it "cannot move inside enclosed cavities" 
            pending
        it "cannot pass through gates" $ do
            let posA = Axial 1 1
            antMoves boardWithGate (Axial 0 0) `shouldSatisfy` not . elem posA
    describe "Beetle" $ do
        it "can move atop the hive"
            pending
        it "cannot pass through higher level gates"
            pending
        it "cannot pass through regular gates either"
            pending
    describe "Ladybug" $ do
        it "goes 2 hexes on top and then must drop down"
            pending
    describe "Mosquito" $ do
        it "ain't got no moves when its only neighbor is another mosquito" $ do
            let board = addPiece wM (Axial 2 2) 
                        $ addPiece bM (Axial 3 2) 
                        $ boardWithGate
            mosquitoMoves board (Axial 3 2) `shouldBe` []
        it "must remain in beetle mode if it starts the turn atop the hive" $ do
            let board = addPiece wM (Axial 1 0) boardWithGate
            mosquitoMoves board (Axial 1 0) `shouldBe` beetleMoves board (Axial 1 0)
    describe "Spider" $ do
        it "normally has only 2 moves"
            pending
        it "gets extra moves at a door"
            pending
        it "cannot pass through gates" $ do
            -- add a spider just east of bA1
            let board = addPiece bS1 (Axial 2 2) boardWithGate
            let posA = Axial 1 1
            spiderMoves board (Axial 2 2) `shouldSatisfy` not . elem posA

-- other things to check:
    -- all pieces in a ring are free?
    -- pieces covered by a beetle/mosquito are not free
    -- handling of situations where a player must pass
    -- game is won when a queen is surrounded
    -- game is a draw when both queens are surrounded

-- other specs: quickcheck tests for oneHiveRule etc.
-- maybe try smallcheck?


