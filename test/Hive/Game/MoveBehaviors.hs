module Hive.Game.MoveBehaviors
    -- (pieceMovementSpec)
  where

import qualified Test.Tasty as Tasty
import Test.Tasty.Hspec

import Data.List (find, sort, maximumBy, (\\))
import Data.Function (on)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as Set

import Hive.Game.Board
import Hive.Game.HexGrid (AxialPoint(..))
import qualified Hive.Game.HexGrid as Grid
import Hive.Game.Engine
import Hive.Game.Move
import Hive.Game.Piece

import Hive.Game.TestUtil

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

-- figure 4.2.1 from PHLAC
-- bS2 gets extra moves due to a door
boardWithDoor = makeBoard [(0,0,wA2)
                          ,(1,0,bQ)
                          ,(2,0,bS1)
                          ,(3,0,bQ)
                          ,(-1,1,bG2)
                          ,(3,1,bA1)
                          ,(-2,2,wG1)
                          ,(1,2,bS2) -- the interesting piece!
                          ,(3,2,wA1)
                          ,(-2,3,bB1)
                          ,(0,3,bA2)
                          ,(2,3,bG1)
                          ,(0,4,bG2)
                          ,(1,4,bS1)
                          ]

-- figure 4.3.1 from PHLAC: ring makes everything but wQ free
boardWithRing = makeBoard [(3,-1,bB1)
                          ,(0,0,bQ)
                          ,(1,0,wB1)
                          ,(2,0,wQ)
                          ,(-1,1,wG1)
                          ,(2,1,bA1)
                          ,(-2,2,wA1)
                          ,(-1,2,bS2)
                          ,(1,2,wA2)
                          ,(-1,3,bG1)
                          ,(1,3,bA2)
                          ,(-1,4,wG2)
                          ,(0,4,bS1)
                          ]

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



-- an example game i came up with for testing pillbugs
pillbugGame = fromRight $ gameFromTranscript [ "wS1"
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

-- testing a corner case of relativizing moves for beetles
beetleGame = fromRight $ gameFromTranscript [ "wS1"
                                            , "bS1 wS1\\"
                                            , "wQ \\wS1"
                                            , "bQ bS1\\"
                                            , "wB1 -wS1"
                                            , "bG1 bS1-"
                                            , "wB1 \\wS1"
                                            , "bG1 -bS1"
                                            -- , "wB1 -wS1"
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
            bB2 `shouldNotSatisfy` isStuckWhenSurrounded
        it "is NOT stuck if gated in" $
            wB2 `shouldNotSatisfy` isStuckWhenGatedIn
        it "does not reference itself when a move is relativized" $
            relativizeMove beetleGame (Move wB1 (Axial (-1) 0))
                `shouldNotBe` Just (RelativeMove wB1 wB1 Grid.SW)
    describe "Grasshopper" $ do
        it "moves in straight lines only"
            pending
        it "makes exactly one hop (cannot traverse past a gap in pieces along a line)"
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
        it "can dance like a butterfly or sting like a bee" $
            1 `shouldBe` 1
        it "has no moves when its only neighbor is another mosquito" $ do
            let board = addPieces [(2,2,wM), (3,2,bM)] boardWithGate
            mosquitoMoves board (Axial 3 2) `shouldBe` []
        it "must remain in beetle mode if it starts the turn atop the hive" $ do
            let board = addPiece wM (Axial 1 0) boardWithGate
            mosquitoMoves board (Axial 1 0) `shouldMatchList` Grid.neighbors (Axial 1 0)
    describe "Pillbug" $ do
        -- full pillbug processing requires a Game, not merely a Board
        -- we're going to have to build a desired game state from a set of actual legal moves
        it "can't move pieces past an upper level gate" $
            Map.lookup wQ (gamePossibleMoves pillbugGame) `shouldBe` Just [Axial (-1) 1]
        it "can't move a piece that moved last turn"
            pending
        it "can't move a piece that is under another piece"
            pending
        it "moves like a queen"
            pending
        it "can move enemy pieces around" $ do
            let Right game' = applyTranscript [ "wB1 /wS2", "bG1 wP\\" ] pillbugGame
            -- pillbug can move bG1 to -wP
            fromJust (Map.lookup bG1 $ gamePossibleMoves game') `shouldContain` [Axial (-2) 0]
            pending
        it "can only move pieces that are at ground level" $
            fromJust (Map.lookup bB1 $ gamePossibleMoves pillbugGame) `shouldNotContain` [Axial (-1) 1]
        it "is stuck if surrounded" $
            bP `shouldSatisfy` isStuckWhenSurrounded
        it "is stuck if gated in" $
            -- N.B. this is just using pillbugMoves, and not pillbugProcessing.
            -- however, pillbugProcessing doesn't ever move the pillbug itself.
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
            -- unclean though, and relies on too much engine knowledge
            bQ `shouldSatisfy` isStuckWhenGatedIn
    describe "Spider" $ do
        it "normally has only 2 moves" $
            spiderMoves boardWithRing (Axial 0 4)
                `shouldMatchList` [ Axial 2 2, Axial (-2) 4 ]
        it "even has 2 moves inside this ring" $
            spiderMoves boardWithRing (Axial (-1) 2)
                `shouldMatchList` [ Axial 0 1, Axial 0 2 ]
        it "gets extra moves at a door" $
            spiderMoves boardWithDoor (Axial 1 2)
                `shouldMatchList` [ Axial 2 1, Axial (-1) 2, Axial (-1) 4, Axial (-2) 4 ]
        it "cannot pass through gates" $ do
            -- bS1 bA1-
            let board = addPiece bS1 (Axial 2 2) boardWithGate
            spiderMoves board (Axial 2 2) `shouldNotContain` [gatedHex]
        it "is stuck if surrounded" $
            wS2 `shouldSatisfy` isStuckWhenSurrounded
        it "is stuck if gated in" $
            wS2 `shouldSatisfy` isStuckWhenGatedIn
    describe "Ring Formation" $
        it "frees up a bunch of bugs" $
            allFreePieces boardWithRing
                `shouldMatchList` [bB1, bQ,  wB1, wG1
                                  ,bA1, wA1, bS2, wA2
                                  ,bG1, bA2, wG2, bS1
                                  ]

