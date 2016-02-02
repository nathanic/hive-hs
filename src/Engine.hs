module Engine where

import Data.Map.Strict (Map)
import qualified HexGrid as Grid

-- game engine

data Species = Ant | Beetle | Grasshopper | Ladybug | Mosquito | Pillbug | QueenBee | Spider
  deriving (Eq,Show)

data Team = White | Black
  deriving (Eq,Show)

data Piece = Piece
    { pieceSpecies :: Species
    , pieceTeam :: Team
    , pieceName :: String
    } deriving (Eq,Show)

type Board = Map Grid.AxialPoint [Piece]

data Game = Game { gameId :: Integer -- TODO: fancier ID type?
                 , gametitle :: String
                 , gameBoard :: Board
                 , gameUnplaced :: [Piece] -- Set?
                 , gameMoves :: [Move] -- move history
                 , gamePossibleMoves :: Map Piece [Move]
                 , gameSpawns :: [Grid.AxialPoint]
                 , gameTurn :: Team
                 , gameWinner :: Maybe Team
                 -- TODO: players' user account reference of some kind?
                 } deriving (Eq, Show)

data Move = Move { movePiece :: Piece
                 , moveCoords :: Grid.AxialPoint
                 -- , moveName :: String -- e.g. "bA2 /wG1"
                 } deriving (Eq,Show)

-- | the set of all possible pieces.
allPieces :: [Piece]
allPieces = map pieceFromName
             ["bA1","bA2","bA3"
             ,"bB1","bB2"
             ,"bG1","bG2","bG3"
             ,"bL"
             ,"bM"
             ,"bP"
             ,"bQ"
             ,"bS1","bS2"
             ,"wA1","wA2","wA3"
             ,"wB1","wB2"
             ,"wG1","wG2","wG3"
             ,"wL"
             ,"wM"
             ,"wP"
             ,"wQ"
             ,"wS1","wS2"
             ]

pieceFromName :: String -> Piece
pieceFromName name = Piece species team name
  where
    t:s:_ = name
    team = case t of
              'b' -> Black
              'w' -> White
              _   -> error "pieceFromName: invalid team"
    species = case s of
                'A' -> Ant
                'B' -> Beetle
                'G' -> Grasshopper
                'L' -> Ladybug
                'M' -> Mosquito
                'P' -> Pillbug
                'Q' -> QueenBee
                'S' -> Spider
                _   -> error "pieceFromName: invalid species"

-- allowedMoves :: Board -> Piece -> Graph.AxialPoint -> [Move]
-- allowedMoves board (Piece QueenBee _ _) pos

