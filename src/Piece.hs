module Piece where

import Data.Function (on)
data Species = Ant | Beetle | Grasshopper | Ladybug | Mosquito | Pillbug | QueenBee | Spider
  deriving (Eq,Show)

data Team = White | Black
  deriving (Eq,Show)

-- TODO? i wonder if it'd be nicer to just newtype this on the name
-- and have fns to extract the Species and Team on demand?
-- the name is enough info to calculate the rest
-- oh well, this works and would be easy enough to swap out
data Piece = Piece
    { pieceSpecies :: Species
    , pieceTeam :: Team
    , pieceName :: String
    } deriving (Eq)

instance Show Piece where
    show (Piece _ _ name) = name

instance Ord Piece where
    compare = compare `on` pieceName

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

