module Piece where

data Species = Ant | Beetle | Grasshopper | Ladybug | Mosquito | Pillbug | QueenBee | Spider
  deriving (Eq,Show)

data Team = White | Black
  deriving (Eq,Show)

data Piece = Piece
    { pieceSpecies :: Species
    , pieceTeam :: Team
    , pieceName :: String
    } deriving (Eq,Show)

