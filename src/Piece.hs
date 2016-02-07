module Piece where

data Species = Ant | Beetle | Grasshopper | Ladybug | Mosquito | Pillbug | QueenBee | Spider
  deriving (Eq,Show)

data Team = White | Black
  deriving (Eq,Show)

opposing :: Team -> Team
opposing White = Black
opposing Black = White

newtype Piece = Piece {pieceName :: String}
  deriving (Eq,Ord)

instance Show Piece where
  show = pieceName

pieceTeam (Piece (t:_)) =
    case t of
        'b' -> Black
        'w' -> White
        _   -> error "pieceFromName: invalid team"

-- TODO: validation here?
piece :: String -> Piece
piece = Piece

pieceSpecies :: Piece -> Species
pieceSpecies (Piece (_:s:_)) =
    case s of
        'A' -> Ant
        'B' -> Beetle
        'G' -> Grasshopper
        'L' -> Ladybug
        'M' -> Mosquito
        'P' -> Pillbug
        'Q' -> QueenBee
        'S' -> Spider
        _   -> error "pieceFromName: invalid species"

-- | the list of all possible pieces.
allPieces :: [Piece]
allPieces = map piece
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
