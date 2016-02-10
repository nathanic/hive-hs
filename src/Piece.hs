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

-- -- | the list of all possible pieces.
-- allPieces :: [Piece]
-- allPiece
--     = map piece
--              [ "bA1","bA2","bA3"
--              , "bB1","bB2"
--              , "bG1","bG2","bG3"
--              , "bL"
--              , "bM"
--              , "bP"
--              , "bQ"
--              , "bS1","bS2"
--              , "wA1","wA2","wA3"
--              , "wB1","wB2"
--              , "wG1","wG2","wG3"
--              , "wL"
--              , "wM"
--              , "wP"
--              , "wQ"
--              , "wS1","wS2"
--              ]

-- naming them all is just convenient for the repl
-- allows succinct Show instance for Piece
bA1 = piece "bA1"; bA2 = piece "bA2"; bA3 = piece "bA3";
bB1 = piece "bB1"; bB2 = piece "bB2";
bG1 = piece "bG1"; bG2 = piece "bG2"; bG3 = piece "bG3";
bL = piece "bL";
bM = piece "bM";
bP = piece "bP";
bQ = piece "bQ";
bS1 = piece "bS1"; bS2 = piece "bS2";
wA1 = piece "wA1"; wA2 = piece "wA2"; wA3 = piece "wA3";
wB1 = piece "wB1"; wB2 = piece "wB2";
wG1 = piece "wG1"; wG2 = piece "wG2"; wG3 = piece "wG3";
wL = piece "wL";
wM = piece "wM";
wP = piece "wP";
wQ = piece "wQ";
wS1 = piece "wS1"; wS2 = piece "wS2";


allPieces :: [Piece]
allPieces = [ bA1, bA2, bA3
            , bB1, bB2
            , bG1, bG2, bG3
            , bL
            , bM
            , bP
            , bQ
            , bS1, bS2
            , wA1, wA2, wA3
            , wB1, wB2
            , wG1, wG2, wG3
            , wL
            , wM
            , wP
            , wQ
            , wS1, wS2
            ]


