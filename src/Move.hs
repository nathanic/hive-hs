module Move
    ( AbsoluteMove(..)
    , RelativeMove
    , interpretMove
    , parseMove
    ) where

-- this is mainly going to be about parsing and displaying moves in Hive notation

import Data.List (find)
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec

import HexGrid (AxialPoint(..),Direction)
import qualified HexGrid as Grid
import Board
import Piece


-- | the traditional notational representation for Hive moves
data RelativeMove = RelativeMove { moverPiece :: Piece
                                 , targetPiece :: Piece
                                 , dirFromTarget :: Direction
                                 }
                    -- first piece placed stands alone
                    | RelativeFirst { moverPiece :: Piece }
  deriving (Eq,Show)

-- | a more convenient machine representation for a move
data AbsoluteMove = AbsoluteMove { movePiece :: Piece
                                 , moveCoords :: AxialPoint
                                 }
  deriving (Eq,Show)

-- given a parsed move triple (mover,target,direction)
-- translate it into a Move using the Board
interpretMove :: Board -> RelativeMove -> AbsoluteMove
interpretMove board (RelativeFirst mover) = AbsoluteMove mover (Axial 0 0)
interpretMove board (RelativeMove mover target dir) = 
    AbsoluteMove mover (head $ findTopPieces (== target) board)

parseMove :: String -> Either ParseError RelativeMove
parseMove = parse moveP "MOVE"

moveP :: CharParser st RelativeMove
moveP = try nonFirstMoveP <|> firstMoveP

firstMoveP :: CharParser st RelativeMove
firstMoveP = RelativeFirst <$> pieceP

nonFirstMoveP :: CharParser st RelativeMove
nonFirstMoveP = do
    mover <- pieceP
    spaces
    (orient, target) <- flipFlop orientP pieceP
    return $ RelativeMove mover target (orientToDirection orient)

pieceP :: CharParser st Piece
pieceP = do
    name <- choice (try . string . pieceName <$> allPieces)
    return . fromJust $ find (\p -> pieceName p == name) allPieces

orientP :: CharParser st Char
orientP = char '/' <|> char '-' <|> char '\\'

-- | try ab, and if that fails try ba.
-- | encodes the position of `a` by returning an Either.
flipFlop :: GenParser tok st a -> GenParser tok st b -> GenParser tok st (Either a a, b)
flipFlop a b =
    try ( (,) <$> (Left <$> a) <*> b)
        <|> (flip (,) <$> b <*> (Right <$> a))

-- | this results in the direction from the old piece to the new piece
orientToDirection :: Either Char Char -> Direction
orientToDirection (Left '/')   = Grid.SW
orientToDirection (Left '-' )  = Grid.W
orientToDirection (Left '\\')  = Grid.NW
orientToDirection (Right '/')  = Grid.SE
orientToDirection (Right '-')  = Grid.E
orientToDirection (Right '\\') = Grid.NE


-- next, the inverse: describe a move
-- we know the mover, we know target pos
-- need to find a target piece the orientation back to mover
