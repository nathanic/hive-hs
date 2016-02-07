module HexGrid
    ( AxialPoint(..)
    , Direction(..)
    , allDirections
    , directionVectors
    , neighbor
    , neighbors
    , gatePositions
    , findDirectionFromAxialPoints
    ) where

import Data.Maybe (fromJust)
import Data.Tuple (swap)

-- so we're using a pointy-top hex grid with an axial coordinate system
-- http://www.redblobgames.com/grids/hexagons/#coordinates

data Direction = NE | E | SE | SW | W | NW
  deriving (Show, Eq)

data Rotation = CW | CCW
  deriving (Show, Eq)

data PixelPoint = Pixel Double Double
  deriving (Show, Eq)

-- TODO: distinguish vectors from points?
-- axial points are always integer coordinates on the centers of hexes
data AxialPoint = Axial Int Int
  deriving (Show, Eq, Ord)

-- cubic hex space is more freeform for intermediate calculations
data CubicPoint = Cubic Double Double Double
  deriving (Show, Eq)

allDirections :: [Direction]
allDirections = [NE, E, SE, SW, W, NW]

opposite :: Direction -> Direction
opposite dir =
    case dir of
        NE -> SW
        E  -> W
        SE -> NW
        SW -> NE
        W  -> E
        NW -> SE

hexWidthFromSize :: Double -> Double
hexWidthFromSize size = size * sqrt 3.0

hexHeightFromSize :: Double -> Double
hexHeightFromSize size = size * 2.0

hexDimsFromSize :: Double -> (Double, Double)
hexDimsFromSize size = (hexWidthFromSize size, hexHeightFromSize size)

axialToCubic :: AxialPoint -> CubicPoint
axialToCubic (Axial p q) = Cubic (fromIntegral p) (fromIntegral q) (fromIntegral (-p - q))

cubicToAxial :: CubicPoint -> AxialPoint
cubicToAxial (Cubic x y z) = Axial (round x) (round y)

directionVectors :: [(Direction, AxialPoint)]
directionVectors =
    [ (NE, Axial  1  (-1))
    , (E , Axial  1    0 )
    , (SE, Axial  0    1 )
    , (SW, Axial (-1)  1 )
    , (W , Axial (-1)  0 )
    , (NW, Axial  0  (-1))
    ]

directionToAxialVector :: Direction -> AxialPoint
directionToAxialVector dir =
  fromJust $ lookup dir directionVectors

neighbor :: Direction -> AxialPoint -> AxialPoint
neighbor dir (Axial p q) = Axial (p + dp) (q + dq)
  where Axial dp dq = directionToAxialVector dir

neighbors :: AxialPoint -> [AxialPoint]
neighbors axpt = map (`neighbor` axpt) [NE,E,SE,SW,W,NW]

rotate60 :: CubicPoint -> Rotation -> CubicPoint
rotate60 (Cubic x y z)  CW = Cubic (-y) (-z) (-x)
rotate60 (Cubic x y z) CCW = Cubic (-z) (-x) (-y)

plus (Cubic x1 y1 z1) (Cubic x2 y2 z2) = Cubic (x1+x2) (y1+y2) (z1+z2)
minus (Cubic x1 y1 z1) (Cubic x2 y2 z2) = Cubic (x1-x2) (y1-y2) (z1-z2)

gatePositions :: AxialPoint -> AxialPoint -> (AxialPoint, AxialPoint)
gatePositions axA axB = (cubicToAxial gate1, cubicToAxial gate2)
  where
    (cuA, cuB) = (axialToCubic axA, axialToCubic axB)
    cuVec = cuB `minus` cuA
    gate1 = cuA `plus` rotate60 cuVec CW
    gate2 = cuA `plus` rotate60 cuVec CCW

findDirectionFromAxialPoints :: AxialPoint -> AxialPoint -> Maybe Direction
findDirectionFromAxialPoints (Axial pFrom qFrom) (Axial pTo qTo) =
    lookup (Axial (oneOrZero (pTo - pFrom)) (oneOrZero (qTo - qFrom)))
        $ map swap directionVectors
  where oneOrZero x = if x >= 1 then 1 else 0

