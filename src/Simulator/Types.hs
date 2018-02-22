{-# LANGUAGE LambdaCase #-}

module Simulator.Types
  where

import Data.List ( find )

data Color = Red | Blue | Orange | Yellow | Green | White | Black
  deriving (Eq, Show)

-- S: scalar, V: Vector, M: Vector, O: Orientation

data Vector = Vector { x :: !Int, y :: !Int, z :: !Int } deriving Eq

multSV :: Int -> Vector -> Vector
multSV a (Vector x y z) = Vector (a * x) (a * y) (a * z)

invV :: Vector -> Vector
invV = ((-1) `multSV`)

multVV :: Vector -> Vector -> Int
multVV (Vector a1 a2 a3) (Vector b1 b2 b3) = a1*b1 + a2*b2 + a3*b3

addVV :: Vector -> Vector -> Vector
addVV (Vector a1 a2 a3) (Vector b1 b2 b3) = Vector (a1+b1) (a2+b2) (a3+b3)

red, blue, orange, yellow, green, white, zero :: Vector
blue = Vector 1 0 0
white = Vector 0 1 0
red = Vector 0 0 1
green = invV blue
yellow = invV white
orange = invV red
zero = Vector 0 0 0

colors :: [Vector]
colors = [blue, white, red]

colorMapping :: [(Vector, Color)]
colorMapping = [(blue, Blue), (white, White), (red, Red)
               , (green, Green), (yellow, Yellow), (orange, Orange)]

color :: Vector -> Color
color v = maybe Black snd . find ((== v) . fst) $ colorMapping

colorToVector :: Color -> Vector
colorToVector c = maybe zero fst . find ((== c) . snd) $ colorMapping

components :: Vector -> [(Int, Vector)]
components v = zip components colors
  where
    components = map (v `multVV`) colors

instance Show Vector where
  show v
      | c == Black = show (x v) ++ ", " ++ show (y v) ++ ", " ++ show (z v)
      | otherwise = show c
    where
      c = color v

data Matrix = Matrix { row1 :: !Vector, row2 :: !Vector, row3 :: !Vector }
  deriving Show

multMV :: Matrix -> Vector -> Vector
multMV (Matrix v1 v2 v3) w =
    Vector (v1 `multVV` w)
           (v2 `multVV` w)
           (v3 `multVV` w)

transp :: Matrix -> Matrix
transp (Matrix (Vector _11 _12 _13)
               (Vector _21 _22 _23)
               (Vector _31 _32 _33)) =
    Matrix (Vector _11 _21 _31)
           (Vector _12 _22 _32)
           (Vector _13 _23 _33)

multSM :: Int -> Matrix -> Matrix
multSM s (Matrix r1 r2 r3) =
  Matrix (s `multSV` r1)
         (s `multSV` r2)
         (s `multSV` r3)

multMM :: Matrix -> Matrix -> Matrix
multMM (Matrix r1 r2 r3) m2 =
  Matrix (Vector (r1 `multVV` t1) (r1 `multVV` t2) (r1 `multVV` t3))
         (Vector (r2 `multVV` t1) (r2 `multVV` t2) (r2 `multVV` t3))
         (Vector (r3 `multVV` t1) (r3 `multVV` t2) (r3 `multVV` t3))
  where
    Matrix t1 t2 t3 = transp m2

addMM :: Matrix -> Matrix -> Matrix
addMM (Matrix l1 l2 l3) (Matrix r1 r2 r3) =
  Matrix (l1 `addVV` r1) (l2 `addVV` r2) (l3 `addVV` r3)

data Orientation = Clockwise | CounterClockwise

instance Show Orientation
  where
    show Clockwise = ""
    show CounterClockwise = "'"

invO :: Orientation -> Orientation
invO = \case
    Clockwise -> CounterClockwise
    CounterClockwise -> Clockwise

