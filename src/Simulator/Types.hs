{-# LANGUAGE LambdaCase #-}

module Simulator.Types
    where

-- some acronyms
-- s: scalar, v: Vector, m: Vector, o: Orientation
-- a: add, m: multiply, i: inverse, t: transpose
-- C: color, F: face, S: side, P: Piece

-- three dimensional vectors, note: we just need integers as scalars
data Vector = Vector { x :: !Int, y :: !Int, z :: !Int } deriving (Eq, Show)

-- scale vector (multiply scalar with vector)
-- , note: we only need integers as scalars
msv :: Int -> Vector -> Vector
msv a (Vector x y z) = Vector (a * x) (a * y) (a * z)

-- inverse a vector
iv :: Vector -> Vector
iv = ((-1) `msv`)

-- vector scalar product
mvv :: Vector -> Vector -> Int
mvv (Vector a1 a2 a3) (Vector b1 b2 b3) = a1*b1 + a2*b2 + a3*b3

-- add two vectors
avv :: Vector -> Vector -> Vector
avv (Vector a1 a2 a3) (Vector b1 b2 b3) = Vector (a1+b1) (a2+b2) (a3+b3)

-- matrix for vector rotation
data Matrix = Matrix { row1 :: !Vector, row2 :: !Vector, row3 :: !Vector }
  deriving Show

-- multiply matrix with vector
mmv :: Matrix -> Vector -> Vector
mmv (Matrix v1 v2 v3) w =
    Vector (v1 `mvv` w)
           (v2 `mvv` w)
           (v3 `mvv` w)

-- transpose a matrix
tm :: Matrix -> Matrix
tm (Matrix (Vector _11 _12 _13)
           (Vector _21 _22 _23)
           (Vector _31 _32 _33)) =
    Matrix (Vector _11 _21 _31)
           (Vector _12 _22 _32)
           (Vector _13 _23 _33)

-- scale matrix (multiply scalar with matrix)
msm :: Int -> Matrix -> Matrix
msm s (Matrix r1 r2 r3) =
  Matrix (s `msv` r1)
         (s `msv` r2)
         (s `msv` r3)

-- matrix product
mmm :: Matrix -> Matrix -> Matrix
mmm (Matrix r1 r2 r3) m2 =
  Matrix (Vector (r1 `mvv` t1) (r1 `mvv` t2) (r1 `mvv` t3))
         (Vector (r2 `mvv` t1) (r2 `mvv` t2) (r2 `mvv` t3))
         (Vector (r3 `mvv` t1) (r3 `mvv` t2) (r3 `mvv` t3))
  where
    Matrix t1 t2 t3 = tm m2

-- matrix addition
amm :: Matrix -> Matrix -> Matrix
amm (Matrix l1 l2 l3) (Matrix r1 r2 r3) =
  Matrix (l1 `avv` r1) (l2 `avv` r2) (l3 `avv` r3)

