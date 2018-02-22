{-# LANGUAGE LambdaCase #-}

module Simulator.Rules
    ( Brick(..)
    , Rule(..)
    , applyRule
    , corners
    , edges
    , solution
    , rules
    ) where

import           Data.Bifunctor   ( bimap, first )

import           Simulator.Types

-- determinant :: Matrix -> Int
-- determinant (Matrix (Vector a b c)
--                     (Vector d e f)
--                     (Vector g h i)) =
--   a * (e * i - h * f) - d * (b * i - h * c) + g * (b * f - e * c)

data Rule = Rule { axis        :: !Vector
                 , layer       :: !Int
                 , orientation :: !Orientation
                 , rotation    :: !Matrix
                 }

instance Show Rule
  where
    show (Rule axis layer orientation _) =
      show axis' ++ show orientation'
      where
        (axis', orientation') = case layer of
          1 -> (axis, orientation)
          -1 -> (invV axis, invO orientation)

data Brick = Brick { brickPosition :: !Vector
                   , brickFace1    :: !(Vector, Color)
                   , brickFace2    :: !(Vector, Color)
                   , brickFace3    :: !(Vector, Color)
                   } deriving Eq

instance Show Brick
  where
    show (Brick pos face1 face2 face3)
      = show posCs ++ ", " ++ show face1 ++ ", " ++ show face2
     ++ if snd face3 == Black then "" else (", " ++ show face3)
      where
        posCs = filter (/= Black)
              . map toColor
              . components
              $ pos
        toColor (c, v) = case c of
            1 -> color v
            -1 -> color . invV $ v
            _ -> Black

ops :: [Vector -> Vector]
ops = [id, invV]

corners, edges :: [Brick]
corners = genBricks cornerGenerator
edges = genBricks edgeGenerator

solution :: [Brick]
solution = corners ++ edges

genBricks :: Generator -> [Brick]
genBricks gen = [ Brick (foldl1 addVV vs)
                        (f1, color f1)
                        (f2, color f2)
                        (if null fs then (zero, Black) else (head fs, color . head $ fs))
                | (ops, axes) <- gen
                , let vs@(f1 : f2 : fs) = zipWith ($) ops axes
                ]

type Generator = [([Vector -> Vector], [Vector])]

cornerGenerator :: Generator
cornerGenerator = [([op1, op2, op3], colors)
                  | op1 <- ops, op2 <- ops, op3 <- ops
                  ]

edgeGenerator :: Generator
edgeGenerator = [([op, invV . op], [c1, c2])
                | op <- ops
                , c1 <- colors, c2 <- colors, c2 /= c1
                ]
buildRotation :: Vector -> Orientation -> Matrix
buildRotation (Vector x y z) orientation =
    (sin orientation `multSM` ux) `addMM` uut
  where -- see https://en.wikipedia.org/wiki/Rotation_matrix
    sin Clockwise = -1 -- -pi/2
    sin CounterClockwise = 1 -- pi/2
    uut = Matrix (Vector (x * x) (x * y) (x * z))
                 (Vector (x * y) (y * y) (y * z))
                 (Vector (x * z) (y * z) (z * z))
    ux = Matrix (Vector 0 (-z) y)
                (Vector z 0 (-x))
                (Vector (-y) x 0)

rules :: [Rule]
rules = [ Rule a l orientation (buildRotation a orientation)
        | a <- colors
        , orientation <- [Clockwise, CounterClockwise]
        , l <- [1,-1]
        ]

applyRule :: Rule -> Brick -> Brick
applyRule (Rule a l _ r) b@(Brick pos face1 face2 face3)
    | pos `multVV` a == l = Brick (r `multMV` pos)
                                  (first (r `multMV`) face1)
                                  (first (r `multMV`) face2)
                                  (first (r `multMV`) face3)
    | otherwise = b

-- getRotationAxis :: Matrix -> Vector
-- getRotationAxis (Matrix (Vector a b c)
--                         (Vector d e f)
--                         (Vector g h i)) =
--                 Vector (h - f) (c - g) (d - b)

-- faceColors :: [Brick] -> Vector -> Vector -> [Color]
-- faceColors cube face up = []
--   where
--     faceBricks = filter isFace cube
--     isFace brick = face `multVV` (brickPosition brick) == 1
