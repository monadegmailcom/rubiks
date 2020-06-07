module Simulator.Rules
    ( Rule(..)
    , applyRule
    , applyRules
    , rotR, rotR', rotB, rotB', rotO, rotO', rotY, rotY', rotG, rotG', rotW
    , rotW') where

import Simulator.Cube
import Simulator.Types

-- two orientations
data Orientation = Clockwise | CounterClockwise

-- build the rotation matrix for clockwise and counterclockwise rotations with
-- given axis. Note: only the angles -pi/2 and pi/2 are needed.
buildRotation :: Vector -> Orientation -> Matrix
buildRotation (Vector x y z) orientation =
    (sin' orientation `msm` ux) `amm` uut
  where -- see https://en.wikipedia.org/wiki/Rotation_matrix
    sin' Clockwise = -1 -- -pi/2
    sin' CounterClockwise = 1 -- pi/2
    uut = Matrix (Vector (x * x) (x * y) (x * z))
                 (Vector (x * y) (y * y) (y * z))
                 (Vector (x * z) (y * z) (z * z))
    ux = Matrix (Vector 0 (-z) y)
                (Vector z 0 (-x))
                (Vector (-y) x 0)

-- rule to apply to the cube/pieces, the axis points
-- in the direction of the cube's side, which is rotates.
data Rule = Rule { ruleAxis     :: !Vector
                 , ruleRotation :: !Matrix
                 }

-- generate all 12 different rules
rules :: [Rule]
rules = [ Rule a (buildRotation a orientation)
        | a <- [red, blue, orange, yellow, green, white]
        , orientation <- [Clockwise, CounterClockwise]
        ]

-- the ticked rotations are counter clockwise
rotR, rotR', rotB, rotB', rotO, rotO', rotY, rotY', rotG, rotG', rotW, rotW'
    :: Rule
rotR = rules !! 0
rotR' = rules !! 1
rotB = rules !! 2
rotB' = rules !! 3
rotO = rules !! 4
rotO' = rules !! 5
rotY = rules !! 6
rotY' = rules !! 7
rotG = rules !! 8
rotG' = rules !! 9
rotW = rules !! 10
rotW' = rules !! 11

-- apply the rule to a brick
applyRule :: Rule -> Brick -> Brick
applyRule (Rule a r) b@(Brick pos piece)
    | pos `mvv` a == 1 = case piece of
        CornerPiece f1 f2 f3 ->
            Brick pos' (CornerPiece (rotateFace f1)
                                    (rotateFace f2)
                                    (rotateFace f3))
        EdgePiece f1 f2 ->
            Brick pos' (EdgePiece (rotateFace f1)
                                  (rotateFace f2))
    | otherwise = b
  where
    pos' = r `mmv` pos
    rotateFace (Face c d) = Face c (r `mmv` d)

-- apply rules to the cube.
applyRules :: Cube -> [Rule] -> Cube
applyRules = foldl (\bs r -> map (applyRule r) bs)

