module Simulator.Cube
    ( Brick(..)
    , Color(..)
    , Cube
    , Face(..)
    , Piece(..)
    , red, blue, orange, yellow, green, white
    , solvedCube
    ) where

import Simulator.Types

-- colors taken from rubik's cube
data Color = Red | Blue | Orange | Yellow | Green | White
  deriving (Eq, Show)

-- some constants, orienting the cube in the space according to rubik's cube
-- we associate orientation in space with colors
red, blue, orange, yellow, green, white :: Vector
blue = Vector 1 0 0
white = Vector 0 1 0
red = Vector 0 0 1
green = iv blue
yellow = iv white
orange = iv red

-- a face has a color and an orientation in space
data Face = Face { faceColor :: !Color
                 , faceDirection :: !Vector
                 } deriving Show

-- there a two kinds of cube pieces: corner pieces with three visible faces
-- and edge pieces with two visible faces. A corner piece will always stay a
-- corner piece under rule application, as does an edge piece. Note: a piece
-- does not yet have a position.
data Piece = CornerPiece !Face !Face !Face
           | EdgePiece !Face !Face deriving Show

-- a brick adds a position to a piece.
data Brick = Brick { brickPosition :: Vector
                   , brickPiece :: Piece
                   } deriving Show

blueF, redF, whiteF, orangeF, yellowF, greenF :: Face
blueF = Face Blue blue
redF = Face Red red
whiteF = Face White white
orangeF = Face Orange orange
yellowF = Face Yellow yellow
greenF = Face Green green

bwrP, bryP, bwoP, byoP, wrgP, wogP, rygP, yogP, bwP, brP, boP, byP, gyP, goP
    , gwP, grP, wrP, woP, yoP, yrP :: Piece
bwrP = CornerPiece blueF whiteF redF
bryP = CornerPiece blueF redF yellowF
bwoP = CornerPiece blueF whiteF orangeF
byoP = CornerPiece blueF yellowF orangeF
wrgP = CornerPiece whiteF redF greenF
wogP = CornerPiece whiteF orangeF greenF
rygP = CornerPiece redF yellowF greenF
yogP = CornerPiece yellowF orangeF greenF
bwP = EdgePiece blueF whiteF
brP = EdgePiece blueF redF
boP = EdgePiece blueF orangeF
byP = EdgePiece blueF yellowF
gyP = EdgePiece greenF yellowF
goP = EdgePiece greenF orangeF
gwP = EdgePiece greenF whiteF
grP = EdgePiece greenF redF
wrP = EdgePiece whiteF redF
woP = EdgePiece whiteF orangeF
yoP = EdgePiece yellowF orangeF
yrP = EdgePiece yellowF redF

type Cube = [Brick]

-- the original solved cube consists of these bricks
solvedCube :: Cube
solvedCube = map (\piece -> Brick (solvedPos piece) piece) pieces
  where
    solvedPos (CornerPiece a b c) = faceDirection a `avv` faceDirection b `avv`
                                   faceDirection c
    solvedPos (EdgePiece a b) = faceDirection a `avv` faceDirection b
    pieces = [ bwrP, bryP, bwoP, byoP, wrgP, wogP, rygP, yogP, bwP, brP, boP
             , byP, gyP, goP, gwP, grP, wrP, woP, yoP, yrP]


