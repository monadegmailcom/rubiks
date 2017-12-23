module Types
  where

data Color = Red | Blue | Orange | Yellow | Green | White
  deriving (Show)

type Corner = (Color, Color, Color)
type Edge = (Color, Color)

data Rotation = Pass | Clockwise | CounterClockwise

data Cube = Cube { posWhiteOrangeGreen  :: !(Corner, Rotation)
                 , posWhiteBlueOrange   :: !(Corner, Rotation)
                 , posWhiteRedBlue      :: !(Corner, Rotation)
                 , posWhiteGreenRed     :: !(Corner, Rotation)
                 , posYellowGreenOrange :: !(Corner, Rotation)
                 , posYellowRedGreen    :: !(Corner, Rotation)
                 , posYellowBlueRed     :: !(Corner, Rotation)
                 , posYellowOrangeBlue  :: !(Corner, Rotation)
                 , posWhiteOrange       :: !(Edge, Rotation)
                 , posWhiteBlue         :: !(Edge, Rotation)
                 , posWhiteRed          :: !(Edge, Rotation)
                 , posWhiteGreen        :: !(Edge, Rotation)
                 , posYellowGreen       :: !(Edge, Rotation)
                 , posYellowRed         :: !(Edge, Rotation)
                 , posYellowBlue        :: !(Edge, Rotation)
                 , posYellowOrange      :: !(Edge, Rotation)
                 , posBlueOrange        :: !(Edge, Rotation)
                 , posBlueRed           :: !(Edge, Rotation)
                 , posGreenRed          :: !(Edge, Rotation)
                 , posGreenOrange       :: !(Edge, Rotation)
                 }
