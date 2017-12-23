{-# Language LambdaCase #-}

module Rules where

import Types


whiteOrangeGreen, whiteBlueOrange, whiteRedBlue, whiteGreenRed
 , yellowGreenOrange, yellowRedGreen, yellowBlueRed, yellowOrangeBlue :: Corner
[ whiteOrangeGreen, whiteBlueOrange, whiteRedBlue, whiteGreenRed
 , yellowGreenOrange, yellowRedGreen, yellowBlueRed, yellowOrangeBlue
 ]
 =
 [ (White, Orange, Green)
 , (White, Blue, Orange)
 , (White, Red, Blue)
 , (White, Green, Red)
 , (Yellow, Green, Orange)
 , (Yellow, Red, Green)
 , (Yellow, Blue, Red)
 , (Yellow, Orange, Blue)
 ]

whiteOrange, whiteBlue, whiteRed, whiteGreen, yellowGreen, yellowRed, yellowBlue
 , yellowOrange, blueOrange, blueRed, greenRed, greenOrange :: Edge
[ whiteOrange, whiteBlue, whiteRed, whiteGreen, yellowGreen, yellowRed
 , yellowBlue, yellowOrange, blueOrange, blueRed, greenRed, greenOrange]
 =
 [ (White, Orange)
 , (White, Blue)
 , (White, Red)
 , (White, Green)
 , (Yellow, Green)
 , (Yellow, Red)
 , (Yellow, Blue)
 , (Yellow, Orange)
 , (Blue, Orange)
 , (Blue, Red)
 , (Green, Red)
 , (Green, Orange)
 ]

(<->) :: Rotation -> Rotation -> Rotation
(<->) Pass x = x
(<->) x Pass = x
(<->) Clockwise Clockwise = CounterClockwise
(<->) CounterClockwise CounterClockwise = Clockwise
(<->) _ _ = Pass

ruleRed :: Rotation -> Cube -> Cube
ruleRed rotation cube =
  cube { posYellowBlueRed = update posYellowRedGreen
       , posYellowRedGreen = update posWhiteGreenRed
       , posWhiteGreenRed = update posWhiteRedBlue
       , posWhiteRedBlue = update posYellowBlueRed
       , posYellowRed = update posGreenRed
       , posGreenRed = update posWhiteRed
       , posWhiteRed = update posBlueRed
       , posBlueRed = update posYellowRed
       }
  where
    update f = ((fst . f) cube, rotation <-> (snd . f) cube)

ruleBlue :: Rotation -> Cube -> Cube
ruleBlue rotation cube =
  cube { posYellowOrangeBlue = update posYellowBlueRed
       , posYellowBlueRed = update posWhiteRedBlue
       , posWhiteRedBlue = update posWhiteBlueOrange
       , posWhiteBlueOrange = update posYellowOrangeBlue
       , posBlueRed = update posWhiteBlue
       , posWhiteBlue = update posBlueOrange
       , posBlueOrange = update posYellowBlue
       , posYellowBlue = update posBlueRed
       }
  where
    update f = ((fst . f) cube, rotation <-> (snd . f) cube)

ruleOrange :: Rotation -> Cube -> Cube
ruleOrange rotation cube =
  cube { posWhiteOrangeGreen = update posYellowGreenOrange
       , posYellowGreenOrange = update posYellowOrangeBlue
       , posYellowOrangeBlue = update posWhiteBlueOrange
       , posWhiteBlueOrange = update posWhiteOrangeGreen 
       , posGreenOrange = update posYellowOrange
       , posYellowOrange = update posBlueOrange
       , posBlueOrange = update posWhiteOrange
       , posWhiteOrange = update posGreenOrange
       }
  where
    update f = ((fst . f) cube, rotation <-> (snd . f) cube)

ruleYellow :: Rotation -> Cube -> Cube
ruleYellow rotation cube =
  cube { posYellowGreenOrange = update posYellowRedGreen
       , posYellowRedGreen = update posYellowBlueRed
       , posYellowBlueRed = update posYellowOrangeBlue
       , posYellowOrangeBlue = update posYellowGreenOrange
       , posYellowOrange = update posYellowGreen
       , posYellowGreen = update posYellowRed
       , posYellowRed = update posYellowBlue
       , posYellowBlue = update posYellowOrange
       }
  where
    update f = ((fst . f) cube, rotation <-> (snd . f) cube)

ruleGreen :: Rotation -> Cube -> Cube
ruleGreen rotation cube =
  cube { posWhiteOrangeGreen = update posWhiteGreenRed 
       , posWhiteGreenRed = update posYellowRedGreen 
       , posYellowRedGreen = update posYellowGreenOrange
       , posYellowGreenOrange = update posWhiteOrangeGreen
       , posWhiteGreen = update posGreenRed
       , posGreenRed = update posYellowGreen
       , posYellowGreen = update posGreenOrange
       , posGreenOrange = update posWhiteGreen
       }
  where
    update f = ((fst . f) cube, rotation <-> (snd . f) cube)

ruleWhite :: Rotation -> Cube -> Cube
ruleWhite rotation cube =
  cube { posWhiteGreenRed = update posWhiteOrangeGreen
       , posWhiteOrangeGreen = update posWhiteBlueOrange
       , posWhiteBlueOrange = update posWhiteRedBlue
       , posWhiteRedBlue = update posWhiteGreenRed
       , posWhiteBlue = update posWhiteRed
       , posWhiteRed = update posWhiteGreen
       , posWhiteGreen = update posWhiteOrange
       , posWhiteOrange = update posWhiteBlue
       }
  where
    update f = ((fst . f) cube, rotation <-> (snd . f) cube)
  
rulePass :: Cube -> Cube
rulePass = id

rules :: [Cube -> Cube]
rules = [ ruleRed Clockwise, ruleRed CounterClockwise
        , ruleBlue Clockwise, ruleBlue CounterClockwise
        , ruleOrange Clockwise, ruleOrange CounterClockwise
        , ruleYellow Clockwise, ruleYellow CounterClockwise
        , ruleGreen Clockwise, ruleGreen CounterClockwise
        , ruleWhite Clockwise, ruleWhite CounterClockwise
        ]

rulesWithPass :: [Cube -> Cube]
rulesWithPass = rulePass : rules

solution :: Cube
solution = Cube { posWhiteOrangeGreen  = (whiteOrangeGreen, Pass)
                , posWhiteBlueOrange   = (whiteBlueOrange, Pass) 
                , posWhiteRedBlue      = (whiteRedBlue, Pass)  
                , posWhiteGreenRed     = (whiteGreenRed, Pass) 
                , posYellowGreenOrange = (yellowGreenOrange, Pass) 
                , posYellowRedGreen    = (yellowRedGreen, Pass) 
                , posYellowBlueRed     = (yellowBlueRed, Pass) 
                , posYellowOrangeBlue  = (yellowOrangeBlue, Pass) 
                , posWhiteOrange       = (whiteOrange, Pass) 
                , posWhiteBlue         = (whiteBlue, Pass) 
                , posWhiteRed          = (whiteRed, Pass) 
                , posWhiteGreen        = (whiteGreen, Pass) 
                , posYellowGreen       = (yellowGreen, Pass) 
                , posYellowRed         = (yellowRed, Pass) 
                , posYellowBlue        = (yellowBlue, Pass) 
                , posYellowOrange      = (yellowOrange, Pass) 
                , posBlueOrange        = (blueOrange, Pass) 
                , posBlueRed           = (blueRed, Pass) 
                , posGreenRed          = (greenRed, Pass) 
                , posGreenOrange       = (greenOrange, Pass) 
                }

cornerRotate :: Corner -> Rotation -> Corner
cornerRotate (c1, c2, c3) = \case
  Pass             -> (c1, c2, c3)
  Clockwise        -> (c3, c1, c2)
  CounterClockwise -> (c2, c3, c1)

edgeRotate :: Edge -> Rotation -> Edge
edgeRotate (c1, c2) = \case
  Pass             -> (c1, c2)
  Clockwise        -> (c2, c1)
  CounterClockwise -> (c2, c1)
