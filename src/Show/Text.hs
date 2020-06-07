module Show.Text
    ( showC
    , showCube
    ) where

import Simulator.Cube
import Simulator.Types

import           Data.List ( sortBy )
import qualified Data.Text.Lazy as TL

showC :: Color -> TL.Text
showC Red = "r"
showC Blue = "b"
showC Orange = "o"
showC Yellow = "y"
showC Green = "g"
showC White = "w"

-- show cube as flattened 2d model, each side is shown from front view, each
-- side's orientation is according to the adjecent sides.
--    yyy
--    yyy
--    yyy
-- ooogggrrrbbb
-- ooogggrrrbbb
-- ooogggrrrbbb
--    www
--    www
--    www
showCube :: Cube -> TL.Text
showCube cube = TL.intercalate ""
              . map fst
              . sortBy sortF
              $ colorCoords
  where
    sortF lhs rhs = compare (swap . snd $ lhs) (swap . snd $ rhs)
    swap (x, y) = (y, x)
    colorCoords = foldl buildColorCoord [] cube ++
        [ ("   ", (0, 0)), ("   ", (0, 1)), ("   ", (0, 2))
        , ("\n", (6, 0)), ("\n", (6, 1)), ("\n", (6, 2))
        , ("\n", (12, 3)), ("\n", (12, 4)), ("\n", (12, 5))
        , ("   ", (0, 6)), ("   ", (0, 7)), ("   ", (0, 8))
        , ("\n", (6, 6)), ("\n", (6, 7)), ("\n", (6, 8))
        , ("y", (4, 1)), ("o", (1, 4)), ("g", (10, 4)), ("r", (7, 4))
        , ("b", (4, 4)), ("w", (4, 7))]
    buildColorCoord acc = \case
        Brick pos (CornerPiece f1 f2 f3) ->
              ((showC . faceColor) f1, pos2Coord pos f1)
            : ((showC . faceColor) f2, pos2Coord pos f2)
            : ((showC . faceColor) f3, pos2Coord pos f3)
            : acc
        Brick pos (EdgePiece f1 f2) ->
              ((showC . faceColor) f1, pos2Coord pos f1)
            : ((showC . faceColor) f2, pos2Coord pos f2)
            : acc
    pos2Coord pos face = let dir = faceDirection face
                             (right, down, x, y) = dir2RightDownOffset dir
                         in (x + pos `mvv` right, y + pos `mvv` down)
    dir2RightDownOffset dir
        | dir == blue = (red, white, 4, 4)
        | dir == white = (red, green, 4, 7)
        | dir == red = (green, white, 7, 4)
        | dir == orange = (blue, white, 1, 4)
        | dir == yellow = (red, blue, 4, 1)
        | dir == green = (orange, white, 10, 4)
        | otherwise = error "dir2RightDown: invalid dir"

