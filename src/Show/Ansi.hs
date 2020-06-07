module Show.Ansi
    ( showC
    , showCube
    ) where

import Simulator.Cube
import Simulator.Types

import qualified Data.Text.Lazy.IO as TL
import qualified System.Console.ANSI as A
import qualified Data.Colour.Names as N

showC :: Color -> IO ()
showC c = A.setSGR [A.SetRGBColor A.Background color]
        >> TL.putStr "  "
  where
    color = case c of
        Red -> N.red
        Blue -> N.blue
        Orange -> N.orange
        Yellow -> N.yellow
        Green -> N.forestgreen
        White -> N.white

-- fold 2d model with blue side above to get back cube
showCube :: Cube -> IO ()
showCube cube = do
    (r, c) <- A.getCursorPosition >>= maybe (error "no cursor position")
                                            return
    let rows = 11
    A.scrollPageUp rows
    mapM_ (uncurry $ plot (r - rows)) colorCoords
    A.setCursorPosition r c
    A.setSGR [A.Reset]
  where
    plot base color (r, c) = do
        A.setCursorPosition (base + r) c
        showC color
    colorCoords = map swapCoord $ foldl buildColorCoord [] cube ++
        [ (Yellow, (8, 1)), (Orange, (2, 4)), (Green, (20, 4)), (Red, (14, 4))
        , (Blue, (8, 4)), (White, (8, 7))]
    swapCoord (color, (x, y)) = (color, (y, x))
    buildColorCoord acc = \case
        Brick pos (CornerPiece f1 f2 f3) ->
              (faceColor f1, pos2Coord pos f1)
            : (faceColor f2, pos2Coord pos f2)
            : (faceColor f3, pos2Coord pos f3)
            : acc
        Brick pos (EdgePiece f1 f2) ->
              (faceColor f1, pos2Coord pos f1)
            : (faceColor f2, pos2Coord pos f2)
            : acc
    pos2Coord pos face = let dir = faceDirection face
                             (right, down, x, y) = dir2RightDownOffset dir
                         in (x + 2 * pos `mvv` right, y + pos `mvv` down)
    dir2RightDownOffset dir
        | dir == blue = (red, white, 8, 4)
        | dir == white = (red, green, 8, 7)
        | dir == red = (green, white, 14, 4)
        | dir == orange = (blue, white, 2, 4)
        | dir == yellow = (red, blue, 8, 1)
        | dir == green = (orange, white, 20, 4)
        | otherwise = error "dir2RightDown: invalid dir"

