module View (
  run
) where

import Control.Monad

type Color = String

type Field = [[Color]]

red :: Color
red = "\x1b[41m"

black :: Color
black = "\x1b[40m"

backGround :: Color
backGround = "\x1b[49m"

clearConsole:: IO ()
clearConsole = putStr "\e[;H\e[2J"

block :: String
block = "  "

draw :: [[Color]] -> IO ()
draw = clearConsole >>

{-
import Graphics.Gloss


cellSize :: Float
cellSize = 10

bgColor :: Color
bgColor = white

windowName :: String
windowName = "Cell Automaton"

run :: Int -> Int -> [[a]] -> ([[a]] -> [[a]]) -> (a -> Color) -> IO ()
run size fps initialState next stateToColor = simulate window bgColor fps initialState renderState $ const . const next
  where
    s = (fromInteger . toInteger) size
    window = createWindow s windowName
    renderState = (render s) . (map . map) stateToColor


triangles :: Float -> Float -> Float -> [Path]
triangles s x' y' = [[(x, y), (x + (cellSize * s) / s, y), (x, y - (cellSize * s) / s)], [(x + (cellSize * s) / s, y), (x, y - (cellSize * s) / s), (x + (cellSize * s) / s, y - (cellSize * s) / s)]]
    where
        x = x' * (cellSize * s) / s - (cellSize * s) / 2
        y = -1 * y' * (cellSize * s) / s + (cellSize * s) / 2

cell :: Float -> (Float, Float, Color) -> Picture
cell s (x, y, c) = color c (pictures (map polygon (triangles s x y)))

coordinate :: Float -> Field -> [(Float, Float, Color)]
coordinate s field = concat $ map (\(x, cs) -> [(x, fst y, snd y) | y <- zip [0..s - 1] cs]) $ zip [0..s - 1] field

render :: Float -> Field -> Picture
render s field = pictures (map (cell s) (coordinate s field) ++ [backGround s])

makeVerticalLines :: Float -> Float -> [Picture]
makeVerticalLines s x = if x <= (cellSize * s) / 2
    then [line [(x, (cellSize * s) / 2), (x, -1 * (cellSize * s) / 2)]] ++ makeVerticalLines s (x + (cellSize * s) / s)
    else []

makeHorizontalLines :: Float -> Float -> [Picture]
makeHorizontalLines s y = if y <= (cellSize * s) / 2
    then [line [((cellSize * s) / 2, y), (-1 * (cellSize * s) / 2, y)]] ++ makeHorizontalLines s (y + (cellSize * s) / s)
    else []

backGround :: Float -> Picture
backGround s = pictures $ makeVerticalLines s (-1 * (cellSize * s) / 2) ++ makeHorizontalLines s (-1 * (cellSize * s) / 2)

createWindow :: Float -> String -> Display
createWindow s windowName = InWindow windowName (round (cellSize * s) + 50, round (cellSize * s) + 50) (10, 10)

-}
