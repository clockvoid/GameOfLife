module View (
    cell
    , backGround
    , fieldWidth
    , troutNumber
    , render
    , size
    , createWindow
) where

import Graphics.Gloss

type Field = [[Color]]

fieldWidth :: Float
fieldWidth = 400

troutNumber :: Float
troutNumber = 40

size :: Int
size = truncate troutNumber

triangles :: Float -> Float -> [Path]
triangles x' y' = [[(x, y), (x + fieldWidth / troutNumber, y), (x, y - fieldWidth / troutNumber)], [(x + fieldWidth / troutNumber, y), (x, y - fieldWidth / troutNumber), (x + fieldWidth / troutNumber, y - fieldWidth / troutNumber)]]
    where
        x = x' * fieldWidth / troutNumber - fieldWidth / 2
        y = -1 * y' * fieldWidth / troutNumber + fieldWidth / 2

cell :: (Float, Float, Color) -> Picture
cell (x, y, c) = color c (pictures (map polygon (triangles x y)))

coordinate :: Field -> [(Float, Float, Color)]
coordinate field = concat $ map (\(x, cs) -> [(x, fst y, snd y) | y <- zip [0..troutNumber - 1] cs]) $ zip [0..troutNumber - 1] field

render :: Field -> Picture
render field = pictures (map cell (coordinate field) ++ [backGround])

makeVerticalLines :: Float -> [Picture]
makeVerticalLines x = if x <= fieldWidth / 2
    then [line [(x, fieldWidth / 2), (x, -1 * fieldWidth / 2)]] ++ makeVerticalLines (x + fieldWidth / troutNumber)
    else []

makeHorizontalLines :: Float -> [Picture]
makeHorizontalLines y = if y <= fieldWidth / 2
    then [line [(fieldWidth / 2, y), (-1 * fieldWidth / 2, y)]] ++ makeHorizontalLines (y + fieldWidth / troutNumber)
    else []

backGround :: Picture
backGround = pictures $ makeVerticalLines (-1 * fieldWidth / 2) ++ makeHorizontalLines (-1 * fieldWidth / 2)

createWindow :: String -> Display
createWindow windowName = InWindow windowName (round fieldWidth + 50, round fieldWidth + 50) (10, 10)
