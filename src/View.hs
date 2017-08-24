module View (
    cell
    , backGround
    , fieldWidth
    , troutNumber
    , render
) where

import Graphics.Gloss

type Field = [[Color]]

fieldWidth :: Float
fieldWidth = 200

troutNumber :: Float
troutNumber = 20

triangles :: Float -> Float -> [Path]
triangles x' y' = [[(x, y), (x + fieldWidth / troutNumber, y), (x, y - fieldWidth / troutNumber)], [(x + fieldWidth / troutNumber, y), (x, y - fieldWidth / troutNumber), (x + fieldWidth / troutNumber, y - fieldWidth / troutNumber)]]
    where
        x = x' * fieldWidth / troutNumber - 100
        y = -1 * y' * fieldWidth / troutNumber + 100

cell :: (Float, Float, Color) -> Picture
cell (x, y, c) = color c (pictures (map polygon (triangles x y)))

coordinate :: Field -> [(Float, Float, Color)]
coordinate field = concat $ map (\(x, cs) -> [(x, fst y, snd y) | y <- zip [0..troutNumber - 1] cs]) $ zip [0..troutNumber - 1] field

render :: Field -> Picture
render field = pictures (map cell (coordinate field))

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

