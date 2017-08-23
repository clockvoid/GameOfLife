module View (
    cell
) where

import Graphics.Gloss

type Field = [[Color]]

triangles :: Float -> Float -> (Path, Path)
triangles x y = ([(x, y), (x + 5, y), (x, y - 5)], [(x + 5, y), (x, y - 5), (x + 5, y - 5)])

picture :: (Path, Path) -> Picture
picture (a, b) = pictures [polygon a, polygon b]

cell :: (Float, Float) -> Color -> Picture
cell (x, y) c = color c (picture (triangles x y))

--coordinate :: Field -> [(Float, Float, Color)]
--coordinate field = [(x, y, c) | 

--render :: Field -> Picture

