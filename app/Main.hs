module Main where

import View
import Graphics.Gloss
import Data.List

fps :: Int
fps = 10

size :: Int
size = 60

main :: IO ()
main = run size fps initialState next cellStateToColor

-- 状態の型
type CellState = Bool

-- 初期状態
initialState :: [[CellState]]
initialState = [if mod x 2 == 0 then replicate size False else replicate size True | x <- [0.. size - 1]]

next :: [[CellState]] -> [[CellState]]
next prevField = nextField
  where
    nextField = (map . map) step $ withCoord prevField
    aroundCell (x, y) = map (getCell prevField) . map (\(dx, dy) -> (x + dx, y + dy)) $ around
    step ((x, y), s) = rule s $ foldr ((+) . boolToInt) 0 $ aroundCell (x, y)

getCell :: [[CellState]] -> (Int, Int) -> CellState
getCell field (x, y) = cell
  where
    cell =
      if x < 0 || y < 0 || x > length field - 1 || y > (length . head) field - 1 then
        False
      else
        field !! y !! x

rule :: Bool -> Int -> Bool
rule s c
  | s == True = if c == 2 || c == 3 then True else False
  | otherwise = if c == 3 then True else False

boolToInt :: Bool -> Int
boolToInt x = if x then 1 else 0

type WithCoord a = ((Int, Int), a)

withCoord :: [[CellState]] -> [[WithCoord CellState]]
withCoord field = (map (uncurry zip) . zip indicies) field
  where
    indicies = [ [ (x, y) | x <- [0..(length . head) field] ] | y <- [0..length field] ]

delta :: [Int]
delta = [0, -1, 1]

around :: [(Int, Int)]
around = filter notCenter $ concat $ map (\x -> map ((,) x) delta) delta
  where
    notCenter :: (Int, Int) -> Bool
    notCenter (x, y) = x /= 0 || y /= 0

-- 色付けのルール
cellStateToColor :: CellState -> Color
cellStateToColor model = c
  where
    c = if model then red else white
