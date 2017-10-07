module Main where

import System.Random
import Control.Monad
import View
import Graphics.Gloss
import Data.List

fps :: Int
fps = 10

size :: Int
size = 50

main :: IO ()
main = do
  rands <- replicateM (size * size - 2) $ (getStdRandom $ randomR (0, 1) :: IO Double)
  let modRands = (2 : rands) ++ [-2]
  let initialState = aina size (map modRandToState modRands)
  -- let initialState = [[ x `mod` 2 == 0 | x <- [0..size]] | y <- [0..size]]
  run size fps initialState transition cellStateToColor

modRandToState :: Double -> CellState
modRandToState x
  | x < -1 = Slime 0
  | x > 1 = Goal
  | x < 0.2 = Wall
  | otherwise = Road

aina :: Int -> [a] -> [[a]]
aina _ [] = []
aina n a = take n a : aina n (drop n a)

-- 状態の型
data CellState = Goal | Road | Wall | Trace | Slime Int 

-- 遷移関数
transition :: [[CellState]] -> [[CellState]]
transition prevField = nextField
  where
    nextField = (map . map) step $ withCoord prevField
    aroundCell (x, y) = map (getCell prevField) . map (\(dx, dy) -> (x + dx, y + dy)) $ around
    step ((x, y), s) = rule s $ aroundCell (x, y)

getCell :: [[CellState]] -> (Int, Int) -> CellState
getCell field (x, y) = cell
  where
    cell =
      if x < 0 || y < 0 || x > length field - 1 || y > (length . head) field - 1 then
        Wall
      else
        field !! y !! x

rule :: CellState -> [CellState] -> CellState
rule Road s = maybe Road id $ foldr f Nothing $ filter isSlime s
  where
    f :: CellState -> Maybe Int -> Maybe Int
    f (Slime i) Nothing = Just i
    f (Slime i) (Just j)
      | i > j = Just i
      | otherwise = Just j
    isSlime :: CellState -> Bool
    isSlime Slime _ = True
    isSlime _ = False
rule c _ = c

type WithCoord a = ((Int, Int), a)

withCoord :: [[CellState]] -> [[WithCoord CellState]]
withCoord field = (map (uncurry zip) . zip indicies) field
  where
    indicies = [ [ (x, y) | x <- [0..(length . head) field] ] | y <- [0..length field] ]

delta :: [Int]
delta = [0, -1, 1]

around :: [(Int, Int)]
around = [(0, 1), (1, 0), (0, -1), (-1, 0)]

-- 色付けのルール
cellStateToColor :: CellState -> Color
cellStateToColor Road = white
cellStateToColor Wall = Black
cellStateToColor Trace = yellow
cellStateToColor Goal = red
cellStateToColor Slime _ = green
