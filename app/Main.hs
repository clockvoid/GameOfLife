module Main where

import View
import Graphics.Gloss
import Data.List

fps :: Int
fps = 30

size :: Int
size = 60

main :: IO ()
main = run size fps initialState next cellStateToColor

-- 状態の型
type CellState = Bool

-- 初期状態
initialState :: [[CellState]]
initialState = [if mod x 2 == 0 then replicate size False else replicate size True | x <- [0.. size - 1]]

bin :: CellState -> Int
bin True = 1
bin False = 0

-- nextStep :: [CellState] -> [CellState] -> [CellState] -> [(CellState, Int)]
-- nextStep [] ys zs = [(ys !! x, if x == 0 then ((bin (ys !! (x + 1)) + (bin (zs !! x)) + (bin (zs !! (x + 1))))) else if x == size -1 then ((bin (ys !! (x - 1))) + (bin (zs !! (x - 1))) + (bin (zs !! (x)))) else ((bin (ys !! (x - 1))) + (bin (ys !! (x + 1))) + (bin (zs !! (x - 1))) + (bin (zs !! x)) + (bin (zs !! (x + 1))))) | x <- [0..size - 1]]
-- nextStep xs ys [] = [(ys !! x, if x == 0 then ((bin (ys !! (x + 1))) + (bin (xs !! x)) + (bin (xs !! (x + 1)))) else if x == size -1 then ((bin (ys !! (x - 1))) + (bin (xs !! (x - 1))) + (bin (xs !! (x)))) else ((bin (ys !! (x - 1))) + (bin (ys !! (x + 1))) + (bin (xs !! (x - 1))) + (bin (xs !! x)) + (bin (xs !! (x + 1))))) | x <- [0..size - 1]]
-- nextStep xs ys zs = [(ys !! x, if x == 0 then ((bin (xs !! x)) + (bin (xs !! (x + 1))) + (bin (ys !! (x + 1))) + (bin (zs !! x)) + (bin (zs !! (x + 1)))) else if x == size -1 then ((bin (xs !! (x - 1))) + (bin (xs !! (x))) + (bin (ys !! (x - 1))) + (bin (zs !! (x - 1))) + (bin (zs !! (x)))) else ((bin (xs !! (x - 1))) + (bin (xs !! x)) + (bin (xs !! (x + 1))) + (bin (ys !! (x - 1))) + (bin (ys !! (x + 1))) + (bin (zs !! (x - 1))) + (bin (zs !! x)) + (bin (zs !! (x + 1))))) | x <- [0..size - 1]]

blocks :: [CellState] -> [Int]
blocks xs = zipWith3 (\x y z -> x + y + z) ys (init (0:ys)) (tail (ys ++ [0]))
    where ys = map bin xs

lar :: [CellState] -> [(CellState, Int)]
lar xs = zip xs (zipWith (+) (init (0:ys)) (tail (ys ++ [0])))
    where ys = map bin xs

addLst :: (CellState, Int) -> Int -> (CellState, Int)
addLst (x, y) z = (x, y + z)

nextStep :: [CellState] -> [CellState] -> [CellState] -> [(CellState, Int)]
nextStep [] ys zs = zipWith addLst (lar ys) (blocks zs)
nextStep xs ys [] = zipWith addLst (lar ys) (blocks xs)
nextStep xs ys zs = zipWith addLst (lar ys) (zipWith (+) (blocks xs) (blocks zs))

stepCell :: (CellState, Int) -> CellState
stepCell (False, 3) = True
stepCell (False, _) = False
stepCell (True, 2) = True
stepCell (True, 3) = True
stepCell (True, _) = False

-- 遷移関数
next :: [[CellState]] -> [[CellState]]
next prevField = [map stepCell (if x == 0 then nextStep [] (prevField !! x) (prevField !! (x + 1)) else if x == size - 1 then nextStep (prevField !! (x - 1)) (prevField !! x) [] else nextStep (prevField !! (x - 1)) (prevField !! x) (prevField !! (x + 1))) | x <- [0..size - 1]]

-- 色付けのルール
cellStateToColor :: CellState -> Color
cellStateToColor model = c
  where
    c = if model then red else white
