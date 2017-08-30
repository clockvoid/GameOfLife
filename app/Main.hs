module Main where

import View
import Graphics.Gloss
import Data.List

fps :: Int
fps = 1

size :: Int
size = 40

main :: IO ()
main = run size fps initialState next cellStateToColor

-- 状態の型
type CellState = Bool

-- 初期状態
initialState :: [[CellState]]
initialState = (replicate size . replicate size) False

-- 遷移関数
next :: [[CellState]] -> [[CellState]]
next prevField = nextField
  where
    nextField = (map . map) not prevField

-- 色付けのルール
cellStateToColor :: CellState -> Color
cellStateToColor model = c
  where
    c = if model then red else white
