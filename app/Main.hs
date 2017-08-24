module Main where

import View
import Graphics.Gloss
import Data.List

main :: IO ()
main = display (InWindow "Game of Life" (round fieldWidth + 50, round fieldWidth + 50) (10, 10)) white $ pictures [render (replicate 20 (replicate 20 red)), backGround]
