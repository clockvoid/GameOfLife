module Main where

import View
import Graphics.Gloss

main :: IO ()
main = display (InWindow "Game of Life" (round fieldWidth + 50, round fieldWidth + 50) (10, 10)) white $ pictures [backGround, cell (0, 0) black]
