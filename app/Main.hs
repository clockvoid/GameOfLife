module Main where

import View
import Graphics.Gloss
import Data.List

main :: IO ()
main = simulate (InWindow "Game of Life" (round fieldWidth + 50, round fieldWidth + 50) (10, 10)) white 1 [replicate 40 red] render (\v n f -> f ++ [replicate 40 red])
