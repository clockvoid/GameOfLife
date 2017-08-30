module Main where

import View
import Graphics.Gloss
import Data.List

main :: IO ()
main = simulate (createWindow "Game of Life") white 1 initialState render (\v n f -> f ++ [replicate size red])

initialState = [replicate size red]
