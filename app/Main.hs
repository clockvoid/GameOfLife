module Main where

import View
import Graphics.Gloss
import Data.List

main :: IO ()
main = display (createWindow "Game of Life") white $ render initialState

initialState = replicate 20 (replicate 20 red)
