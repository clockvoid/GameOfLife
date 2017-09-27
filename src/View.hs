module View (
  run, 
  red, 
  black ,
  backGround
) where

import Control.Monad

type Color = String

type Field = [[Color]]

red :: Color
red = "\x1b[41m"

black :: Color
black = "\x1b[40m"

backGround :: Color
backGround = "\x1b[49m"

clearConsole:: IO ()
clearConsole = putStr "\\e[;H\\e[2J"

block :: String
block = "  "

run :: Int ->  [[a]] -> (a -> a) -> IO () -- fps -> initialState -> next -> IO ()
run fps initialState next = do

draw :: Field -> IO ()
draw field = do 
  clearConsole
  putStr $ concat $ map (++ (backGround ++ "\n")) $ map (foldl (++) "") field 

