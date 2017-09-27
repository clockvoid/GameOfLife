module View (
  run, 
  red, 
  black,
  backGround,
  Color
) where

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

run :: Int -> Int -> [[a]] -> ([[a]] -> [[a]]) -> (a -> Color) -> IO () -- size -> fps -> initialState -> next -> stateToColor -> IO ()
run size fps initialState next stateToColor = do
  print "test"

drow :: Field -> IO ()
drow field = do 
  clearConsole
  putStr $ concat $ map (++ (backGround ++ "\n")) $ map (foldl (++) "") field 

