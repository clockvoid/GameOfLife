{-# LANGUAGE PatternGuards #-}

module View (
  run
) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Data.Maybe (maybe)

type Field = [[Color]]

data State = State (Maybe Path) Field

cellSize :: Float
cellSize = 10

bgColor :: Color
bgColor = white

fieldToState :: Field -> State
fieldToState field = State Nothing field

windowName :: String
windowName = "Cell Automaton"

nextField :: (Field -> Field) -> State -> State
nextField next state
  | State Nothing ss <- state
  , _ <- next
  = fieldToState $ next ss

  | otherwise
  = state

run :: Int -> Int -> [[a]] -> ([[a]] -> [[a]]) -> (a -> Color) -> IO ()
run size fps initialState next stateToColor = play window bgColor fps (State Nothing initialState) renderState $ const . const (nextField next)
  where
    s = (fromInteger . toInteger) size
    window = createWindow s windowName
    renderState = (render s) . (map . map) stateToColor . pathToField

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

cutArray :: [a] -> [[a]] -> [[a]]
cutArray [] xs = xs
cutArray ys xs = cutArray (drop cellSize ys) (take cellSize ys):xs

pathToField :: State -> Field
pathToField state
  | State (Just ps) ss <- state
  = cutArray $ map boolToInt $ map (flip elem ps) [(x, y) | x <- [0..cellSize], y <- [0..cellSize]]

floatToInt :: Float -> Int
floatToInt num = fromIntegral $ floor (num + 400) / (800 / cellSize)

handleEvent :: Event -> State -> State
handleEvent event state
  | EventMotion (x, y) <- event
  , State (Just ps) ss <- state
  = State (Just ((floatToInt x, floatToInt y):ps)) ss

  -- Start drawing a new line.
  | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
  , State Nothing ss       <- state
  = State (Just [(floatToInt x, floatToInt y)]) []
  
  -- Finish drawing a line, and add it to the picture.
  | EventKey (MouseButton LeftButton) Up _ pt@(x,y)      <- event
  , State (Just ps) ss    <- state
  = State Nothing $ pathToField state
  
  | otherwise
  = state

triangles :: Float -> Float -> Float -> [Path]
triangles s x' y' = [[(x, y), (x + (cellSize * s) / s, y), (x, y - (cellSize * s) / s)], [(x + (cellSize * s) / s, y), (x, y - (cellSize * s) / s), (x + (cellSize * s) / s, y - (cellSize * s) / s)]]
  where
    x = x' * (cellSize * s) / s - (cellSize * s) / 2
    y = -1 * y' * (cellSize * s) / s + (cellSize * s) / 2

cell :: Float -> (Float, Float, Color) -> Picture
cell s (x, y, c) = color c (pictures (map polygon (triangles s x y)))

coordinate :: Float -> Field -> [(Float, Float, Color)]
coordinate s field = concat $ map (\(x, cs) -> [(x, fst y, snd y) | y <- zip [0..s - 1] cs]) $ zip [0..s - 1] field

render :: Float -> State -> Picture
render s state
  | _ <- s
  , State _ field <- state
  = pictures (map (cell s) (coordinate s field) ++ [backGround s])

makeVerticalLines :: Float -> Float -> [Picture]
makeVerticalLines s x = if x <= (cellSize * s) / 2
  then [line [(x, (cellSize * s) / 2), (x, -1 * (cellSize * s) / 2)]] ++ makeVerticalLines s (x + (cellSize * s) / s)
  else []

makeHorizontalLines :: Float -> Float -> [Picture]
makeHorizontalLines s y = if y <= (cellSize * s) / 2
  then [line [((cellSize * s) / 2, y), (-1 * (cellSize * s) / 2, y)]] ++ makeHorizontalLines s (y + (cellSize * s) / s)
  else []

backGround :: Float -> Picture
backGround s = pictures $ makeVerticalLines s (-1 * (cellSize * s) / 2) ++ makeHorizontalLines s (-1 * (cellSize * s) / 2)

createWindow :: Float -> String -> Display
createWindow s windowName = InWindow windowName (round (cellSize * s) + 50, round (cellSize * s) + 50) (10, 10)
