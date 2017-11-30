{-# LANGUAGE PatternGuards #-}

module View (
  run
) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Data.Maybe (maybe)

type Field = [[Color]]

data State = State (Bool, Maybe [(Int, Int)]) [[Bool]]

cellSize :: Float
cellSize = 10

bgColor :: Color
bgColor = white

fieldToState :: [[Bool]] -> State
fieldToState field = State (False, Nothing) field

windowName :: String
windowName = "Cell Automaton"

nextField :: ([[Bool]] -> [[Bool]]) -> Float -> State -> State
nextField next num state
  | State (False, Nothing) ss <- state
  , _ <- num
  , _ <- next
  = fieldToState $ next ss

  | otherwise
  = state

run :: Int -> Int -> [[Bool]] -> ([[Bool]] -> [[Bool]]) -> (Bool -> Color) -> IO ()
run size fps initialState next stateToColor = play window bgColor fps (State (False, Nothing) initialState) renderState (handleEvent s) (nextField next)
  where
    s = (fromInteger . toInteger) size
    window = createWindow s windowName
    renderState = (render s) . (map . map) stateToColor. (pathToField size)

boolToInt :: Bool -> Color
boolToInt b = if b then red else white

cutArray :: Int -> [a] -> [[a]] -> [[a]]
cutArray size [] xs = xs
cutArray size ys xs = cutArray size (drop size ys) ((take size ys):xs)

xor x y = (x || y) && (not (x && y))

pathToField :: Int -> State -> [[Bool]]
pathToField size state
  | State (_, Just ps) ss <- state
  = cutArray size (map (flip elem ps) [(x, y) | x <- [0..(size - 1)], y <- [0..(size - 1)]]) []
  -- (zipWith xor (concat ss) (map (flip elem ps) [(x, y) | x <- [0..(size - 1)], y <- [0..(size - 1)]])) []

  | State (_, Nothing) ss <- state
  = ss

convertX :: Float-> Float -> Int
convertX size num = round $ ((size * cellSize) / 2 - num) / cellSize

convertY :: Float -> Float -> Int
convertY size num = round $ ((size * cellSize) / 2 - num) / cellSize

handleEvent :: Float -> Event -> State -> State
handleEvent size event state
  | EventMotion (x, y) <- event
  , State (True, Just ps) ss <- state
  = State (True, Just ((convertX size x, convertY size y):ps)) ss

  | EventKey (MouseButton LeftButton) Down (Modifiers Down Up Up) pt@(x,y) <- event
  , State (False, Nothing) ss       <- state
  = State (True, Just [(convertX size x, convertY size y)]) $ cutArray (round size) [False | x <- [0..(round size) - 1], y <- [0..(round size) - 1]] []

  | EventKey (MouseButton LeftButton) Down (Modifiers Down Up Up) pt@(x,y) <- event
  , State (_, Just ps) ss       <- state
  = State (True, Just ((convertX size x, convertY size y):ps)) ss

  | EventKey (MouseButton LeftButton) Up (Modifiers Down Up Up) pt@(x,y)      <- event
  , State (True, Just ps) ss    <- state
  = State (False, Just ((convertX size x, convertY size y):ps)) $ pathToField (round size) state
  
  | EventKey (MouseButton LeftButton) Up (Modifiers Up Up Up) pt@(x,y)      <- event
  , State (_, Just ps) ss    <- state
  = State (False, Nothing) $ pathToField (round size) state
  
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

render :: Float -> Field -> Picture
render s field = pictures (map (cell s) (coordinate s field) ++ [backGround s])

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
