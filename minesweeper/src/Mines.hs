{-# LANGUAGE FlexibleInstances #-}

module Mines
    ( Minefield
    , Tile (status, square, coords)
    , Status (Visible, Unknown, Flagged, Potential)
    , displayTile
    , makeMinefield
    , gameWon
    , gameLost
    , adjacentMines
    , uncoverTile
    , flagTile
    , potentialTile
    , tileAt
    , setAllVisible -- debugging only
    ) where

import System.Random
import Data.Char
import Data.List

type Coords = (Int, Int)
type Minefield = [[Tile]]

data Status = Visible | Unknown | Flagged | Potential deriving Eq
data Square = Mine | Empty deriving Eq
data Tile = Tile 
    { status :: Status
    , square :: Square
    , coords :: Coords
    } deriving Eq


-- GAME SETTINGS
mHeight = 20
mWidth = 20
numMines = 50

-- DEBUGGING UTILS
testMines = [
    (0, 1), (4, 6), (8, 2), (9, 8), (2, 4), 
    (6, 6), (1, 3), (9, 9), (7, 0), (2, 2),
    (1, 2), (4, 2), (9, 0), (6, 1), (8, 4)
    ]

-- Make every tile visible
setAllVisible :: Minefield -> Minefield
setAllVisible m = setTilesVisible m (concat m)

setTilesVisible :: Minefield -> [Tile] -> Minefield
setTilesVisible m []     = m
setTilesVisible m (t:ts) = setTilesVisible (revealTile m t) ts

-- SHOW MINEFIELD
instance {-# OVERLAPS #-} Show Minefield where
    show = displayMinefield

displayMinefield :: Minefield -> String
displayMinefield m = displayGrid m m

displayGrid :: Minefield -> [[Tile]] -> String
displayGrid _ []     = ""
displayGrid m (g:gs) =
    let row = displayRow m g
        rest = displayGrid m gs
    in row ++ "\n" ++ rest

displayRow :: Minefield -> [Tile] -> String
displayRow _ []     = ""
displayRow m (t:ts) = case status t of
    Unknown   -> " _ " ++ (displayRow m ts)
    Flagged   -> " ! " ++ (displayRow m ts)
    Potential -> " ? " ++ (displayRow m ts)
    Visible   -> (displayTile m t) ++ (displayRow m ts)

displayTile :: Minefield -> Tile -> String
displayTile m t = case square t of
    Mine  -> " X "
    Empty -> case adjacentMines m t of
        0 -> " - "
        n -> " " ++ [intToDigit n] ++ " "


-- INITIALISE MINEFIELD
-- Create 2D list of tiles with randomly placed mines
makeMinefield :: Int -> Int -> IO Minefield
makeMinefield h w = do
    let m = emptyMinefield h w
    c <- generateMines h w numMines
    return $ fillMines m c

-- Create 2D list of empty tiles
emptyMinefield :: Int -> Int -> Minefield
emptyMinefield 0 w = []
emptyMinefield h w = (emptyMinefield (h-1) w) ++ [createEmptyRow (h-1) w]

-- Create 1D list of empty tiles
createEmptyRow :: Int -> Int -> [Tile]
createEmptyRow i 0 = [] 
createEmptyRow i n =
    let t = Tile{status = Unknown, square = Empty, coords = (i, n-1)}
    in (createEmptyRow i (n-1)) ++ [t]

-- Generate list of mines
generateMines :: Int -> Int -> Int -> IO([Coords])
generateMines h w 0 = return []
generateMines h w n = do
    x <- randomRIO (0, h-1)
    y <- randomRIO (0, w-1)
    rest <- generateMines h w (n-1)
    return $ (x, y):rest

-- Fill minefield with list of Mines
fillMines :: Minefield -> [Coords] -> Minefield
fillMines m []     = m
fillMines m (c:cs) = fillMines (addMine m c) cs

-- Add mine to minefield
addMine :: Minefield -> Coords -> Minefield
addMine m coord = 
    let t = Tile{status = Unknown, square = Mine, coords = coord}
    in setTileAt m coord t


-- GAME RULES
-- Game is won if all tiles without mines are visible
gameWon :: Minefield -> Bool
gameWon m = all rowDone m

rowDone :: [Tile] -> Bool
rowDone row = 
    let x = filter (\t -> not $ (isVisible t) || (isMine t)) row
    in length x == 0

-- We've lost if we step on a mine
gameLost :: Minefield -> Bool
gameLost m = any rowLost m

rowLost :: [Tile] -> Bool
rowLost row = any tileLost row

tileLost :: Tile -> Bool
tileLost t = (isVisible t) && (isMine t)


-- MAKE TILE VISIBLE AND CLEAR AREA AROUND IT - CALLABLE
uncoverTile :: Minefield -> Tile -> Minefield
uncoverTile m t = clear $ revealTile m t

-- keep clearing until convergence
clear :: Minefield -> Minefield
clear m
    | m == m'   = m'
    | otherwise = clear m'
    where m' = clear' m

clear' :: Minefield -> Minefield
clear' m =
    let toBeCleared = filter (clearedSpace m) (concat m)
    in clearTiles m toBeCleared

clearTiles :: Minefield -> [Tile] -> Minefield
clearTiles m [] = m
clearTiles m (t:ts) = 
    let m' = revealTile m t
    in clearTiles m' ts

-- Is tile is adjacent to a visible tile not touching any mines?
clearedSpace :: Minefield -> Tile -> Bool
clearedSpace m t = 
    let visAdj = filter isVisible (adjacents m t)
    in any (==0) (map (adjacentMines m) visAdj)


-- MAKE TILE VISIBLE - NOT CALLABLE
revealTile :: Minefield -> Tile -> Minefield
revealTile m t = setTile m t t{status=Visible}

-- MAKE TILE FLAGGED - CALLABLE
flagTile :: Minefield -> Tile -> Minefield
flagTile m t = setTile m t t{status=Flagged}

-- MARK TILE AS POTENTIALLY A MINE - CALLABLE
potentialTile :: Minefield -> Tile -> Minefield
potentialTile m t = setTile m t t{status=Potential}


-- SET TILE AT LOCATION
setTile :: Minefield -> Tile -> Tile -> Minefield
setTile m t1 t2 = setTileAt m (coords t1) t2

setTileAt :: Minefield -> Coords -> Tile -> Minefield
setTileAt [] _ _ = []
setTileAt (m:ms) coord t = case coord of
    (0, j) -> (setTileRow m t j):ms
    (i, j) -> m:(setTileAt ms (i-1, j) t)

setTileRow :: [Tile] -> Tile -> Int -> [Tile]
setTileRow [] _ _   = []
setTileRow (x:xs) t 0 = t:xs
setTileRow (x:xs) t j = x:(setTileRow xs t (j-1))

-- UTILITY FUNCTIONS
inRange :: Minefield -> Coords -> Bool
inRange m (i, j) = 
    let len = length m 
    in i >= 0 && i < len && j >= 0 && j < len

--tileAt :: Minefield -> Coords -> Maybe Tile
--tileAt m (i, j)
--    | inRange m (i, j) = Just $ m !! i !! j
--    | otherwise        = Nothing

-- Unsafe
tileAt :: Minefield -> Coords -> Tile
tileAt m (i, j) = m !! i !! j

adjacents :: Minefield -> Tile -> [Tile]
adjacents [] _     = []
adjacents (m:ms) t = (filter (isBeside t) m) ++ (adjacents ms t)

adjacentMines :: Minefield -> Tile -> Int
adjacentMines m t = length $ filter (isBeside t)(mines m)

mines :: Minefield -> [Tile]
mines []     = []
mines (m:ms) = (filter isMine m) ++ (mines ms)

isBeside :: Tile -> Tile -> Bool
isBeside t1 t2 = 
    abs (i-k) <= 1 && abs (j-l) <= 1 && (i /= k || j /= l)
    where
        (i, j) = coords t1
        (k, l) = coords t2

isVisible :: Tile -> Bool
isVisible Tile{status = Visible} = True
isVisible _                      = False

isMine :: Tile -> Bool
isMine Tile{square = Mine} = True
isMine _                   = False
