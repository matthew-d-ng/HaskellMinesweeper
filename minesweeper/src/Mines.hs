module Mines
    ( makeMinefield
    , Minefield
    , displayMinefield
    , gameWon
    , gameLost
    , uncoverTile
    , flagTile
    , potentialTile
    , tileAt
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

-- Game settings
mHeight = 10
mWidth = 10
numMines = 20

testMines = [
    (0, 1), (4, 6), (8, 2), (9, 8), (2, 4), 
    (6, 6), (1, 3), (9, 9), (7, 0), (2, 2),
    (1, 2), (4, 2), (9, 0), (6, 1), (8, 4)
    ]

-- debugging
setAllVisible :: Minefield -> Minefield
setAllVisible m = setTilesVisible m (concat m)

setTilesVisible :: Minefield -> [Tile] -> Minefield
setTilesVisible m []     = m
setTilesVisible m (t:ts) = setTilesVisible (revealTile m t) ts

-- Initialise Minefield
emptyMinefield :: Int -> Int -> Minefield
emptyMinefield h w
    | h > 0     = (emptyMinefield (h-1) w) ++ [createEmptyRow (h-1) w]
    | otherwise = []

createEmptyRow :: Int -> Int -> [Tile]
createEmptyRow i n 
    | n > 0 = 
        let t = Tile{status = Unknown, square = Empty, coords = (i, n-1)}
        in (createEmptyRow i (n-1)) ++ [t]
    | otherwise = []

makeMinefield :: Minefield
makeMinefield =
    let m = emptyMinefield mHeight mWidth
        c = testMines --generateMines mHeight mWidth numMines g
    in fillMines m c

--generateMines :: RandomGen g => Int -> Int -> Int -> g -> [Coords]
--generateMines h w n g = do
--    x <- randomR (0, h-1)
--    y <- randomR (0, w-1)
--    rest <- generateMines h w n-1
--    (x, y):rest

fillMines :: Minefield -> [Coords] -> Minefield
fillMines m []     = m
fillMines m (c:cs) = fillMines (addMine m c) cs

addMine :: Minefield -> Coords -> Minefield
addMine m coord = 
    let t = Tile{status = Unknown, square = Mine, coords = coord}
    in setTileAt m coord t

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

-- String representation of Minefield
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

-- Choose a location
uncoverTile :: Minefield -> Tile -> Minefield
uncoverTile m t = clear $ revealTile m t

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

clearedSpace :: Minefield -> Tile -> Bool
clearedSpace m t = 
    let visAdj = filter isVisible (adjacents m t)
    in any (==0) (map (adjacentMines m) visAdj)

-- Change status of Tile
revealTile :: Minefield -> Tile -> Minefield
revealTile m t = setTile m t t{status=Visible}

-- Change status of Tile - Callable by user
flagTile :: Minefield -> Tile -> Minefield
flagTile m t = setTile m t t{status=Flagged}

potentialTile :: Minefield -> Tile -> Minefield
potentialTile m t = setTile m t t{status=Potential}

-- Set new tile at location
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

-- Util
inRange :: Minefield -> Coords -> Bool
inRange m (i, j) = 
    let len = length m 
    in i >= 0 && i < len && j >= 0 && j < len

--tileAt :: Minefield -> Coords -> Maybe Tile
--tileAt m (i, j)
--    | inRange m (i, j) = Just $ m !! i !! j
--    | otherwise        = Nothing

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
