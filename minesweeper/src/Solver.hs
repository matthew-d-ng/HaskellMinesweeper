module Solver
    ( computerMove
    ) where

import Mines
import Data.List

-- The computer tries to determine what is definitely a mine,
-- and what is definitely not.
-- Initially, I had it so it preferred to uncover a tile first
-- However, I swapped the order, as planting a flag first makes
-- it seem more "human"
computerMove :: Minefield -> Minefield
computerMove m = 
    let mines   = determineMines m
        noMines = determineEmpty m mines
    in case filter (not . isFlagged) mines of
        (y:ys) -> flagTile m (coords y)
        []     -> case noMines of
            (x:_) -> uncoverTile m (coords x)
            []    -> if not $ gameWon m
                then uncoverTile m (coords $ thinkHarder m)
                else m

-- We begin with no knowledge of any existing mines
determineMines :: Minefield -> [Tile]
determineMines m = getMinesAdjacent m []

-- When deducing the presence of mines, we may miss some things if we need
-- the information we find out later on,
-- so we run it until we don't learn anything new
getMinesAdjacent :: Minefield -> [Tile] -> [Tile]
getMinesAdjacent m t
    | t == t'   = t'
    | otherwise = getMinesAdjacent m t'
    where t' = getMinesAdjacent' m t

-- only considering visible tiles, we're not cheating
-- passing the deduced mines around with foldl so we miss less
getMinesAdjacent' :: Minefield -> [Tile] -> [Tile]
getMinesAdjacent' m mines =
    let visibles = filter isVisible (concat m)
    in foldl (obviousAdjacentMines m) mines (concat m)

-- Minefield -> [confirmed mines] -> tile -> [confirmed mines]
-- this is basically seeing if the number of hidden tiles is precisely
-- the reported number of adjacent mines on the tile: they couldn't be anywhere
-- else in that case
-- we're also taking into account mines we already know about
obviousAdjacentMines :: Minefield -> [Tile] -> Tile -> [Tile]
obviousAdjacentMines m mines t = 
    let amountAdj = adjacentMines m t
        hidden    = filter (\x -> isBeside t x && not (isVisible x)) (concat m)
        unknown   = hidden \\ mines
        adjKnown  = length hidden - length unknown
        minesLeft = amountAdj - adjKnown
    in if length unknown == minesLeft
        then mines ++ unknown
        else mines

-- this is not necessary at all and reduces "efficiency"
-- however I just don't like having duplicates
determineEmpty :: Minefield -> [Tile] -> [Tile]
determineEmpty m = nub . getAdjacentEmpties m

-- only considering visible tiles, we see what adjacent tiles couldn't
-- possible be mines, as the mines are all accounted for
getAdjacentEmpties :: Minefield -> [Tile] -> [Tile]
getAdjacentEmpties m mines = 
    let visibles = filter isVisible (concat m)
    in concatMap (obviousAdjacentEmpties m mines) visibles

-- Again, we're just counting the amount of hidden tiles as well as
-- determined mines. If all mines are accounted for, the leftover hidden
-- tiles are safe.
obviousAdjacentEmpties :: Minefield -> [Tile] -> Tile -> [Tile]
obviousAdjacentEmpties m mines t =
    let amountAdj = adjacentMines m t
        hidden    = filter (\x -> isBeside t x && not (isVisible x)) (concat m)
        unknown   = hidden \\ mines
        adjKnown  = length hidden - length unknown
    in if adjKnown == amountAdj 
        then unknown
        else []

-- If we reach here, we can't figure anything out, so we will guess.
-- If it's the first move we'll just choose the tile in the middle
-- this gives the best chance of a large opening
-- Otherwise we'll try figure out what the best choice is
thinkHarder :: Minefield -> Tile
thinkHarder m = case filter isVisible (concat m) of
    [] -> tileAt m ((length m) `div` 2, (length m) `div` 2)
    ts -> guess m

-- as a general rule of thumb the more bigger numbers it touches the
-- more likely it is to be a mine
guess :: Minefield -> Tile
guess m = 
    let ts = filter isVisible (concat m)
        adjacents = nub $ concatMap (\t -> filter (isBeside t) (concat m)) ts
        adjHidden = (filter (not . isFlagged) adjacents) \\ ts
        ordCounts = sortOn (sumAdj m) adjHidden
    in head ordCounts

sumAdj :: Minefield -> Tile -> Int
sumAdj m t = 
    let ts = filter (\t' -> isVisible t' && isBeside t' t) (concat m)
    in sum $ map (adjacentMines m) ts
