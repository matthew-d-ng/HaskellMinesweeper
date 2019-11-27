module Solver
    ( computerMove
    ) where

import Mines
import Data.List
import System.IO.Unsafe

computerMove :: Minefield -> Minefield
computerMove m = 
    let mines   = determineMines m
        noMines = determineEmpty m mines
    in case filter (not . isFlagged) mines of
        (y:ys) -> flagTile m (coords y)
        []     -> case noMines of
            (x:_) -> uncoverTile m (coords x)
            []    -> panic m

determineMines :: Minefield -> [Tile]
determineMines m = getMinesAdjacent m []

getMinesAdjacent :: Minefield -> [Tile] -> [Tile]
getMinesAdjacent m t
    | t == t'   = t'
    | otherwise = getMinesAdjacent m t'
    where t' = getMinesAdjacent' m t

getMinesAdjacent' :: Minefield -> [Tile] -> [Tile]
getMinesAdjacent' m mines =
    let visibles = filter isVisible (concat m)
    in foldl (obviousAdjacentMines m) mines (concat m)

-- Minefield -> [confirmed mines] -> tile -> [confirmed mines]
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

-- this is not necessary at all 
-- and reduces efficiency but it makes me feel better
determineEmpty :: Minefield -> [Tile] -> [Tile]
determineEmpty m = nub . getAdjacentEmpties m

getAdjacentEmpties :: Minefield -> [Tile] -> [Tile]
getAdjacentEmpties m mines = 
    let visibles = filter isVisible (concat m)
    in concatMap (obviousAdjacentEmpties m mines) visibles

obviousAdjacentEmpties :: Minefield -> [Tile] -> Tile -> [Tile]
obviousAdjacentEmpties m mines t =
    let amountAdj = adjacentMines m t
        hidden    = filter (\x -> isBeside t x && not (isVisible x)) (concat m)
        unknown   = hidden \\ mines
        adjKnown  = length hidden - length unknown
    in if adjKnown == amountAdj 
        then unknown
        else []

panic :: Minefield -> Minefield
panic _ = [[]]  -- TODO
