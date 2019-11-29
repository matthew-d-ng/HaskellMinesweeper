# HaskellMinesweeper

[//]: # (To view this README nicely formatted visit https://github.com/matthew-d-ng/HaskellMinesweeper)

## Mines.hs
Implements the base features of the game minesweeper.

### Types and Data
#### Minefield :: [[Tile]]
A minefield is represented as a 2D list of Tiles. It would have been sufficient to place all tiles into a simple 1D list. However, the 2D list eases indexing and makes sense when visualising the board as a grid.

Tile {status :: Status, square :: Square, coords :: Coords}

Coords :: (Int, Int)

Status = Visible | Unknown | Flagged | Potential

Square = Mine | Empty

### makeMinefield :: Int -> Int -> IO Minefield
Takes a height and a width and creates a randomised board fitting to those dimensions.

### gameWon, gameLost :: Minefield -> Bool
The win condition is defined as all Tiles with an Empty (non-mine) Square are uncovered.
The lose condition is defined as having uncovered a mine.

### uncoverTile, flagTile, potentialTile :: Minefield -> Coords -> Minefield
#### uncoverTile
Not only does function change the status of the tile at the given coordinate to Visible, but it also proceeds to recursively "clear" the surrounding tiles until convergence. A tile is "cleared" under the condition that it is adjacent to another tile that is both visible and not adjacent to any mines itself. This allows a player to create openings when playing the game.

#### flagTile, potentialTile
These simply change the status of the tile at the given coordinate to either Flagged or Potential respectively.


## Solver.hs
Implements a basic solver for a minesweeper board.

### computerMove :: Minefield -> Minefield
Determines obvious mines and obvious empty squares. Initially, I had it so that it preferred to uncover a tile first. However, I swapped the order, as planting a flag first makes it seem more "human".

If nothing can be obviously be determined, there are two cases. Firstly, it may be our initial move, in which case it chooses a tile in the middle of the board. This gives it the best chance of acquiring an opening. Otherwise, it is simply stuck, in which case it attempts to guess. It orders it's best guesses in terms of the sum of touching tiles with numbers. This is not exactly the most correct method, but locally determining probabilities in minesweeper is said to be often incorrect. It is said that a general rule of thumb is that a 50/50 tile touching a bigger number has a higher probability than one that doesn't. This usually works alright, but it should be noted that the computer is unable to spot any complex patterns.

### determineMines :: Minefield -> [Tile]
Starting with an empty list of "known" mines, we proceed to recursively deduce mines, passing our learned information back into our deductor until convergence is reached and we learn nothing more. We deduce the location of a mine based on a simple premise: If the number of unknown adjacent tiles plus the number of known adjacent mines is equal to the number on the tile, then the unknown tiles are also mines, as they couldn't possibly be anywhere else.

### determineEmpties :: Minefield -> [Tile] -> [Tile]
Unlike determineMines, we do not perform this recursively. We take as input the list of mines we have deduced from the above function. We then proceed to apply the following premise to each visible tile: If the amount of known mines adjacent to the tile is equal to the tile's number, then any other adjacent hidden tile must be empty.

## Main.hs
Implements a GUI for the minesweeper game using the reactive module of Threepenny-GUI.

### minefield :: Behavior (Minefield)
The board is drawn every time a change is detected in this value. Constructed with an initial value of our newly created minefield, and modified by the event constructed through a union between a player move and a computer move.

### makeMove :: Behavior (Modes -> (Int, Int) -> Minefield -> Minefield)
A time varying function which holds a different (Minefield -> Minefield) based on the position of the mouse when it clicks the canvas and of the mode that the program is in.

```haskell
let
    coord :: Event (Int, Int)
    coord = getMinePos <$> UI.mousedown canvas

-- makeMove :: Behavior (Modes -> (Int, Int) -> Minefield -> Minefield)
makeMove <- let
    init = \mode -> case mode of
        Mine   -> flip uncoverTile
        Flag   -> flip flagTile
        Unsure -> flip potentialTile
    next = init <$ UI.click canvas
    in stepper init next

-- diggingMode :: Behavior (Modes)
diggingMode <- accumB Mine modeEvent

let
    anyMove :: Event (Minefield -> Minefield)
    anyMove = -- Applying coord event and digging behavior to makeMove
        let player   = (makeMove <*> diggingMode) <@> coord
            computer = computerMove <$ UI.click compButton
        in unionWith const player computer

minefield <- accumB m anyMove
```
