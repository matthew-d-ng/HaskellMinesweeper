{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

import Mines
import Solver

mHeight = 20    :: Int
mWidth = 20     :: Int
canvasSize = 25 * mWidth :: Int

data Modes = Mine | Flag | Unsure deriving Show

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "minesweeper"
    canvas <- UI.canvas
        # set UI.height canvasSize
        # set UI.width canvasSize
        # set UI.style [("border", "solid black 1px"), ("background", "#eee")]

    m <- liftIO $ makeMinefield mHeight mWidth
    drawBoard m m canvas

    header       <- UI.h1     #+ [string "Minesweeper"]
    digButton    <- UI.button #+ [string "Dig Square"]
    flagButton   <- UI.button #+ [string "Plant Flag"]
    unsureButton <- UI.button #+ [string "Mark Unsure"]
    compButton   <- UI.button #+ [string "Make AI move"]

    getBody window #+
        [ column [element canvas]
        , element header
        , element digButton
        , element flagButton
        , element unsureButton
        , element compButton
        ]

    let 
        mineEvent :: Event (Modes -> Modes)
        mineEvent = const Mine <$ UI.click digButton

        flagEvent :: Event (Modes -> Modes)
        flagEvent = const Flag <$ UI.click flagButton

        unsureEvent :: Event (Modes -> Modes)
        unsureEvent = const Unsure <$ UI.click unsureButton

        modeEvent :: Event (Modes -> Modes)
        modeEvent = unionWith const mineEvent (unionWith const flagEvent unsureEvent)

        -- Converts mousePos to the index of the tile clicked
        coord :: Event (Int, Int)
        coord = getMinePos <$> UI.mousedown canvas

    -- makeMove :: Behavior (Modes -> (Int, Int) -> Minefield -> Minefield)
    --   Allows for Event (Minefield -> Minefield) to depend on coords and mode
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

    liftIO $ onChange minefield $ 
        \e -> do runUI window (playerMove window e canvas)
    return ()

-- Get mine index based on mouse click
getMinePos :: (Int, Int) -> (Int, Int)
getMinePos (x, y) =
    let c = fromIntegral canvasSize :: Float
        w = fromIntegral mWidth     :: Float
        h = fromIntegral mHeight    :: Float
        b = floor $ (fromIntegral x :: Float) / c * w
        a = floor $ (fromIntegral y :: Float) / c * h
    in (a, b)

getTilePos :: (Int, Int) -> (Double, Double)
getTilePos (x, y) =
    let c = fromIntegral canvasSize :: Double
        w = fromIntegral mWidth     :: Double
        h = fromIntegral mHeight    :: Double
        b = (fromIntegral x :: Double) / w * c
        a = (fromIntegral y :: Double) / h * c
    in (a, b)


playerMove :: Window -> Minefield -> Element -> UI ()
playerMove w m canvas = do
    drawBoard m m canvas
    if (gameLost m) 
        then gameOverMessage w 
        else if (gameWon m) 
            then gameWonMessage w 
            else return ()

gameOverMessage :: Window -> UI ()
gameOverMessage w = do
    loseMessage <- UI.h1 #+ [string "Game Lost! You hit a mine!"]
    getBody w #+ [element loseMessage]
    return ()

gameWonMessage :: Window -> UI ()
gameWonMessage w = do
    winMessage <- UI.h1 #+ [string "Game complete! All safe squares uncovered!"]
    getBody w #+ [element winMessage]
    return ()

drawGridLines :: Element -> UI ()
drawGridLines canvas = do
    let max       = fromIntegral canvasSize
        ranges    = [0, 25..max]
        top       = zip (repeat 0) ranges
        bottom    = zip (repeat max) ranges
        left      = zip ranges (repeat 0)
        right     = zip ranges (repeat max)
    mapM_ (drawLine canvas) (zip top bottom)
    mapM_ (drawLine canvas) (zip left right)

drawLine :: Element -> (UI.Point, UI.Point) -> UI ()
drawLine canvas (a, b) = do
    UI.beginPath canvas
    UI.moveTo a canvas
    UI.lineTo b canvas
    UI.closePath canvas
    UI.stroke canvas

drawBoard :: Minefield -> [[Tile]] -> Element -> UI ()
drawBoard _ [] canvas = drawGridLines canvas
drawBoard m (r:rs) canvas = do
    canvas # set' UI.lineWidth 1.0
    canvas # set' UI.strokeStyle "gray"
    canvas # set' UI.textFont "14px sans-serif"
    canvas # set' UI.textAlign UI.Center
    drawRow m r canvas
    drawBoard m rs canvas

drawRow :: Minefield -> [Tile] -> Element -> UI ()
drawRow m [] canvas = return ()
drawRow m (t:ts) canvas = do
    let colour = getTileColour (status t)
        rectPos = getTilePos $ coords t
        textPos = let (a, b) = rectPos
                  in (a+12.5, b+14)

    canvas # set' UI.fillStyle (UI.htmlColor colour)
    canvas # UI.fillRect rectPos 25 25
    canvas # set' UI.fillStyle (UI.htmlColor "black")

    case status t of
        Visible   -> canvas # UI.fillText (displayTile m t) textPos
        Flagged   -> canvas # UI.fillText "!" textPos
        Potential -> canvas # UI.fillText "?" textPos
        _         -> return ()

    drawRow m ts canvas

getTileColour :: Status -> String
getTileColour Visible = "LightGoldenrodYellow"
getTileColour _       = "DarkKhaki"
