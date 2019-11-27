{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

import Mines
import Solver

mHeight = 20
mWidth = 20
canvasSize = 25 * mWidth

data Modes = Mine | Flag | Unsure deriving Show

main :: IO ()
main = do
    -- m <- makeMinefield mHeight mWidth
    startGUI defaultConfig setup

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

    let
        move :: Event (Minefield)
        move = 
            let canvasClick = UI.click canvas
                compClick   = UI.click compButton
            in minefield <@ unionWith const canvasClick compClick
    
    onEvent move $ \e -> do playerMove e canvas
    return ()

-- Get mine index based on mouse click
getMinePos :: (Int, Int) -> (Int, Int)
getMinePos (x, y) =
    let c = fromIntegral canvasSize :: Float
        w = fromIntegral mWidth     :: Float
        h = fromIntegral mHeight    :: Float
        a = floor $ (fromIntegral x :: Float) / c * w
        b = floor $ (fromIntegral y :: Float) / c * h
    in (a, b)

getTilePos :: (Int, Int) -> (Double, Double)
getTilePos (x, y) =
    let c = fromIntegral canvasSize :: Double
        w = fromIntegral mWidth     :: Double
        h = fromIntegral mHeight    :: Double
        a = (fromIntegral x :: Double) / w * c
        b = (fromIntegral y :: Double) / h * c
    in (a, b)


playerMove :: Minefield -> Element -> UI ()
playerMove m canvas = do
    drawBoard m m canvas
    if (gameLost m) 
        then gameOverMessage canvas 
        else if (gameWon m) 
            then gameWonMessage canvas 
            else return ()

gameOverMessage :: Element -> UI ()
gameOverMessage canvas = return () -- TODO

gameWonMessage :: Element -> UI ()
gameWonMessage canvas = return () -- TODO


drawBoard :: Minefield -> [[Tile]] -> Element -> UI ()
drawBoard _ [] canvas = return ()
drawBoard m (r:rs) canvas = do
    canvas # set' UI.lineWidth 2.0
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
