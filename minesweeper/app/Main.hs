{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Reactive.Threepenny

import Mines

mHeight = 20
mWidth = 20
canvasSize = 25 * mWidth

data Modes = Mine | Flag | Unsure deriving Show

main :: IO ()
main = do
    m <- makeMinefield mHeight mWidth
    startGUI defaultConfig (setup $ setAllVisible m)

setup :: Minefield -> Window -> UI ()
setup m window = do
    return window # set title "Balkan Simulator"
    canvas <- UI.canvas
        # set UI.height canvasSize
        # set UI.width canvasSize
        # set UI.style [("border", "solid black 1px"), ("background", "#eee")]

    drawBoard m m canvas

    mineMode   <- UI.button #+ [string "Dig Mine"]
    flagMode   <- UI.button #+ [string "Plant Flag"]
    unsureMode <- UI.button #+ [string "Mark Unsure"]
    compMove   <- UI.button #+ [string "Make AI move"]
    reset      <- UI.button #+ [string "New Game"]

    getBody window #+
        [ column [element canvas]
        , element mineMode
        , element flagMode
        , element unsureMode
        , element compMove
        , element reset
        ]

    let 
        mineEvent :: Event (Modes -> Modes)
        mineEvent = const Mine <$ UI.click mineMode

        flagEvent :: Event (Modes -> Modes)
        flagEvent = const Flag <$ UI.click flagMode

        unsureEvent :: Event (Modes -> Modes)
        unsureEvent = const Unsure <$ UI.click unsureMode

        modeEvent :: Event (Modes -> Modes)
        modeEvent = unionWith const mineEvent (unionWith const flagEvent unsureEvent)

        digMine :: Event (Minefield -> Minefield)
        digMine tile = flip $ uncoverTile tile

    diggingMode <- accumB Mine modeEvent
    mousePos <- stepper (0, 0) $ UI.mousemove canvas
    minefield <- accumB m digMine tile
        where tile = getTile <$> getMinePos <$> mousePos

    let 
        bst :: Behavior (Modes, (Int, Int))
        bst = (,) <$> diggingMode <*> ( getMinePos <$> mousePos)

        board :: Behaviour (Minefield)
        board = (,) <$> 

        move :: Event (Modes, (Int, Int))
        move = bst <@ UI.click canvas
    
    onEvent move $ \e -> do playerMove e canvas

    on UI.click reset $ const $
        canvas # UI.clearCanvas

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

playerMove :: (Modes, (Int, Int)) -> Element -> UI ()
playerMove (m, pos) canvas = do
    canvas # set' UI.lineWidth 2.0
    canvas # set' UI.strokeStyle "black"

    let colour = case m of
        Mine -> "DarkKhaki"
        _    -> "LightGoldenrodYellow"

    canvas # set' UI.fillStyle (UI.htmlColor "DarkKhaki")
    canvas # UI.fillRect (getTilePos pos) 25 25

drawBoard :: Minefield -> [[Tile]] -> Element -> UI ()
drawBoard _ [] canvas = do canvas # set' UI.strokeStyle "black"
drawBoard m (r:rs) canvas = do
    canvas # set' UI.lineWidth 2.0
    drawRow m r canvas
    drawBoard m rs canvas

drawRow :: Minefield -> [Tile] -> Element -> UI ()
drawRow m [] canvas = do canvas # set' UI.strokeStyle "black"
drawRow m (t:ts) canvas = do
    let colour = getTileColour (status t)
    canvas # set' UI.fillStyle (UI.htmlColor colour)
    canvas # set' UI.text (displayTile m t)
    canvas # UI.fillRect (getTilePos $ coords t) 25 25
    drawRow m ts canvas

getTileColour :: Status -> String
getTileColour Visible = "LightGoldenrodYellow"
getTileColour _       = "DarkKhaki"

-- startGame :: IO ()
-- startGame = do
--     m <- makeMinefield mHeight mWidth
--     makeMove m

-- makeMove :: Minefield -> IO ()
-- makeMove m = do
--     print m
--     putStrLn "Enter Row: "
--     in1 <- getLine
--     putStrLn "Enter Column: "
--     in2 <- getLine
--     let move = (read in1, read in2)
--         tile = tileAt m move
--         m' = uncoverTile m tile
--         lost = gameLost m'
--         won = gameWon m'
--     if not (lost || won)
--         then makeMove m'
--         else do
--             putStrLn "Game has ended."
--             print m'
