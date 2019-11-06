{-# LANGUAGE OverloadedStrings #-}
module Main where

import Mines
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        html "<head><title>balkan simulator</title></head> <body><h1>Minesweeper</h1></body>"

startGame :: IO ()
startGame = do
    let m = makeMinefield
    makeMove m

makeMove :: Minefield -> IO ()
makeMove m = do
    putStrLn $ displayMinefield m
    putStrLn "Enter Row: "
    in1 <- getLine
    putStrLn "Enter Column: "
    in2 <- getLine
    let move = (read in1, read in2)
        tile = tileAt m move
        m' = uncoverTile m tile
        lost = gameLost m'
        won = gameWon m'
    if not (lost || won)
        then makeMove m'
        else do
            putStrLn "Game has ended."
            putStrLn $ displayMinefield m'
