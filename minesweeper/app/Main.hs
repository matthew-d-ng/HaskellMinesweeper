{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Web.Scotty as S
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import Mines

main :: IO ()
--main = S.scotty 3000 $ do
--    S.get "/" $ do
--        homepage
main = startGame

homepage = S.html . renderHtml  $ do
    H.head $ do
        H.title "Balkan Simulator"
    H.body $ do 
        H.h1 "Minesweeper"

startGame :: IO ()
startGame = do
    m <- makeMinefield
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
