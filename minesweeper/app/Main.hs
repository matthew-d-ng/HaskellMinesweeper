module Main where

import Mines

main :: IO ()
main = do
    g <- getStdGen
    m <- makeMinefield g
    putStrLn $ displayMinefield m
    getLine <- nextMove
    case nextMove of
