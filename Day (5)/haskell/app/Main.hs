module Main where

import Solve

main :: IO ()
main = do 
    putStrLn " ---- PART ONE ---- "
    solve "427 players; last marble is worth 70723 points"
    putStrLn " ---- PART TWO ---- "
    solve "427 players; last marble is worth 7072300 points"


