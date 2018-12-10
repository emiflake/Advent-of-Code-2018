module Main where

import Solve

main :: IO ()
main = solve "coordinates.txt" >> pure ()
