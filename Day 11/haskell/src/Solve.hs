{-# LANGUAGE BangPatterns #-}
module Solve where

import Data.Array.Unboxed (UArray, array)
import qualified Data.Array.Unboxed as A

import Data.Foldable
import Data.Ord
import Data.List
import Text.Pretty.Simple
import Control.Monad

type FuelCells = UArray (Int, Int) FuelCell
type FuelCell = Int

powerLevelFor :: Int -> (Int, Int) -> FuelCell
powerLevelFor serialNumber (x, y) = 
    let rackID = x + 10
        powerLevelInitial = ((rackID * y) + serialNumber) * rackID
        hundreds n = (n `div` 100) `mod` 10
     in hundreds powerLevelInitial - 5

extract :: (Int, Int) -> (Int, Int) -> FuelCells -> [FuelCell]
extract (x, y) (w, h) fuelCells = [ fuelCells A.! (xo + x, yo + y)
                                  | xo <- [0..pred w]
                                  , yo <- [0..pred h]  
                                  ]

data Aggregate = Aggregate { pos   :: (Int, Int)
                           , total :: FuelCell
                           , size  :: Int } deriving Show

cellAggregate :: (Int, Int) -> Int -> FuelCells -> Aggregate
cellAggregate (x, y) dim fuelCells = Aggregate topLeft total dim
    where total   = sum $ extract (x, y) (dim, dim) fuelCells
          topLeft = (x, y)


genArray :: Int -> FuelCells 
genArray serialNumber = array ((1, 1), (300, 300)) [ ((x, y), powerLevelFor serialNumber (x, y))
                                                   | x <- [1..300]
                                                   , y <- [1..300]
                                                   ]


findBestFor :: Int -> FuelCells -> Aggregate
findBestFor sz cells = maximumBy (comparing total)
    [ cellAggregate (x, y) sz cells
    | x <- [1..301 - sz]
    , y <- [1..301 - sz]
    ]

findBest :: FuelCells -> Aggregate
findBest cells = maximumBy (comparing total) $ 
    map (`findBestFor` cells) [1..30] -- Assumes all answers are under 30x30, 
                                      -- bad assumption but performs faster as a result

solve :: String -> IO ()
solve s = do
    let serialNumber = read s :: Int


    let !arr = genArray serialNumber

    putStrLn " ---- PART ONE ---- "
    pPrint $ findBestFor 5 arr

    -- putStrLn " ---- PART TWO ---- "
    -- pPrint . findBest $ genArray serialNumber