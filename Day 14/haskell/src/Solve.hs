{-# LANGUAGE OverloadedLists, NamedFieldPuns, MultiWayIf, ViewPatterns  #-}
module Solve where

import           Data.Sequence (Seq, (!?), (><))
import qualified Data.Sequence as Seq

import Data.Maybe
import Data.Foldable
import Data.List

type Recipes = Seq Int

data State = State { curr :: !(Int, Int)
                   , recipes :: !Recipes }
                   deriving Show

startState :: State
startState = State (0, 1) [3, 7]

pushRecipes :: State -> [Int] -> State
pushRecipes s@State{recipes} moreRecipes = s{recipes = recipes >< Seq.fromList moreRecipes}


step :: State -> ([Int], State)
step State{ curr=(a, b), recipes } = (digits, State (newA, newB) newRecipes)
    where recipeA = recipes `Seq.index` a
          recipeB = recipes `Seq.index` b
          digits = getDigits (recipeA + recipeB)
          newRecipes = recipes >< Seq.fromList digits
          newA = (a + recipeA + 1) `mod` length newRecipes
          newB = (b + recipeB + 1) `mod` length newRecipes

getDigits :: Int -> [Int]
getDigits ((`divMod` 10) -> (x,y))
    | x == 0    = [y]
    | otherwise = [x,y]


allRecipes :: [Int]
allRecipes = 3 : 7 : go startState
    where go (step -> (out, state)) = out ++ go state


match :: Recipes -> [Int] -> Bool
match _ [] = True
match (sqs Seq.:|> r) (x:xs) | x == r = match sqs xs
                             | otherwise = False

findPattern :: [Int] -> [Int] -> Int
findPattern xs = length
               . takeWhile (\tail -> not (xs `isPrefixOf` tail))
               . tails

solve :: Int -> IO ()
solve input = do
    putStrLn " ---- PART ONE ---- "
    putStrLn (concatMap show . take 10 . drop input $ allRecipes)
    putStrLn " ---- PART ONE ---- "
    print $ map (\ x -> read [x] :: Int) (show input) `findPattern` allRecipes
