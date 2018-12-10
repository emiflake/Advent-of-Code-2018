module ActualSolve where

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Control.Monad
import           Data.List
import           Data.Ord
import           Data.Char

parse' :: String -> Map.Map Char (Set.Set Char)
parse' input = Map.fromListWith Set.union $ concat
    [[(line !! 5, Set.empty), (line !! 36, Set.singleton $ line !! 5)] | line <- lines input]

day7a :: String -> String
day7a = loop . parse'
  where loop deps = case Map.lookupMin $ Map.filter Set.null deps of
            Just (k, _) -> k : loop (Map.map (Set.delete k) $ Map.delete k deps)
            _ | Map.null deps -> []

day7b :: Int -> Int -> String -> Int
day7b cost workers = ap loop (map (start 0) . take workers . Map.keys . Map.filter Set.null) . parse'
  where start t c = (t + cost + ord c - ord 'A' + 1, c)
        loop deps ((t, k):ready)
          | Map.null deps' && null ready' = t
          | otherwise = loop deps' ready'
          where deps' = Map.map (Set.delete k) $ Map.delete k deps
                ok k a = k `notElem` map snd ready && Set.null a
                pending = map (start t) $ Map.keys $ Map.filterWithKey ok deps'
                ready' = sort $ ready ++ take (workers - length ready) pending