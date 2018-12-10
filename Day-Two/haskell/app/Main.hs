module Main where

import Data.List
import Data.Maybe
import Data.Bool
import Data.Ord


type ID = String

parse :: String -> [ID]
parse = lines

solve1 :: [ID] -> Int
solve1 ids = let (twos, threes) = unzip . map count $ ids
    in sum twos * sum threes

count :: ID -> (Int, Int)
count id = (lens 2 grouped, lens 3 grouped)
    where grouped = group . sort $ id
          lens n = bool 0 1 . isJust . find ((==n) . length) 


others :: [a] -> [[a]]
others xs = ($ []) . foldr f (const []) $ zip (drop 1 . tails $ xs) xs
    where f (as, a) b = \front -> (front ++ as) : b (front ++ [a])

othersWithSelf :: [a] -> [(a, [a])]
othersWithSelf = zip <$> Prelude.id <*> others

-- solve2 :: [ID] -> [Int]
solve2 ids = uncurry nodifference . fst . head $ sortBy (comparing snd) diffs
    where diffs = concat $ map (\(a, xs) -> 
            map (\b -> ((a, b), diff a b)) xs) 
            $ othersWithSelf ids

nodifference :: Eq a => [a] -> [a] -> [a]
nodifference a b = reverse $ go a b []
        where go [] [] c = c
              go (x:xs) (y:ys) c | x == y    = go xs ys (x:c)
                                 | otherwise = go xs ys c 

diff :: Eq a => [a] -> [a] -> Int
diff [] [] = 0
diff (x:xs) (y:ys) | x == y = diff xs ys
                   | otherwise = 1 + diff xs ys



main :: IO ()
main = do
    txt <- readFile "ids.txt"

    putStrLn "Part one: "
    print . solve1 . parse $ txt

    putStrLn "Part two: "
    print . solve2 . parse $ txt