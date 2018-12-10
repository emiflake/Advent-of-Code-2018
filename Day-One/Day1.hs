{-# LANGUAGE LambdaCase #-}
module Day1 where

import Data.List

type Freq = Int
 
parse :: String -> [Freq]
parse = map (\case { ('-':xs) -> negate $ read xs
                   ; (_:xs)   ->          read xs }) . lines

solve1 :: [Freq] -> Freq
solve1 = sum

unique :: Eq a => [a] -> [a]
unique = nub

isUnique :: Eq a => [a] -> Bool
isUnique xs = (unique xs) == xs

duplicate :: Eq a => [a] -> Maybe a
duplicate xs = go xs []
    where go []    seen = Nothing
          go (h:t) seen | h `elem` seen = Just h
                        | otherwise = go t (h:seen)

timeline xs = reverse $ go xs [0]
    where go [] c = c
          go (h:t) (v:vs) = go t (h + v:v:vs)

solve2 :: [Freq] -> Maybe Freq
solve2 xs = duplicate freqs
    where freqs = timeline . take 1000000 . cycle $ xs


main = do
    freqs <- readFile "freq.txt"
    putStrLn "First part: "

    let parsedFreqs = parse freqs

    print . solve1 $ parsedFreqs
    putStrLn "Second part: "
    print . solve2 $ parsedFreqs