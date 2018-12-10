module Solve where

import           Data.Char
import Control.Monad

foldPass :: String -> String
foldPass = foldr step []
    where step x (y:ys) | x /= y && toLower x == toLower y = ys
          step x ys                                        = x : ys

solve :: String -> IO ()
solve path = do
    f <- readFile path

    putStrLn " ---- PART ONE ---- "
    print . length . foldPass $ f

    putStrLn " ---- PART TWO ---- "
    putStrLn "Pick the smallest!!!!:"
    let xs = map (length . foldPass) [ filter ((&&) <$> (toLower b /=) <*> (toUpper b /=)) f | b <- ['a'..'z'] ] 
    print xs
    putStrLn "Which is... "
    print . minimum $ xs

pad :: Int -> a -> [a] -> [a]
pad n v xs | len < n = (take (n - len) $ repeat v) ++ xs
           | otherwise = xs
    where len = length xs


addZeroLessThanTen :: Int -> String
addZeroLessThanTen = pad 2 '0' . show