
{-# LANGUAGE TupleSections, LambdaCase, BangPatterns #-}
module Solve where

import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Data.Either
import Data.Ord
import Data.List
import Data.Maybe
import Data.Foldable

type Pos = (Int, Int)
type Locus = (String, Pos)

x' (x, _) = x
y' (_, y) = y 

addPos (ax, ay) (bx, by) = (ax + bx, ay + by) 

parse :: String -> [Pos]
parse = rights . map (runParser posParser () "") . lines
    where posParser :: Parser Pos
          posParser = do
            x <- read <$> many1 digit
            _ <- string ", "
            y <- read <$> many1 digit

            pure (x, y)

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

getClosest :: Pos -> [Locus] -> Maybe Locus
getClosest !pos !loci = 
       sortOn (manhattanDistance pos . snd) loci
    |> groupBy (\(_, a) (_, b) -> manhattanDistance pos a == manhattanDistance pos b)
    |> head
    |> \case 
        [x] -> Just x
        _   -> Nothing

manhattanDistance :: Pos -> Pos -> Int
manhattanDistance (ax, ay) (bx, by) = abs (bx - ax) + abs (by - ay)

maxBy f = head . sortOn (Down . f)
minBy f = head . sortOn f

type Bounds = (Pos, Pos, Pos, Pos)

getBounds :: [Pos] -> Bounds
getBounds xs = (minBy x' xs, maxBy x' xs, minBy y' xs, maxBy y' xs)


-- STATUS: TODO
getBoundsOvershoot :: [Pos] -> Bounds
getBoundsOvershoot xs = ( addPos (-overshot,   0) $ minBy x' xs
                        , addPos ( overshot,   0) $ maxBy x' xs
                        , addPos (  0, -overshot) $ minBy y' xs
                        , addPos (  0,  overshot) $ maxBy y' xs)
            where overshot = 1


inBounds :: Pos -> Bounds -> Bool
inBounds (x, y) ((ax, _), (bx, _), (_, cy), (_, dy))  = x > ax && x < bx && y > cy && y < dy


-- STATUS: TODO
generateAllPos :: Bounds -> [Pos]
generateAllPos ((ax, _), (bx, _), (_, cy), (_, dy)) = [ (x, y) | x <- [ax..bx], y <- [cy..dy]]

alphs :: [String]
alphs = tail $ (inits . repeat) ['A'..'Z'] >>= sequence

named :: [Pos] -> [Locus]
named = zip alphs

closestLoci :: Bounds -> [Locus] -> [Pos] -> [Locus]
closestLoci !bounds !loci !pos =
    pos
    |> mapMaybe (\l -> if l `inBounds` bounds then l `getClosest` loci else Nothing) 
    |> sortOn fst

-- solve :: String -> IO ()
solve path = do
    f <- readFile path

    let loci = Solve.parse f

    let namedLoci = named loci

    putStrLn " ---- ALL LOCI ---- "
    putStrLn . unlines . map (\(k,v) -> k ++ ":  " ++ show v) $ namedLoci

    let bounds = getBounds loci
    let allPos = generateAllPos bounds
    
    putStrLn " ---- PART ONE ---- "
    -- STATUS: TODO
    let closests = closestLoci bounds namedLoci allPos
    print . maximum .  map length . groupBy (\a b -> snd a == snd b) $ closests

    putStrLn " ---- PART TWO ---- "
    -- print . filter (\pos -> 10000 < (foldr (\v acc -> acc + manhattanDistance v pos) 0 loci) ) $ allPos

    print . length . filter (\pos -> 10000 > foldr' ((+) . manhattanDistance pos) 0 loci) $ allPos

    pure ()
