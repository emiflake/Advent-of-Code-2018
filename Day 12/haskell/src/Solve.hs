{-# LANGUAGE NumDecimals  #-}
module Solve where

import Text.Pretty.Simple

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String

import Data.List
import Data.Ord
import Data.Foldable
import Data.Monoid

data Rule = String :-> Char deriving Show

data Machine = Machine { pots  :: String
                       , rules :: [Rule]
                       , startingAt :: Int } 
                       deriving Show

parseInput :: String -> Either ParseError Machine
parseInput = parse p ""
                where p = do
                        _ <- string "initial state: "
                        pots <- many1 (oneOf "#.")
                        _ <- newline
                        _ <- newline

                        let ruleP = do
                            pots <- many1 (oneOf "#.")
                            _ <- string " => "
                            pot <- oneOf "#."

                            pure $ pots :-> pot

                        rules <- ruleP `sepBy` newline

                        pure $ Machine pots rules 0



match :: String -> Rule -> Bool
match str (matcher :-> _) = str == matcher

findMatch :: String -> [Rule] -> Char
findMatch str rules = case rule of
        Nothing -> '.'
        Just (_ :-> result) -> result
    where rule = find (\rule -> str `match` rule) rules 

slices :: [a] -> Int -> [[a]]
slices xs size = map (\i -> take size $ drop i xs) [0..(length xs - size)]

generateLookup :: String -> [String]
generateLookup s = slices expanded 5
    where expanded = "...." ++ s ++ "...."


next :: Machine -> Machine
next (Machine pots rules s) = Machine (trim newPots) rules newStart
    where lookup  = generateLookup pots
          newPots = map (`findMatch` rules) lookup

          -- Make sure we change the offset based on what we actually moved
          newStart = case newPots of
            ('#':_) -> s - 2
            ('.':'#':_) -> s - 1
            ('.':'.':'.':_) -> s + 1 -- this line ensures that if we somehow 
                                     -- move (->) right, we'll account for that
            _ -> s

trim :: String -> String
trim = reverse . dropWhile (=='.') . reverse . dropWhile (=='.')

sumPots :: Machine -> Int
sumPots (Machine pots _ s) = sum . map fst . filter ((=='#') . snd) $ zip [s..] pots

showMachine :: Machine -> String
showMachine m@(Machine p _ s) =  show s ++ ":\t" ++ show (sumPots m) ++ ":\t" ++ p


solve :: String -> IO ()
solve path = do
    f <- readFile path

    case parseInput f of
        Left err -> print err
        Right machine -> do            
            let iterations = iterate next machine


            putStrLn " ---- PART ONE ---- "
            putStrLn "Generation 20:"
            putStrLn "Offset\tSum\tRepresentation (truncated)"
            putStrLn . take 50 . showMachine $ iterations !! 20

            putStrLn " ---- PART TWO ---- "
            -- part two is trickier, our implementation would *never*
            -- be able to finish all the way to 50 billion 
            -- but we know that at *some* point it'll repeat, 
            -- so we need to find the difference when it does

            
            -- take this slice, for example 100-110
            putStrLn "Slice at 100-110, showing repetition:"
            putStrLn "Offset\tSum\tRepresentation (truncated)"
            putStrLn . unlines . map (take 50 . showMachine) $ take 10 $ drop 100 iterations

            -- This prints
            {-
            Offset  Sum     Representation (truncated)
            59:     2580:   ###.........###..........###............
            60:     2601:   ###.........###..........###............
            61:     2622:   ###.........###..........###............
            62:     2643:   ###.........###..........###............
            63:     2664:   ###.........###..........###............
            64:     2685:   ###.........###..........###............
            65:     2706:   ###.........###..........###............
            66:     2727:   ###.........###..........###............
            67:     2748:   ###.........###..........###............
            68:     2769:   ###.........###..........###............
            -}

            -- as you can see, the only thing changing is the offset
            -- which means, that we can *calculate* the result from here
            
            -- like so:
            let a = iterations !! 1000
            let b = iterations !! 1001

            let diff = sumPots b - sumPots a

            putStrLn "Using random guess at 1000"
            print (sumPots a + diff * (50e9 - 1e3))
            -- safe bet, but we can do better

            -- let's write a function that does it for us
            let deltasAt :: Int -> [Machine] -> (Int, Bool)
                deltasAt offset = (\(x:xs) -> (x, all (==x) xs)) -- fancy way of saying whether they're all 
                                                                 -- equal and returning their delta 
                                . map (\[a, b] -> b - a)
                                . (`slices` 2) -- differences, in slices of 2
                                . map sumPots -- we're looking for differences in the sum
                                . take 10 -- our sample size is 10, which is safe enough
                                . drop offset 

            --
            let findRepeating :: [Machine] -> Maybe (Int, Int)
                findRepeating iterations = fmap (\(value, (delta, _)) -> (value, delta)) 
                                         . find (snd . snd) 
                                         . map (\v -> (v, v `deltasAt` iterations)) $ [0..]

            case findRepeating iterations of
                Just (iteration, delta) -> do
                    putStrLn "Actually finding the pattern of repetition:"
                    print $ sumPots (iterations !! iteration) + delta * (50e9 - iteration)
                Nothing -> putStrLn "Well, we couldn't find a repetition!!!!!!!!"

            -- We did it!

            -- Possible things TODO: Change datastructure to Sequence, maybe, or a Map
