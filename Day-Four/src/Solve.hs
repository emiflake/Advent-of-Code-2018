{-# LANGUAGE LambdaCase #-}
module Solve where

import           NanoParsec
import           Control.Applicative
import qualified Data.Map.Strict as Map
import           Data.List
import           Data.Ord

-- |Represents a guard's unique identifier
type ID = Int

data Time = Time { year   :: Int
                 , month  :: Int
                 , day    :: Int
                 , hour   :: Int
                 , minute :: Int } deriving (Show, Eq, Ord)

data GuardAction = BeginShift { guardID :: ID } | FallAsleep | WakeUp deriving (Eq, Show)
data LogMessage = LogMessage { time :: Time , action :: GuardAction } deriving (Eq, Show)

surrounded :: (String, String) -> Parser a -> Parser a
surrounded (before, after) p = string before >> p >>= \v -> string after >> pure v 

timeParser :: Parser Time
timeParser =
    Time <$> number
         <*> (char '-' >> number) 
         <*> (char '-' >> number) 
         <*> (space >> number) 
         <*> (char ':' >> number)

actionParser :: Parser GuardAction
actionParser =
    parseShift <|> (string "falls asleep" >> pure FallAsleep) <|> (string "wakes up" >> pure WakeUp)
    where parseShift :: Parser GuardAction
          parseShift = do
            _ <- string "Guard #"
            id <- number 
            _ <- string " begins shift"

            pure $ BeginShift id

messageParser :: Parser LogMessage
messageParser =
    LogMessage <$> surrounded ("[", "]") timeParser <*> (space >> actionParser)

-- |Represenst a shift, that a guard undertakes
type Shift = (ID, Time, [LogMessage])

-- |Convert the actions into groups of shifts WARNING: must be sorted in order to work!
separateShifts :: [LogMessage] -> [Shift]
separateShifts [] = []
separateShifts (x:xs) = (guardID $ action x, time x, takeWhile f xs) : separateShifts (dropWhile f xs)
    where f = \case { LogMessage _ (BeginShift _) -> False ; _ -> True }

-- |Convert shift into how many minutes the guard is asleep during it
shiftToMinutes :: Shift -> Bool -> Int
shiftToMinutes (id, _, []) _ = 0 
shiftToMinutes (id, startTime, x:xs) asleep | asleep    = d + shiftToMinutes (id, time x, xs) fallsAsleep
                                            | otherwise = shiftToMinutes (id, time x, xs) fallsAsleep
    where d = timeToMinutes $ deltaTime startTime (time x)
          fallsAsleep = action x == FallAsleep


-- |Get all times a guard is asleep
shiftToTimes :: Shift -> Bool -> [Time]
shiftToTimes (id, _, []) _ = [] 
shiftToTimes (id, startTime, x:xs) asleep | asleep    = ntimes ++ shiftToTimes (id, time x, xs) fallsAsleep
                                          | otherwise = shiftToTimes (id, time x, xs) fallsAsleep
    where d = timeToMinutes $ deltaTime startTime (time x)
          ntimes = map (addTimes startTime . Time 0 0 0 0) [0..(pred d)]
          fallsAsleep = action x == FallAsleep

-- |Convert shifts into a Map from ID to Int, counting how many minutes each guard is asleep
shiftsToMinutes :: [Shift] -> Map.Map ID Int
shiftsToMinutes [] = Map.empty
shiftsToMinutes (x@(id, _, _):xs) = Map.alter (\case { Just v -> Just $ v + shift
                                                     ; Nothing -> Just shift }) id
                                        (shiftsToMinutes xs)
    where shift = x `shiftToMinutes` False


-- |Nullify all fields besides the minute field
onlyMinutes :: [Time] -> [Time]
onlyMinutes [] = []
onlyMinutes (Time _ _ _ _ m:xs) = Time 0 0 0 0 m:onlyMinutes xs 

-- |Difference between two times, using b - a
deltaTime :: Time -> Time -> Time
deltaTime (Time by bm bd bh bm') (Time ay am ad ah am') = Time (ay - by) (am - bm) (ad - bd) (ah - bh) (am' - bm')
 
-- |Add two times, using cross addition, WARNING: does not simplify
addTimes :: Time -> Time -> Time
addTimes (Time by bm bd bh bm') (Time ay am ad ah am') = Time (ay + by) (am + bm) (ad + bd) (ah + bh) (am' + bm')
 
-- |Converts a Time to minutes, verbatim
timeToMinutes :: Time -> Int
timeToMinutes (Time y m d h m') = y * 365 * 31 * 24 * 60 + m * 31 * 24 * 60 + d * 24 * 60 + h * 60 + m'

-- |Check if the shift is by a guard
shiftIsGuard :: ID -> Shift -> Bool
shiftIsGuard gid (sid, time, xs) = gid == sid

-- |Get the maximum, ordering by a specific Ord derived element
maxOn :: Ord a => (c -> a) -> [c] -> c
maxOn f = head . sortOn (Down . f)

-- |Finds the minute a guard sleeps most often during their shifts
mostSleepyMinute :: ID -> [Shift] -> [(Int, Int)] 
mostSleepyMinute guard = sortOn (Down . fst) . map (\g -> (length g, head g)) . group . sort . map timeToMinutes . onlyMinutes . concatMap (`shiftToTimes` False) . filter (shiftIsGuard guard)



unique :: Eq a => [a] -> [a]
unique xs = go xs []
    where go [] c = c
          go (x:xs) c | x `elem` c = go xs c
                      | otherwise  = go xs (x:c)

-- |Solves part 1 and part 2 of Day4 of Advent of Code!
solve :: String -> IO ()
solve pathname = do
    log <- readFile pathname

    let actions = sortOn time $ map (runParser messageParser) . lines $ log
    let shifts = separateShifts actions
    -- Get all unique guard IDs
    let allGuards = unique . map ((\case { BeginShift x -> x }) . action) . filter ((\case { BeginShift _ -> True ; _ -> False}) . action) $ actions


    let sleeper = fst . maxOn snd . Map.assocs . shiftsToMinutes $ shifts


    putStrLn " ---- PART ONE ---- "

    putStr "Guard who sleeps most: "
    putStrLn . ("#" ++) . show $ sleeper

    
    let sleepyMinute = snd . head $ mostSleepyMinute sleeper shifts
    putStr $ "Guard #" ++ show sleeper ++ " sleeps most at "
    print sleepyMinute

    putStrLn $ "So, the result is: " ++ show (sleepyMinute * sleeper) 

    putStrLn " ---- PART TWO ---- "
    let (guard, (freq, minute)) = head . sortOn (Down . fst . snd).  map (\(a, b) -> (a, head b)). filter ((/=0) . length . snd) . map (\g -> (g, mostSleepyMinute g shifts)) $ allGuards
    putStrLn $ "Guard #" ++ show guard ++ " slept the most frequently on one minute"
    putStrLn $ "Guard #" ++ show guard ++ " slept a total of " ++ show freq ++ " times on " ++ show minute
    putStrLn $ "So, the result is: " ++ show (guard * minute)