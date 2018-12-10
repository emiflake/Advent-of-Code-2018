{-# LANGUAGE LambdaCase #-}
module Solve where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Data.Either
import Data.Maybe
import Data.List
import Data.Ord
import Control.Monad

type Step = Char

type Requirement = (Step, Step)

type RState = [Step]

stepCost :: Step -> Int
stepCost step = fromMaybe 0 (lookup step (zip ['A'..'Z'] [1..])) + 60 - 1

requirementMet :: RState -> Requirement -> Bool
requirementMet xs (r, _) = r `elem` xs

allRequirementsMet :: RState -> [Requirement] -> Bool
allRequirementsMet xs = all (requirementMet xs)

options :: [Requirement] -> [Step]
options = nub . map snd

requirementsFor :: Step -> [Requirement] -> [Requirement]
requirementsFor s = filter (\ (_, r) -> r == s) 

hasNoRequirements :: [Requirement] -> [Step]
hasNoRequirements reqs =  nub . map fst . filter (\(r, _) -> r `notElem` options reqs) $ reqs 

validOptions :: RState -> [Requirement] -> [Step]
validOptions rstate reqs = sort . filter (\opt -> opt `notElem` rstate && allRequirementsMet rstate (requirementsFor opt reqs)) $ hasNoRequirements reqs ++ options reqs

stepThrough :: RState -> [Requirement] -> RState
stepThrough rstate requirements = case validOptions rstate requirements of 
    [] -> rstate
    (x:_) -> stepThrough (x : rstate) requirements

type Work = (Int, Step)

workers :: Int -> [Maybe Work]
workers n = replicate n Nothing 

allocateWorker :: Step -> [Maybe Work] -> Maybe [Maybe Work]
allocateWorker s workers = case find (==Nothing) workers of
    Nothing -> Nothing
    Just Nothing -> Just $ Just (stepCost s, s) : delete Nothing workers

allocateWorkers :: [Step] -> [Maybe Work] -> ([Maybe Work], [Step])
allocateWorkers [] ws = (ws, [])
allocateWorkers (s:steps) ws = case allocateWorker s ws of
    Nothing -> (ws, s:steps)
    Just nws -> allocateWorkers steps nws
    
alreadyBeingProcessed :: Step -> [Maybe Work] -> Bool
alreadyBeingProcessed step = isJust . find (\case Just (_, s) -> s == step
                                                  Nothing     -> False)

notProcessed :: [Maybe Work] -> [Step] -> [Step]
notProcessed ws = filter (\s -> not $ alreadyBeingProcessed s ws)

stepWorkers :: [Maybe Work] -> ([Step], [Maybe Work])
stepWorkers = foldl (\(steps, workers) v -> case v of 
                        Nothing        -> (steps     , Nothing:workers)
                        Just (0, step) -> (step:steps, Nothing:workers)
                        Just (n, step) -> (steps     , Just (pred n, step):workers)) ([], [])

singleStep' :: [Maybe Work] -> RState -> [Requirement] -> ([Maybe Work], RState)
singleStep' ws rstate reqs = case notProcessed nws $ validOptions nstate reqs of
                                [] -> (nws, nstate)
                                xs -> (fst $ allocateWorkers xs nws, nstate)

    where (nws, nstate) = step ws rstate

step :: [Maybe Work] -> RState -> ([Maybe Work], RState)
step ws rstate = case stepWorkers ws of
                        ([], nws) -> (nws, rstate)
                        (steps, nws) -> (nws, steps ++ rstate)

stepThroughTimed' :: [Maybe Work] -> RState -> [Requirement] -> [([Maybe Work], RState)] 
stepThroughTimed' ws rstate reqs = case validOptions rstate reqs of
    [] | all (==Nothing) ws -> [(ws, rstate)]
    xs -> (\(nws, rstate) -> (nws, rstate) : stepThroughTimed' nws rstate reqs) (singleStep' ws rstate reqs)

parse :: String -> [Requirement]
parse = rights . map (runParser p () "") . lines
    where p = do
            _ <- string "Step "
            req <- oneOf ['A'..'Z']
            _ <- string " must be finished before step "
            prov <- oneOf ['A'..'Z']
            _ <- string " can begin."

            pure (req, prov)

solve :: String -> IO ()
solve path = do    
    f <- readFile path

    let reqs = Solve.parse f

    putStrLn " ---- PART ONE ---- "
    putStrLn "My solve: "
    print . reverse . stepThrough [] $ reqs

    putStrLn " ---- PART TWO ---- "
    putStrLn "My solve: "
    let steps = stepThroughTimed' (workers 5) [] reqs
    print . (\x -> x - 2) . length $ steps

    putStrLn " --- DEBUG --- "
    -- forM_ steps $ \(ws, rs) ->
    --     putStrLn $ "Workers: " ++ show (catMaybes ws) ++ "\t\t| State: " ++ show rs
