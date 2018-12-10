module Main where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Control.Monad
import Data.Maybe
import Control.Parallel

data Tree a = Branch [Tree a] [a] deriving Show

runParse :: Parser a -> String -> Either ParseError a
runParse p = runParser p () ""

getNode :: Parser (Tree Int)
getNode = do
    childrenN <- consumeNumber
    metadataN <- consumeNumber

    children <- if childrenN > 0 
                then replicateM childrenN getNode
                else pure []

    metadata <- replicateM metadataN consumeNumber

    pure $ Branch children metadata

consumeNumber :: Parser Int
consumeNumber =
    spaces >> read <$> many1 digit

index :: [a] -> Int -> Maybe a
index xs i | i >= 1 && i <= length xs = Just $ xs !! (i - 1)
           | otherwise = Nothing

valueTree :: Tree Int -> Int
valueTree (Branch [] metadata) = sum metadata
valueTree (Branch children metadata) = sum . map valueTree . catMaybes $ map (index children) metadata

instance Foldable Tree where
    foldr f acc (Branch children metadata) = foldr (flip (foldr f)) (foldr f acc metadata) children

instance Functor Tree where
    fmap f (Branch children metadata) = Branch (fmap (fmap f) children) (fmap f metadata)

main :: IO ()
main = do
    f <- readFile "nodes.txt"


    case runParse getNode f of
        Left e -> putStrLn $ "Error: " ++ show e
        Right tree -> do
            putStrLn " ---- PART ONE ---- "
            print $ sum tree
            putStrLn " ---- PART TWO ---- "
            print $ valueTree tree


    pure ()
