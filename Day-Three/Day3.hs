{-# LANGUAGE LambdaCase #-}
module Day3 where

import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import NanoParsec

type ID = Int
type Pos = (Int, Int)
type Dim = (Int, Int)

data Claim = Claim { i    :: Int
                   , pos  :: Pos
                   , size :: Dim }
                   deriving (Eq, Show)

parsed :: String -> [Claim]
parsed = map (runParser pindiv) . lines

many :: Parser a -> Parser [a]
many p = do
    x <- p
    
    xs <- option (many p) (pure [])
    pure (x:xs)

pindiv :: Parser Claim
pindiv = do
    _ <- char '#'
    i <- many digit
    _ <- spaces
    _ <- char '@'
    _ <- spaces
    x <- many digit
    _ <- char ','
    y <- many digit
    _ <- char ':'
    _ <- spaces
    w <- many digit
    _ <- char 'x'
    h <- many digit

    pure $ Claim { i    = read i
                 , pos  = (read x, read y)
                 , size = (read w, read h) }

type Fabric = Map.Map Pos [ID]

claimArea :: Claim -> Fabric -> Fabric
claimArea claim@(Claim id _ _) f = foldr (\pos f -> updateFabric f id pos) f positions
    where positions = createPositions claim

createPositions :: Claim -> [Pos]
createPositions (Claim _ (x, y) (w, h)) = [ (xpos, ypos) |
                                            xpos <- [x..(x+w - 1)]
                                          , ypos <- [y..(y+h - 1)] ]

updateFabric :: Fabric -> ID -> Pos -> Fabric
updateFabric f i p = Map.alter (\case
    Just v -> Just (i:v)
    Nothing -> Just [i]) p f

-- Find out if Claim overlaps
overlaps :: Claim -> Fabric -> Bool
overlaps claim@(Claim id _ _) f = not . all not $ map (`isContested` f) positions
    where positions = createPositions claim

isContested :: Pos -> Fabric -> Bool
isContested p f = case Map.lookup p f of
    Just xs -> length xs /= 1
    Nothing -> True


startingFabric :: Fabric
startingFabric = Map.fromList []

main :: IO ()
main = do
    f <- readFile "claims.txt"

    let claims = parsed f
    let fabric = foldr claimArea startingFabric claims

    -- count the amount of fabric areas with more than 2 claims
    putStrLn "First part: "
    print $ foldr (\p c -> c + (if length p >= 2 then 1 else 0)) 0 fabric

    -- find the only non overlapping claim
    putStrLn "Second part: "
    print $ find (\claim -> not $ claim `overlaps` fabric) claims

    pure ()
