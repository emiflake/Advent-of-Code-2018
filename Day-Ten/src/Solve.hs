{-# LANGUAGE ViewPatterns #-}
module Solve where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String
import Data.Either
import Data.List
import Control.Monad

import Codec.Picture

import Text.Pretty.Simple

import Data.Array (Array, (!))
import qualified Data.Array as Array

type Img = Array (Vec2 Int) PixelRGB8
type Vec2 a = (a, a)

data Star = Star { position :: (Int, Int)
                 , velocity :: (Int, Int) } deriving Show


black, red :: PixelRGB8
black = PixelRGB8 0 0 0
red   = PixelRGB8 255 0 0

fill :: PixelRGB8 -> Vec2 Int -> Img
fill fillValue (pred -> width, pred -> height) = 
    Array.array ((0, 0), (width, height)) [ ((x, y), fillValue) 
                                          | x <- [0..width]
                                          , y <- [0..height] ]

setPixel :: Img -> (Vec2 Int, PixelRGB8) -> Img
setPixel img (pos, pixel) = img Array.// [(pos, pixel)]

type ImageFunction = Int -> Int -> PixelRGB8

imgReader :: Img -> ImageFunction
imgReader image x y = image ! (x, y)

createImage :: Vec2 Int -> ImageFunction -> Image PixelRGB8
createImage (width, height) f = generateImage f width height

parseStar :: Parser Star
parseStar = do
    _ <- string "position=<"

    x <- read <$> manyTill anyChar (try $ char ',')
    y <- read <$> manyTill anyChar (try $ char '>')

    _ <- string " velocity=<"

    xv <- read <$> manyTill anyChar (try $ char ',')
    yv <- read <$> manyTill anyChar (try $ char '>')

    pure $ Star (x, y) (xv, yv)

parseStars :: String -> [Star]
parseStars = rights . map (parse parseStar "") . lines

stepStars :: [Star] -> [Star]
stepStars = fmap stepStar

stepStar :: Star -> Star
stepStar (Star (x, y) (xv, yv)) = Star (x + xv, y + yv) (xv, yv) 

others :: [a] -> [[a]]
others xs = ($ []) . foldr f (const []) $ zip (drop 1 . tails $ xs) xs
    where f (as, a) b front = (front ++ as) : b (front ++ [a])

othersWithSelf :: [a] -> [(a, [a])]
othersWithSelf = zip <$> id <*> others

distance :: Star -> Star -> Int
distance (Star (x1, y1) _) (Star (x2, y2) _) = abs (x1 - x2) + abs (y1 - y2)  

yspread :: [Star] -> Int
yspread stars = maximum ys - minimum ys 
    where ys = fmap (snd . position) stars

cost :: [Star] -> Int
cost = sum . fmap (\(s, others) -> sum $ map (distance s) others ) . othersWithSelf


dimfor :: [Star] -> (Int, Int)
dimfor stars = dim
    where posses = map position stars
          xs = map fst posses
          ys = map snd posses
          ps = zip xs ys
  
          (minx, miny) = (minimum xs, minimum ys)
          (maxx, maxy) = (maximum xs, maximum ys)
  
          dim@(w, h) = (maxx - minx + 1, maxy - miny + 1)

imageFor :: [Star] -> Img
imageFor stars = foldr (\(x, y) img -> img `setPixel` ((x - minx, y - miny), PixelRGB8 255 255 255)) 
                 (fill black dim) posses
    where posses = map position stars
          xs = map fst posses
          ys = map snd posses
          ps = zip xs ys

          (minx, miny) = (minimum xs, minimum ys)
          (maxx, maxy) = (maximum xs, maximum ys)

          dim@(w, h) = (maxx - minx + 1, maxy - miny + 1)

solve :: String -> IO ()
solve path = do
    stars <- parseStars <$> readFile path
    let starList = iterate stepStars stars

    let findBest (x:y:xs) c | c x < c y = x
                            | otherwise = findBest (y:xs) c

    let best = findBest starList yspread
    let dim = dimfor best in writePng "out.png" $ createImage dim (imgReader (imageFor best))
                        

    let searchRange = take 30 $ drop 10870 starList
    let costBasedStarList = map (\ss -> (yspread ss, ss)) searchRange
    let (cost, stars) = head . sortOn fst $ costBasedStarList


    let dim = dimfor stars


    forM_ (zip [0..] searchRange) $ \(i, ss) ->
        let dim = dimfor ss in      
        writePng ("imgs/test" ++ show i ++ ".png") $ createImage dim (imgReader (imageFor ss))