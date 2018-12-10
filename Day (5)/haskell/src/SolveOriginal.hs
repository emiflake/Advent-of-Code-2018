{-# LANGUAGE NamedFieldPuns #-}
module SolveOriginal where

-- import Data.List
-- import Control.Monad
-- import Data.Ord
-- import qualified Data.Sequence as Seq


-- data GameState = GameState { players :: [Player]
--                            , marbles :: [Marble]
--                            , circle  :: Circle } deriving Show

-- type Player = (String, Int)
-- type Marble = Int

-- data Circle = Circle { contents :: Marble
--                      , current  :: Int } deriving Show

-- showCircle :: Circle -> String
-- showCircle c@(Circle contents current) = unwords . map (\(v, i) -> if i == (current + 1) then "[" ++ show v ++ "]" else show v) $ zip contents [0..]

-- showState :: GameState -> String
-- showState (GameState players marbles c@(Circle contents current)) = 
--                                                                     "CIRCLE: " ++ showCircle c 
--                                                               ++ ",\nPLAYERS: " ++ show players
--                                                               ++ ",\nCURRENT: " ++ show current

-- normalize :: Circle -> Int -> Int
-- normalize c@(Circle contents _) index | index >= len = normalize c (index - len)
--                                       | index < 0    = normalize c (index + len)
--                                       | otherwise    = index
--     where len = length contents

-- circleAt :: Circle -> Int -> Marble
-- circleAt c@(Circle contents _) index = contents !! normalize c index


-- alterScore :: Player -> (Int -> Int) -> Player
-- alterScore (n, scr) f = (n, f scr) 

-- -- Bad performance, TODO: change data structure
-- insert' :: Int -> a -> [a] -> [a]
-- insert' n v xs = let (ys, zs) = splitAt n xs in ys ++ [v] ++ zs

-- pop' :: Int -> [a] -> (a, [a])
-- pop' n xs = let (ys, zs) = splitAt n xs in (head zs, ys ++ tail zs)

-- insertAt :: Circle -> Int -> Marble -> Circle
-- insertAt c@(Circle contents curr) index marble = Circle (insert' (normalize c index + 1) marble contents) curr

-- removeAt :: Circle -> Int -> (Circle, Marble)
-- removeAt c@(Circle contents curr) index = let (v, ncont) = pop' (normalize c index + 1) contents in (Circle ncont curr, v)

-- initialGameState :: [Player] -> [Marble] -> GameState
-- initialGameState players marbles = GameState players marbles (Circle [0] 0)

-- divBy :: (Eq a, Integral a) => a -> a -> Bool
-- divBy a b = a `mod` b == 0 

-- stepState :: GameState -> GameState
-- stepState (GameState (player:players) (marble:marbles) c@(Circle contents current)) | marble `divBy` 23 = let marbleValue = marble
--                                                                                                               (Circle ncontents curr, popped) = removeAt c (current - 7)
--                                                                                                            in (GameState (players++[alterScore player (+ (marbleValue + popped))]) 
--                                                                                                                          marbles 
--                                                                                                                         (Circle ncontents (normalize c (current - 7))))
--                                                                                     | otherwise         = let ncurrent = normalize c (current + 2)
--                                                                                                               (Circle ncontents _ ) = insertAt c ncurrent marble
--                                                                                                            in (GameState (players++[player]) marbles (Circle ncontents ncurrent))
-- stepThrough :: GameState -> GameState
-- stepThrough gs@(GameState _ xs _) | length xs <= 1 = gs
--                                   | otherwise      = stepThrough $ stepState gs


-- bestPlayer :: GameState -> Player
-- bestPlayer (GameState players _ _ ) = head . sortOn (Down . snd) $ players  


-- alphs :: [String]
-- alphs = tail $ (inits . repeat) ['A'..'Z'] >>= sequence

-- parse :: String -> ([Player], [Marble])
-- parse s = (zip alphs $ replicate players 0, [1..(marbles + 1)])
--     where ws = words s
--           (players, marbles) = (read $ head ws, read $ ws !! 6)

-- solve :: String -> IO ()
-- solve d = do
--     let (players, marbles) = parse d

--     let initial = initialGameState players marbles

--     print . bestPlayer . stepThrough $ initial


--     pure ()