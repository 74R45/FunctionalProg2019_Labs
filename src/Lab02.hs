{-# OPTIONS_GHC -Wall #-}
module Lab02 where

-------------------------------
-- Simple Functions on Lists --
-------------------------------

-- Task 1 -----------------------------------------
sumFr :: [Integer] -> Integer
sumFr xs = foldr (+) 0 xs

-- Task 2 -----------------------------------------
factorial :: Integer -> Integer
factorial x = foldl (*) 1 [1..x]

-- Task 3 -----------------------------------------
concatFr :: [Integer] -> [Integer] -> [Integer]
concatFr xs ys = foldr (:) ys xs

-- Task 4 -----------------------------------------
sortInsert :: [Integer] -> [Integer]
sortInsert xs = foldl insert [] xs

insert :: [Integer] -> Integer -> [Integer]
insert [] x = [x]
insert l@(x:xs) y | y <= x = y : l
                  | otherwise = x : insert xs y

-- Task 5 -----------------------------------------
map2 :: (a->b->c) -> [a] -> [b] -> [c]
map2 _ [] _ = []
map2 _ _ [] = []
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys

-- Task 6 -----------------------------------------
expPart :: Integer -> Integer -> Double
expPart m n = sum (map (\i -> fromIntegral (m^i) / fromIntegral (factorial i)) [1..n])

-- Task 7 -----------------------------------------
triangle :: [Integer]
triangle = scanl1 (+) [1..]

-- Task 8 -----------------------------------------
piramid :: [Integer]
piramid = scanl1 (+) (map (^two) [1..])

two :: Integer -- To avoid a warning about
two = 2        -- casting 2 to integer.

-- Task 9 -----------------------------------------
indexes :: [Int] -> [Int] -> [Int]
indexes xs ys = let subys = scanr (:) [] ys in
                [x | x <- [0..(length subys -1)], startingFrom xs (subys!!x)]

startingFrom :: [Int] -> [Int] -> Bool
startingFrom [] _ = True
startingFrom _ [] = False
startingFrom (x:xs) (y:ys) | x == y = startingFrom xs ys
                           | otherwise = False
