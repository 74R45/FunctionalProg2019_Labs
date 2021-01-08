{-# OPTIONS_GHC -Wall #-}
module Lab01 where

--------------------------------
-- Simple Recursive Functions --
--------------------------------

-- Task 1 -----------------------------------------
factorial :: Integer -> Integer
factorial x = if x <= 1 then 1 else x * factorial (x-1)

-- Task 2 -----------------------------------------
listSum :: [Int] -> [Int] -> [Int]
listSum [] [] = []
listSum xs ys = if xs == [] then ys
                else if ys == [] then xs
                else (head xs) + (head ys) : (listSum (tail xs) (tail ys))

-- Task 3 ----------------------------------------- 
oddEven :: [Int] -> [Int]
oddEven [] = []
oddEven (x:xs) = if xs == [] then x : xs
             else head xs : x : oddEven (tail xs)

-- Task 4 -----------------------------------------
position :: Int -> [Int] -> Int
position _ [] = -1
position n (x:xs) = if n == x then 0
                    else let rest = position n xs
                        in if rest == -1 then -1 else 1 + rest

-- Task 5 -----------------------------------------
set :: [Int] -> [Int]
set [] = []
set (x:xs) = if elem x xs
             then set xs
             else x : set xs

-- Task 6 -----------------------------------------
union :: [Int] -> [Int] -> [Int]
union xs ys = set (xs ++ ys)

-- Task 7 -----------------------------------------
intersection :: [Int] -> [Int] -> [Int]
intersection [] _ = []
intersection (x:xs) ys = if elem x ys 
                         then set (x : intersection xs ys)
                         else set (intersection xs ys)

-- Task 8 -----------------------------------------
factorialsM :: [Integer]
factorialsM = map factorial [1..]
