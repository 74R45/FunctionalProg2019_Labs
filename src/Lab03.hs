{-# OPTIONS_GHC -Wall #-}
module Lab03 where

--------------------
-- Bulls and Cows --
--------------------

import Data.Char

-- List of digits '0'..'9'
type Code = String

-- Game step (Move) contains an attempt (Code) and two integers:
--    numbers of "bulls" and "cows" in this attempt
data Move = Move Code Int Int
          deriving (Show, Eq)

-- Task 1 -----------------------------------------
exactMatches :: Code -> Code -> Int
exactMatches "" _ = 0
exactMatches _ "" = 0
exactMatches (x:cd) (y:att) | x == y = 1 + exactMatches cd att
                            | otherwise = exactMatches cd att

-- Task 2 -----------------------------------------
countDigits :: Code -> [Int]
countDigits "" = [0,0,0,0,0,0,0,0,0,0]
countDigits (d:cd) = let res = countDigits cd
                         dInt = digitToInt d in
                     take dInt res ++ [res!!dInt+1] ++ (drop (dInt+1) res)

-- Another solution -------------------------------
--countDigits2 :: Code -> [Int]
--countDigits2 cd = countDigitStep cd 0

--countDigitStep :: Code -> Int -> [Int]
--countDigitStep cd 9 = [countDigit cd 9]
--countDigitStep cd n = countDigit cd n : countDigitStep cd (n+1)

--countDigit :: Code -> Int -> Int
--countDigit "" _ = 0
--countDigit (d:cd) n | digitToInt d == n = 1 + countDigit cd n
--                    | otherwise = countDigit cd n

-- Task 3 -----------------------------------------
matches :: Code -> Code -> Int
matches cd att = sum (zipWith min (countDigits cd) (countDigits att))

-- Task 4 -----------------------------------------
getMove :: Code -> Code -> Move
getMove cd att = let bulls = exactMatches cd att in
                 Move att bulls (matches cd att - bulls)

-- Task 5 -----------------------------------------
isConsistent :: Move -> Code -> Bool
isConsistent mv@(Move cd1 _ _) cd2 = mv == getMove cd2 cd1

-- Task 6 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes mv cdx = filter (isConsistent mv) cdx

-- Task 7 -----------------------------------------
allCodes :: Int -> [Code]
allCodes 0 = [""]
allCodes n = [d:cd | d <- digits, cd <- allCodes (n-1)]

digits :: String
digits = "0123456789"

-- Task 8 -----------------------------------------
solve :: Code -> [Move]
solve cd = solveStep cd (allCodes (length cd))

solveStep :: Code -> [Code] -> [Move]
solveStep _ [] = []
solveStep cd (att:cdx) = let mv = getMove cd att in
                      mv : solveStep cd (filterCodes mv cdx)
