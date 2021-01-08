{-# OPTIONS_GHC -Wall #-}
module Lab05 where

-------------------
-- Combinatorics --
-------------------

import Data.List
import Data.Maybe

type Graph = [[Int]]

-- Task 1 -----------------------------------------
lucky :: Int -> [String]
lucky n = filter isLucky (map show [(10^(2*n-1)::Int)..((10^(2*n)::Int)-1)])
          where isLucky :: String -> Bool
                isLucky x = sum (map ((read::String->Int).(:[])) (take n x)) ==
                            sum (map ((read::String->Int).(:[])) (drop n x))

-- Task 2 -----------------------------------------
queens :: Int -> [[Int]]
queens n = queensStep n 1 []

queensStep :: Int -> Int -> [(Int, Int)] -> [[Int]]
queensStep n x taken | x < n = [y:next | y <- [1..n], notElem (x,y) taken,
                           next <- queensStep n (x+1) (addTaken x y n taken)]
               | x == n = [[y] | y <- [1..n], notElem (x,y) taken]
               | otherwise = []

addTaken :: Int -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
addTaken x0 y0 n taken0 = addHorizontal x0 $ addVertical 1 $
                          addDiagonal1 x0 y0 $ addDiagonal2 x0 y0 taken0
    where addHorizontal :: Int -> [(Int, Int)] -> [(Int, Int)]
          addHorizontal x taken | x > n = taken
                                | otherwise = add (x,y0) (addHorizontal (x+1) taken)
          addVertical :: Int -> [(Int, Int)] -> [(Int, Int)]
          addVertical y taken | y > n = taken
                              | otherwise = add (x0,y) (addVertical (y+1) taken)
          addDiagonal1 :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
          addDiagonal1 x y taken | x > n || y > n = taken
                                | otherwise = add (x,y) (addDiagonal1 (x+1) (y+1) taken)
          addDiagonal2 :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
          addDiagonal2 x y taken | x > n || y < 1 = taken
                                | otherwise = add (x,y) (addDiagonal2 (x+1) (y-1) taken)
          add :: (Int,Int) -> [(Int, Int)] -> [(Int, Int)]
          add xy taken | elem xy taken = taken
                       | otherwise = xy:taken

-- Task 3 -----------------------------------------
maxLen :: [Int] -> Int
maxLen xs = maximum (map length (allAscSeq xs))

-- Task 4 -----------------------------------------
maxSeq :: [Int] -> [Int]
maxSeq xs = head (allMaxSeq xs)

-- Task 5 -----------------------------------------
allMaxSeq :: [Int] -> [[Int]]
allMaxSeq xs = let seqs = allAscSeq xs
                   mx = maximum (map length seqs)
               in filter ((==mx).length) seqs

allAscSeq :: [Int] -> [[Int]]
allAscSeq xs = filter ascending (allSeq xs)

ascending :: [Int] -> Bool
ascending (x:y:xs) = x < y && ascending (y:xs)
ascending _ = True

allSeq :: [Int] -> [[Int]]
allSeq [] = []
allSeq (x:xs) = [x]:(map (x:) next)++next
    where next = allSeq xs

-- Task 6 -----------------------------------------
genExpr :: Int -> Int -> [String]
genExpr a b = filter ((==Just b).readExpr) (genAllExpr (show a))

readExpr :: String -> Maybe Int
readExpr (x:xs) = readExprStep (read [x]) xs
readExpr _ = Nothing

readExprStep :: Int -> String -> Maybe Int
readExprStep n ('+':x:xs) = readExprStep (n + read [x]) xs
readExprStep n ('-':x:xs) = readExprStep (n - read [x]) xs
readExprStep n ('*':x:xs) = readExprStep (n * read [x]) xs
readExprStep n [] = Just n
readExprStep _ _ = Nothing

genAllExpr :: String -> [String]
genAllExpr [] = []
genAllExpr [x] = [[x]]
genAllExpr (x:y:num) = [x:op:next | op <- ['+', '-', '*'], next <- genAllExpr (y:num)]

-- Task 7 -----------------------------------------
genExprBracket :: Int -> Int -> [String]
genExprBracket a b = filter ((==Just b).readExprBracket) (genAllExprBracket (show a))

readExprBracket :: String -> Maybe Int
readExprBracket [x] = Just (read [x])
readExprBracket xs | length xs < 2 || isNothing prev || isNothing next = Nothing
                   | ys!!i == '+' = Just $ (fromJust prev) + (fromJust next)
                   | ys!!i == '-' = Just $ (fromJust prev) - (fromJust next)
                   | ys!!i == '*' = Just $ (fromJust prev) * (fromJust next)
    where i = findOperator 0 ys
          ys = tail $ init xs
          prev = readExprBracket (take i ys)
          next = readExprBracket (drop (i+1) ys)
readExprBracket _ = Nothing

findOperator :: Int -> String -> Int
findOperator nBr ('(':xs) = findOperator (nBr+1) xs + 1
findOperator nBr (')':xs) = findOperator (nBr-1) xs + 1
findOperator 0 (x:_) | x == '+' || x == '-' || x == '*' = 0
                     | otherwise = 1
findOperator nBr (_:xs) = findOperator nBr xs + 1
findOperator _ [] = 0

genAllExprBracket :: String -> [String]
genAllExprBracket xs = nub [zs | ys <- (genAllExpr xs), zs <- (genAllExprBracketStep [[y] | y <- ys])]

genAllExprBracketStep :: [String] -> [String]
genAllExprBracketStep [xs] = [xs]
genAllExprBracketStep xss = [ys | i <- [0,2..(length xss-3)], ys <- genAllExprBracketStep (addBrackets xss i)]

addBrackets :: [String] -> Int -> [String]
addBrackets xss i = take i xss ++ (('(':(xss!!i)++(xss!!(i+1))++(xss!!(i+2))++")") : drop (i+3) xss)

-- Task 8 -----------------------------------------
topolSortAll :: Graph -> [[Int]]
topolSortAll gr = filter (isTopolSort gr) (allSorts (length gr) (length gr - 1) [])

allSorts :: Int -> Int -> [Int] -> [[Int]]
allSorts n m used | n < 1 = []
                  | n == 1 = [[x] | x <- [0..m], notElem x used]
                  | otherwise = [x:next | x <- [0..m], notElem x used, next <- allSorts (n-1) m (x:used)]

isTopolSort :: Graph -> [Int] -> Bool
isTopolSort gr ts = isAcyclic gr && isSet ts && (length ts == length gr) &&
                    (null [(i1,i2) | x <- [0..(length gr)-1], y <- (gr!!x),
                                     i1 <- [elemIndex x ts], i2 <- [elemIndex y ts],
                                     i1 == Nothing || i2 == Nothing || i1 > i2])

isAcyclic :: Graph -> Bool
isAcyclic gr = null [res | x <- [0..(length gr)-1], res <- (allCycles gr x x [])]

allCycles :: Graph -> Int -> Int -> [Int] -> [[Int]]
allCycles gr st end used | st == end && not (null used) = [[end]]
                         | elem st used = []
                         | otherwise = [st:step | nxt <- (gr!!st),
                           step <- (allCycles gr nxt end (st:used))]

isSet :: [Int] -> Bool
isSet [] = True
isSet (x:xs) = notElem x xs && isSet xs

-- Test Data -- Graphs ----------------------------
gr1 :: Graph
gr1 = [[1,2,3], [], [3,4], [4],[]]
