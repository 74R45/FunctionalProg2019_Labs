{-# OPTIONS_GHC -Wall #-}
module Lab04 where

---------------------
-- Oriented Graphs --
---------------------

import Data.List

type Graph = [[Int]]

-- Task 1 -----------------------------------------
isGraph :: Graph -> Bool
isGraph [] = True
isGraph (x:gr) = isSet x && isGraph gr

isSet :: [Int] -> Bool
isSet [] = True
isSet (x:xs) = notElem x xs && isSet xs

-- Task 2 -----------------------------------------
isTournament :: Graph -> Bool
isTournament gr = null [(x,y) | x <- [0..(length gr)-1], y <- [0..(length gr)-1],
                                (elem y (gr!!x)) `xor` (notElem x (gr!!y)), x /= y]

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

-- Task 3 -----------------------------------------
isTransitive :: Graph -> Bool
isTransitive gr = null [(v,u) | v <- [0..(length gr)-1], u <- gr!!v, w <- gr!!u,
                                notElem w (gr!!v)]

-- Task 4 -----------------------------------------
buildTransitive :: Graph -> Graph
buildTransitive gr = until isTransitive step gr
    where step :: Graph -> Graph
          step grx = add (findFirst grx) grx
              where findFirst :: Graph -> (Int, Int)
                    findFirst gry = head [(v,w) | v <- [0..(length gry)-1],
                                             u <- gry!!v, w <- gry!!u,
                                             notElem w (gry!!v)]
                    add :: (Int, Int) -> Graph -> Graph
                    add (x,y) gry = take x gry ++ [sort (y:(gry!!x))] ++ (drop (x+1) gry)

-- Task 5 -----------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay gr x y | null res = Nothing
               | otherwise = Just res
    where res = longest (allWays x [])
          longest :: [[Int]] -> [Int]
          longest [] = []
          longest (xs:xss) | length xs >= length (longest xss) = xs
                           | otherwise = longest xss
          allWays :: Int -> [Int] -> [[Int]]
          allWays a _ | a == y = [[y]]
          allWays a used = [a:c | b <- (gr!!a), notElem b used, b /= a,
                                  c <- (allWays b (a:used))]

-- Task 6 -----------------------------------------
gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay gr | null cycles = Nothing
               | otherwise = Just (head cycles)
    where cycles = [res | x <- [0..(length gr)-1],
                          res <- (allCycles gr x x []),
                          length res - 1 == length gr]

allCycles :: Graph -> Int -> Int -> [Int] -> [[Int]]
allCycles gr st end used | st == end && not (null used) = [[end]]
                         | elem st used = []
                         | otherwise = [st:step | nxt <- (gr!!st),
                           step <- (allCycles gr nxt end (st:used))]

-- Task 7 -----------------------------------------
isAcyclic :: Graph -> Bool
isAcyclic gr = null [res | x <- [0..(length gr)-1], res <- (allCycles gr x x [])]

-- Task 8 -----------------------------------------
topolSort :: Graph -> Maybe [Int]
topolSort gr | isAcyclic gr = Just (snd (until cond1 step1 (0, [])))
             | otherwise = Nothing
    where cond1 :: (Int, [Int]) -> Bool -- For iterating on gr
          cond1 (n, _) = n >= length gr
          step1 :: (Int, [Int]) -> (Int, [Int])
          step1 (n, res1) | notElem n nxt1 = (n+1, n:nxt1)
                          | otherwise = (n+1, nxt1)
              where nxt1 = snd (until cond2 step2 (0, res1)) -- For iterating on gr!!n
                    cond2 :: (Int, [Int]) -> Bool
                    cond2 (m, _) = m >= length (gr!!n)
                    step2 :: (Int, [Int]) -> (Int, [Int])
                    step2 (m, res2) = (m+1, snd (step1 (gr!!n!!m, res2)))

-- Task 9 -----------------------------------------
isTopolSort :: Graph -> [Int] -> Bool
isTopolSort gr ts = isAcyclic gr && isSet ts && (length ts == length gr) &&
                    (null [(i1,i2) | x <- [0..(length gr)-1], y <- (gr!!x),
                                     i1 <- [elemIndex x ts], i2 <- [elemIndex y ts],
                                     i1 == Nothing || i2 == Nothing || i1 > i2])

-- Test data -- Graphs ----------------------------
gr1, gr2, gr3, gr4 :: Graph
gr1 = [[1,2,3],[2,3],[3,4],[4],[]]
gr2 = [[3,4],[0,3],[0,1,4],[2,4],[1]]
gr3 = [[1],[2],[3],[1],[0,3]]
gr4 = [[1,2,3],[1,2,3],[1,2,3],[1,2,3],[0,1,2,3]]
