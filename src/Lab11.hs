{-# OPTIONS_GHC -Wall #-}
module Lab11 where

------------------------------
-- Binary Decision Diagrams --
------------------------------

import Data.Char(isLower)

data BExp = Bvalue Bool | Bvar Char | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Char, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Char, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

-- Task 1 -----------------------------------------
checkSat :: BDD -> Env -> Bool
checkSat (id0, nodes) env = step id0
  where step :: Int -> Bool
        step 1 = True
        step 0 = False
        step id1 = let node = snd $ head $ filter ((== id1) . fst) nodes in
                   case getFromEnv env (fst3 node) of
                     False -> step (snd3 node)
                     True -> step (trd3 node)

fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

trd3 :: (a,b,c) -> c
trd3 (_,_,x) = x

getFromEnv :: Env -> Char -> Bool
getFromEnv env c = snd $ head $ filter ((== c) . fst) env

-- Task 2 -----------------------------------------
sat :: BDD -> [[(Char, Bool)]]
sat (id0, nodes) = step id0
  where step :: Int -> [[(Char, Bool)]]
        step 0 = []
        step 1 = [[]]
        step id1 = let node = snd $ head $ filter ((== id1) . fst) nodes in
                   concat [(map ((fst3 node, False):) (step $ snd3 node)),
                          (map ((fst3 node, True):) (step $ trd3 node))]

-- Task 3 -----------------------------------------
simplify :: BExp -> BExp
simplify e = case e of
                 (And (Bvalue x) (Bvalue y)) -> Bvalue (x && y)
                 (Or (Bvalue x) (Bvalue y)) -> Bvalue (x || y)
                 (Not (Bvalue x)) -> Bvalue (not x)
                 x -> x

-- Task 4 -----------------------------------------
restrict :: BExp -> Char -> Bool -> BExp
restrict e c v = case e of
                   Bvar x | x == c -> Bvalue v
                          | otherwise -> Bvar x
                   And x y -> simplify (And (restrict x c v)
                                            (restrict y c v))
                   Or x y -> simplify (Or (restrict x c v)
                                           (restrict y c v))
                   Not x -> simplify $ Not (restrict x c v)
                   x -> x

-- Task 5 -----------------------------------------
-- Precondition: each variable (letter) in a Boolean expression (BExp) appears
--    exactly once in the list of variables (Char); there are no other items
buildBDD :: BExp -> [Char] -> BDD
buildBDD e xs = buildBDD' e 2 xs 

buildBDD' :: BExp -> NodeId -> [Char] -> BDD
buildBDD' (Bvalue True) _ _ = (1, [])
buildBDD' (Bvalue False) _ _ = (0, [])
buildBDD' e n (x:xs) = let left = buildBDD' (restrict e x False) (2*n) xs
                           right = buildBDD' (restrict e x True) (2*n+1) xs in
                       (n, (n, (x, fst left, fst right)):(snd left)++(snd right))
buildBDD' _ _ [] = error "not enough variables in the list"

-- Task 6 -----------------------------------------
-- Precondition: each variable (letter) in a Boolean expression (BExp) appears
--    exactly once in the list of variables (Char); there are no other items
buildROBDD :: BExp -> [Char] -> BDD
buildROBDD e xs = reduce $ buildBDD e xs

reduce :: BDD -> BDD
reduce nodes = let nodes2 = reduce2 $ reduce1 nodes in
               if (nodes == nodes2) then nodes else reduce nodes2

reduce1 :: BDD -> BDD
reduce1 nodes = let (toReplace, nodes2) = removeDuplicates $ snd nodes in
                replaceIds toReplace (fst nodes, nodes2)

removeDuplicates :: [BDDNode] -> ([(NodeId, NodeId)], [BDDNode])
removeDuplicates [] = ([], [])
removeDuplicates ((m1,n):next) = case findNode next n of
                                 Just m2 -> ((m1, m2):(fst res), snd res)
                                 Nothing -> (fst res, (m1,n):(snd res))
  where res = removeDuplicates next

findNode :: [BDDNode] -> (Char, NodeId, NodeId) -> Maybe NodeId
findNode [] _ = Nothing
findNode ((m1,n1):next) n2 | n1 == n2 = Just m1
                           | otherwise = findNode next n2

replaceIds :: [(NodeId, NodeId)] -> BDD -> BDD
replaceIds ids (m0, []) = (change m0 ids ids, [])
replaceIds [] x = x
replaceIds ids (m0,(m,(c,l,r)):next) = (fst res,(m,(c,change l ids ids,change r ids ids)):(snd res))
  where res = replaceIds ids (m0, next)

change :: NodeId -> [(NodeId, NodeId)] -> [(NodeId, NodeId)] -> NodeId
change x [] _ = x
change x ((from, to):nextIds) ids | x == from = change to ids ids -- x -> y && y -> z => x -> z
                                  | otherwise = change x nextIds ids

reduce2 :: BDD -> BDD
reduce2 nodes = let (toReplace, nodes2) = removeRedundant $ snd nodes in
                replaceIds toReplace (fst nodes, nodes2)

removeRedundant :: [BDDNode] -> ([(NodeId, NodeId)], [BDDNode])
removeRedundant [] = ([], [])
removeRedundant (node@(m,(_,f,t)):next) | f == t = ((m, f):(fst res), snd res)
                                        | otherwise = (fst res, node:(snd res))
  where res = removeRedundant next

-- Task 7 -----------------------------------------
fullBexp :: String -> Maybe BExp
fullBexp s = case bexp s of
               Just (e, "") -> Just e
               _ -> Nothing

bexp :: String -> Maybe (BExp,String)
bexp s = case bcon s of
         Just (e1, rest1) -> case manyCon (e1, rest1) of
            Just (e2, rest2) -> Just (e2, rest2)
            Nothing -> Nothing
         Nothing -> Nothing

bcon :: String -> Maybe (BExp,String)
bcon s = case bdis s of
           Just (e1, rest1) -> case manyDis (e1, rest1) of
             Just (e2, rest2) -> Just (e2, rest2)
             Nothing -> Nothing
           Nothing -> Nothing

manyCon :: (BExp,String) -> Maybe (BExp,String)
manyCon (e0, '|':s) = case bcon s of
                        Just (e1, rest1) -> case manyCon (e1, rest1) of
                          Just (e2, rest2) -> Just (Or e0 e2, rest2)
                          Nothing -> Nothing
                        Nothing -> Nothing
manyCon (e0, s) = Just (e0, s)

bdis :: String -> Maybe (BExp,String)
bdis s = case s of
           ('(':xs) -> case bexp xs of
             Just (e, ')':rest) -> Just (e, rest)
             _ -> Nothing
           ('!':xs) -> case bdis xs of
             Just (e, rest) -> Just (Not e, rest)
             _ -> Nothing
           ('T':xs) -> Just (Bvalue True, xs)
           ('F':xs) -> Just (Bvalue False, xs)
           (x:xs) | isLower x -> Just (Bvar x, xs)
                  | otherwise -> Nothing
           _ -> Nothing

manyDis :: (BExp,String) -> Maybe (BExp,String)
manyDis (e0, '&':s) = case bdis s of
                        Just (e1, rest1) -> case manyDis (e1, rest1) of
                          Just (e2, rest2) -> Just (And e0 e2, rest2)
                          Nothing -> Nothing
                        Nothing -> Nothing
manyDis (e0, s) = Just (e0, s)

-- Test Data --------------------------------------
bs1, bs2, bs3, bs4, bs5, bs6, bs7, bs8, bs9 :: String
bs1 = "F"
bs2 = "!(x&(F|y))"
bs3 = "u&T"
bs4 = "d&(x|!y)"
bs5 = "!(d&(x|!y))"
bs6 = "u&x|y&z" 
bs7 = "!y|(x|!e)"
bs8 = "u|!u"
bs9 = "z&(y|!y&x)"

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Bvalue False
b2 = Not (And (Bvar 'x') (Or (Bvalue False) (Bvar 'y')))
b3 = And (Bvar 'u') (Bvalue True)
b4 = And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y')))
b5 = Not (And (Bvar 'd') (Or (Bvar 'x') (Not (Bvar 'y'))))
b6 = Or (And (Bvar 'u') (Bvar 'x')) (And (Bvar 'y') (Bvar 'z'))
b7 = Or (Not (Bvar 'y')) (Or (Bvar 'x') (Not (Bvar 'e')))
b8 = Or (Bvar 'u') (Not (Bvar 'u'))
b9 = And (Bvar 'z') (Or (Bvar 'y') (And (Not (Bvar 'y')) (Bvar 'x')))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(2,('x',4,5)),(4,('y',1,1)),(5,('y',1,0))])
bdd3 = (5,[(5,('u',0,1))])
bdd4 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('d',0,1)),(9,('d',0,0)),
           (5,('y',10,11)),(10,('d',0,1)),(11,('d',0,1))])
bdd5 = (3,[(4,('y',8,9)),(3,('x',4,5)),(8,('d',1,0)),(9,('d',1,1)),
           (5,('y',10,11)),(10,('d',1,0)),(11,('d',1,0))])
bdd6 = (2,[(2,('u',4,5)),(4,('x',8,9)),(8,('y',16,17)),(16,('z',0,0)),
           (17,('z',0,1)),(9,('y',18,19)),(18,('z',0,0)),(19,('z',0,1)),
           (5,('x',10,11)),(10,('y',20,21)),(20,('z',0,0)),(21,('z',0,1)),
           (11,('y',22,23)),(22,('z',1,1)),(23,('z',1,1))])
bdd7 = (6,[(6,('x',4,5)),(4,('y',8,9)),(8,('e',1,1)),(9,('e',1,0)),
           (5,('y',10,11)),(10,('e',1,1)),(11,('e',1,1))])
bdd8 = (2,[(2,('u',1,1))])
bdd9 = (2,[(2,('x',4,5)),(4,('y',8,9)),(8,('z',0,0)),(9,('z',0,1)),(5,('y',10,11)),(10,('z',0,1)),(11,('z',0,1))])
