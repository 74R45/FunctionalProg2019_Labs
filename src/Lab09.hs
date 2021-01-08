{-# OPTIONS_GHC -Wall #-}
module Lab09 where

------------------
-- Suffix Trees --
------------------

data SuffixTree = Leaf Int | Node [(String, SuffixTree)]
                deriving (Eq, Ord, Show)

-- Task 1 -----------------------------------------
isPrefix :: String -> String -> Bool
isPrefix "" _ = True
isPrefix _ "" = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

-- Task 2 -----------------------------------------
partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition a b = partitionStep ([], a, b)
  where partitionStep :: Eq a => ([a], [a], [a]) -> ([a], [a], [a])
        partitionStep (pref, (x:xs), (y:ys)) | x == y = partitionStep (pref++[x], xs, ys)
                                             | otherwise = (pref, x:xs, y:ys)
        partitionStep x = x

-- Task 3 -----------------------------------------
suffixes :: [a] -> [[a]]
suffixes = init . scanr (:) []

-- Task 4 -----------------------------------------
isSubstring :: String -> String -> Bool
isSubstring "" _ = True
isSubstring a b = or $ map (isPrefix a) $ suffixes b

-- Task 5 -----------------------------------------
findSubstrings :: String -> String -> [Int]
findSubstrings xs ys = mapTrueIndices $ map (isPrefix xs) $ suffixes ys

mapTrueIndices :: [Bool] -> [Int]
mapTrueIndices [] = []
mapTrueIndices (x:xs) = ifFunc x (0:) (map (1+) $ mapTrueIndices xs)

ifFunc :: Bool -> (a -> a) -> a -> a
ifFunc True func res = func res
ifFunc False _ res = res

-- Task 6 -----------------------------------------
getIndices :: SuffixTree -> [Int]
getIndices (Leaf n) = [n]
getIndices (Node []) = []
getIndices (Node ((_, tree):rest)) = sortInsert $ getIndices tree ++ getIndices (Node rest)

sortInsert :: [Int] -> [Int]
sortInsert xs = foldl insertNum [] xs
  where insertNum :: [Int] -> Int -> [Int]
        insertNum [] x = [x]
        insertNum l@(y:ys) x | x <= y = x : l
                             | otherwise = y : insertNum ys x

-- Task 7 -----------------------------------------
findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' _ (Leaf _) = []
findSubstrings' _ (Node []) = []
findSubstrings' s (Node ((a, subtree):rest))
    | isPrefix s a = getIndices subtree
    | a /= "" && isPrefix a s = findSubstrings' (strMinus s a) subtree
    | otherwise = findSubstrings' s (Node rest)

strMinus :: String -> String -> String
strMinus a b = (get2nd $ partition a b)
  where get2nd :: (a, a, a) -> a
        get2nd (_, x, _) = x

-- Task 8 -----------------------------------------
insert :: (String, Int) -> SuffixTree -> SuffixTree
insert _ (Leaf _) = undefined
insert (s, n) (Node []) = Node [(s, Leaf n)]
insert sn@(s, n) (Node (at@(a, t):rest))
    | p == "" = addSubtree at $ insert sn (Node rest)
    | p == a = Node ((a, insert (strMinus s a, n) t):rest)
    | otherwise = Node ((p , Node [(strMinus s p, Leaf n), (strMinus a p, t)]):rest)
  where p = get1st (partition s a)

get1st :: (a, a, a) -> a
get1st (x, _, _) = x

addSubtree :: (String, SuffixTree) -> SuffixTree -> SuffixTree
addSubtree x (Node xs) = Node (x:xs)
addSubtree _ (Leaf _) = undefined

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

-- Task 9 -----------------------------------------
longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring tree = longestString (filter (\x -> length (findSubstrings' x tree) > 1) (allSubstrings str (length str-1))) ""
  where str = getString tree

allSubstrings :: String -> Int -> [String]
allSubstrings _ 0 = []
allSubstrings xs maxLen = filter (\x -> length x == maxLen) (map (take maxLen) (suffixes xs)) ++ allSubstrings xs (maxLen-1)

longestString :: [String] -> String -> String
longestString (x:xs) res | length x > length res = longestString xs x
                         | otherwise = longestString xs res
longestString [] res = res

getString :: SuffixTree -> String
getString tree = longestString (getAllStrings tree) ""
  where getAllStrings :: SuffixTree -> [String]
        getAllStrings (Leaf _) = undefined
        getAllStrings (Node []) = []
        getAllStrings (Node ((s, Leaf _):next)) = s : getAllStrings (Node next)
        getAllStrings (Node ((s, sub):next)) = s : map (s++) (getAllStrings sub) ++ getAllStrings (Node next)

-- Examples of Strings and Suffix Trees -----------
s1 :: String
s1 = "banana"

s2 :: String
s2  = "mississippi"

t1 :: SuffixTree
t1 = Node [("banana", Leaf 0),
          ("a", Node [("na", Node [("na", Leaf 1),
                                   ("", Leaf 3)]),
                     ("", Leaf 5)]),
          ("na", Node [("na", Leaf 2),
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 = Node [("mississippi", Leaf 0),
          ("i", Node [("ssi", Node [("ssippi", Leaf 1),
                                    ("ppi", Leaf 4)]),
                      ("ppi", Leaf 7),
                      ("", Leaf 10)]),
          ("s", Node [("si", Node [("ssippi", Leaf 2),
                                   ("ppi", Leaf 5)]),
                      ("i", Node [("ssippi", Leaf 3),
                                  ("ppi", Leaf 6)])]),
          ("p", Node [("pi", Leaf 8),
                      ("i", Leaf 9)])]

t3 :: SuffixTree
t3 = Node [("a",Node [("s",Leaf 4),
                      ("na",Node [("s",Leaf 2),
                                  ("nas",Leaf 0)])]),
           ("na",Node [("s",Leaf 3),("nas",Leaf 1)]),
           ("s",Leaf 5)]
