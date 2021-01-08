{-# OPTIONS_GHC -Wall #-}
module Lab08 where

-------------------
-- Ordered Trees --
-------------------

import Data.Tree

data BTree a = BEmpty | BNode a (BTree a) (BTree a)
               deriving (Show, Eq)

-- Task 1 -----------------------------------------
dfsForest ::  Forest a -> [a]
dfsForest [] = []
dfsForest ((Node a sons):next) = a : dfsForest sons ++ dfsForest next

-- Task 2 -----------------------------------------
bfsForest ::  Forest a -> [a]
bfsForest [] = []
bfsForest forest = [treeRoot x | x <- forest] ++ bfsForest (nextLayer forest)

treeRoot :: Tree a -> a
treeRoot (Node x _) = x

treeSons :: Tree a -> Forest a
treeSons (Node _ x) = x

nextLayer :: Forest a -> Forest a
nextLayer [] = []
nextLayer (x:next) = treeSons x ++ nextLayer next

-- Task 3 -----------------------------------------
isInTree :: (Eq a) => Tree a -> Tree a -> Bool
isInTree tr1 tr2@(Node _ sons) | tr1 == tr2 = True
                               | sons == [] = False
                               | otherwise = elem True $ map (isInTree tr1) sons

-- Task 4 -----------------------------------------
toBTree :: Forest a -> BTree a
toBTree [] = BEmpty
toBTree (Node val sons : next) = BNode val (toBTree sons) (toBTree next)

-- Task 5 -----------------------------------------
fromBTree :: BTree a -> Forest a
fromBTree BEmpty = []
fromBTree (BNode val left right) = Node val (fromBTree left) : (fromBTree right)

-- Task 6 -----------------------------------------
isSearch :: (Ord a) => BTree a -> Bool
isSearch BEmpty = True
isSearch (BNode v l r) = less l v && greater r v
    where less :: (Ord a) => BTree a -> a -> Bool
          less (BNode val left right) maxV = val <= maxV
              && less left val && between right val maxV
          less BEmpty _ = True
          greater :: (Ord a) => BTree a -> a -> Bool
          greater (BNode val left right) minV = val > minV
              && between left minV val && greater right val
          greater BEmpty _ = True
          between :: (Ord a) => BTree a -> a -> a -> Bool
          between (BNode val left right) minV maxV = val > minV && val <= maxV
              && between left minV val && between right val maxV
          between BEmpty _ _ = True

-- Task 7  ----------------------------------------
elemSearch :: (Ord a) => BTree a -> a -> Bool
elemSearch BEmpty _ = False
elemSearch (BNode val left right) x | x == val = True
                                    | x < val = elemSearch left x
                                    | otherwise = elemSearch right x

-- Task 8 -----------------------------------------
insSearch :: (Ord a) => BTree a -> a -> BTree a
insSearch (BNode val left right) x | x <= val = BNode val (insSearch left x) right
                                   | otherwise = BNode val left (insSearch right x)
insSearch BEmpty x = BNode x BEmpty BEmpty

-- Task 9 -----------------------------------------
delSearch :: (Ord a) => BTree a -> a -> BTree a
delSearch BEmpty _ = BEmpty
delSearch tree@(BNode val left right) x | x < val = BNode val (delSearch left x) right
                                        | x > val = BNode val left (delSearch right x)
                                        | otherwise = delRoot tree
    where delRoot :: (Ord a) => BTree a -> BTree a
          delRoot BEmpty = BEmpty
          delRoot (BNode _ BEmpty r) = r
          delRoot (BNode _ l BEmpty) = l
          delRoot (BNode _ l r) = let biggest = findBiggest l in
                                  BNode biggest (delSearch l biggest) r
          findBiggest :: (Ord a) => BTree a -> a
          findBiggest (BNode v _ r) | r == BEmpty = v -- the BTree will never be BEmpty because of the previous check.
                                    | otherwise = findBiggest r

-- Task 10 ----------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList list = readBSearchTree $ insAllSearch BEmpty list

insAllSearch :: (Ord a) => BTree a -> [a] -> BTree a
insAllSearch tree [] = tree
insAllSearch tree (x:xs) = insAllSearch (insSearch tree x) xs

readBSearchTree :: (Ord a) => BTree a -> [a]
readBSearchTree BEmpty = []
readBSearchTree (BNode val left right) = readBSearchTree left ++
                                         (val : readBSearchTree right)

-- Test Data --------------------------------------
otx :: [Tree Int]
otx = [Node 1 [Node 2 [],
                Node 3 [Node 10 []]] ,
        Node 4 [Node 5 [Node 8 []],
                Node 6 [Node 9 []],
                Node 7 []]
       ]

bt, bt1, bt2 :: BTree Int
bt = BNode 1 (BNode 2 BEmpty
                      (BNode 3 (BNode 10 BEmpty BEmpty)
                                BEmpty)
             )
             (BNode 4 (BNode 5 (BNode 8  BEmpty BEmpty)
                               (BNode 6  (BNode 9 BEmpty BEmpty)
                                         (BNode 7 BEmpty BEmpty)
                               )
                      )
              BEmpty
             )
bt1 = BNode 9 (BNode 4 BEmpty
                       (BNode 8 BEmpty BEmpty))
              (BNode 20 (BNode 10 BEmpty BEmpty)
                        BEmpty)
bt2 = BNode 9 (BNode 4 BEmpty
                       (BNode 8 BEmpty
                                (BNode 9 BEmpty BEmpty)))
              (BNode 20 (BNode 10 BEmpty BEmpty)
                        BEmpty)
