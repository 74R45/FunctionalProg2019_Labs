{-# OPTIONS_GHC -Wall #-}
module Lab07 where

-----------------------------------
-- Streams and Fibonacci Numbers --
-----------------------------------

data Stream a = Cons a (Stream a)

-- Shows first 20 elements
instance Show a => Show (Stream a) where
    show xs = (foldl (++) "[" $ map (\x -> show x ++ ", ") $
                  take 20 $ streamToList xs) ++ "..."

-- Task 1 -----------------------------------------
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Task 2 -----------------------------------------
instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Task 3 -----------------------------------------
sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) (Cons y ys) = Cons x $ Cons y (sInterleave xs ys)

sTake :: Int -> Stream a -> [a]
sTake n xs = take n $ streamToList xs

-- Task 4 -----------------------------------------
nats :: Stream Integer
nats = sIterate (+1) 0

-- Task 5 -----------------------------------------
ruler :: Stream Integer
ruler = fmap numOfMultiplesOfTwo (sTail nats)

sTail :: Stream a -> Stream a
sTail (Cons _ xs) = xs

numOfMultiplesOfTwo :: Integer -> Integer
numOfMultiplesOfTwo x | x == 0 || mod x 2 == 1 = 0
                      | otherwise = numOfMultiplesOfTwo (div x 2) + 1

-- Task 6 -----------------------------------------
rand :: Integer -> Stream Integer
rand = sIterate ((`mod` 2147483648).(+ 12345).(* 1103515245))

-- Task 7 -----------------------------------------
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n | n < 0 = 0
      | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Task 8 -----------------------------------------
fibs2 :: [Integer]
fibs2 = 1 : 1 : fibs2Recursive 1 1
    where fibs2Recursive :: Integer -> Integer -> [Integer]
          fibs2Recursive x y = (x+y) : fibs2Recursive y (x+y)

-- Task 9 -----------------------------------------
data  Matrix a = M(a,a)(a,a)
         deriving (Show, Eq, Ord)

instance Num a => Num (Matrix a) where
    (+) (M(a1,b1)(c1,d1)) (M(a2,b2)(c2,d2)) = M(a1+a2,b1+b2)(c1+c2,d1+d2)
    (*) (M(a1,b1)(c1,d1)) (M(a2,b2)(c2,d2)) = M(a1*a2 + b1*c2, a1*b2 + b1*d2)
                                               (c1*a2 + d1*c2, c1*b2 + d1*d2)
    negate (M(x1,y1)(x2,y2)) = M(-x1,-y1)(-x2,-y2)
    fromInteger x = M(fromInteger x,0)(0,fromInteger x)
    -- No sensible solutions
    abs = undefined
    signum = undefined

-- Task 10 ----------------------------------------
fastFib :: Integer -> Integer
fastFib n = topLeft $ M(1,1)(1,0) ^ n

topLeft :: Matrix a -> a
topLeft (M(x,_) _) = x
