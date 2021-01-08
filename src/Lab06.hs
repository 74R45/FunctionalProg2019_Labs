{-# OPTIONS_GHC -Wall #-}
module Kreshchenko06 where

------------------
-- Type Classes --
------------------

newtype Poly a = P [a]

-- Task 1 -----------------------------------------
x :: Num a => Poly a
x = P [0,1]

-- Task 2 -----------------------------------------
instance (Num a, Eq a) => Eq (Poly a) where
    (==) p1 p2 = coef (normalize p1) == coef (normalize p2)

normalize :: (Num a, Eq a) => Poly a -> Poly a
normalize (P []) = (P [])
normalize (P ns) | last ns == 0 = normalize $ P (init ns)
                 | otherwise = P ns

coef :: Num a => Poly a -> [a]
coef (P p) = p

-- Task 3 -----------------------------------------
instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P p) | null xs = "0"
               | otherwise = foldl1 (\l r -> l++" + "++r) xs
        where xs = filter (/= "") [showStep1 (p!!y) y | y <- [(length p)-1,(length p)-2..0]]
              showStep1 :: (Num b, Eq b, Show b) => b -> Int -> String
              showStep1 0 _ = ""
              showStep1 1 0 = "1"
              showStep1 1 e = showStep2 e
              showStep1 c e = show c ++ showStep2 e
              showStep2 :: Int -> String
              showStep2 0 = ""
              showStep2 1 = "x"
              showStep2 e = "x^" ++ show e

-- Task 4 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P []) p = p
plus p (P []) = p
plus (P (pp1:p1)) (P (pp2:p2)) = P (pp1+pp2 : (coef $ P p1 + P p2))

-- Task 5 -----------------------------------------
times :: Num a => Poly a -> Poly a -> Poly a
times (P p1) p2 = sum [mult (p1!!e) e p2 | e <- [0..(length p1)-1]]
    where mult :: (Num a) => a -> Int -> Poly a -> Poly a
          mult c e (P p) = P $ replicate e 0 ++ (map (c*) p)

-- Task 6 -----------------------------------------
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P p) = P $ map ((-1)*) p
    fromInteger n = P [a] where a = fromInteger n
    -- No sensible solutions
    abs = undefined
    signum = undefined

-- Task 7 -----------------------------------------
applyP :: Num a => Poly a -> a -> a
applyP (P p) y = sum [p!!e * (y^e) | e <- [0..(length p)-1]]

-- Task 8 -----------------------------------------
class Num a => Differentiable a where
    deriv :: a -> a
    nderiv :: Int -> a -> a
    nderiv n a = iterate deriv a !! n

-- Task 9 -----------------------------------------
instance Num a => Differentiable (Poly a) where
    deriv (P p) = P [p!!e * fromIntegral e | e <- [1..(length p)-1]]
