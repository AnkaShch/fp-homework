module Task4 where

import Data.Function (fix)

iterateElement :: a -> [a]
iterateElement = fix (\rec n -> n : rec n)

fibonacci :: Integer -> Integer
fibonacci = fix (\rec n -> if n == 1 || n == 2 then 1 else rec (n  - 1) + rec (n - 2))

factorial :: Integer -> Integer
factorial = fix (\rec n -> if n == 0 then 1 else rec (n - 1) * n)

mapFix :: (a -> b) -> [a] -> [b]
mapFix = fix (\rec f (x : xs) -> f x  : rec f xs)