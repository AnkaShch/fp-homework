module Task4 
  ( iterateElement
  , mapFix
  , factorial
  , fibonacci
  ) where

import Data.Function (fix)

-- | Construct an infinite array of repeating elements
-- iterateElement x == [x, x..]
iterateElement :: a -> [a]
iterateElement =
  fix $ \rec n ->
  n : rec n

-- | Find the nth Fibonacci number
fibonacci :: Integer -> Integer
fibonacci =
  fix $ \rec n ->
    if n == 1 || n == 2 
    then 1 
    else rec (n - 1) + rec (n - 2)

-- | Find the factorial of n
factorial :: Integer -> Integer
factorial =
  fix $ \rec n ->
    if n == 0
    then 1
    else rec (n - 1) * n

-- | Apply the function to each element of the array
mapFix :: (a -> b) -> [a] -> [b]
mapFix =
  fix $ \rec f arr ->
    case arr of
      [] -> []
      x : xs -> f x : rec f xs