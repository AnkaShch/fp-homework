module Block3.Task1
  ( eitherConcat
  , maybeConcat
  ) where

import Data.Maybe

-- | concat all sublist from [Maybe a]
-- >> maybeConcat [Just [1,2,3], Nothing, Just [4,5]]
-- << [1,2,3,4,5]
maybeConcat :: Monoid a => [Maybe a] -> a
maybeConcat [] = mempty
maybeConcat (x:xs)
  | isJust x   = fromJust x <> maybeConcat xs
  | otherwise  = maybeConcat xs

-- | concat all Left and all Right from [Either a b] and return pair of result
-- >> [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]
-- << (Sum {getSum = 8}, [1,2,3,4,5])
eitherConcat :: (Monoid left, Monoid right) => [Either left right] -> (left, right)
eitherConcat []           = (mempty, mempty)
eitherConcat (Left x:xs)  = (x, mempty) <> eitherConcat xs
eitherConcat (Right x:xs) = (mempty, x) <> eitherConcat xs
