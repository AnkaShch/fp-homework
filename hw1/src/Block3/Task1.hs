module Block3.Task1 (
  maybeConcat
) where

import Data.Maybe

maybeConcat :: Monoid a => [Maybe a] -> a
maybeConcat [] = mempty
maybeConcat [x]
  | isJust x = fromJust x
  | otherwise = mempty
maybeConcat (x : xs)
  | isJust x  = fromJust x <> maybeConcat xs
  | otherwise = maybeConcat xs
