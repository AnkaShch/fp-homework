module Task6 
  ( expr1
  , expr1Ans
  , expr2
  , expr2Ans
  ) where

import Data.Maybe (mapMaybe)

-- | The first task
expr1 :: Either a (b, c) -> (Either a b, Either a c)
expr1 = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- | The first task in WHNF
expr1Ans :: Either a (b, c) -> (Either a b, Either a c)
expr1Ans = 
  ( Left ("harold" ++ " hide " ++ "the " ++ "pain")
  , Left ("harold" ++ " hide " ++ "the " ++ "pain")
  )

-- | The second task
expr2 :: Bool
expr2 = null $ mapMaybe foo "pole chudes ochen' chudesno"

-- | The second task in WHNF
-- I calculated the value of the expression by my hands and I got False
expr2 :: Bool
expr2Ans = False

foo :: Char -> Maybe Double
foo char =
    case char == 'o' of
      True -> Just $ exp pi
      False -> Nothing

-- not actually, but good enough for this task
null :: [a] -> Bool
null [] = True
null _  = False

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) =
 let rs = mapMaybe f xs in
 case f x of
  Nothing -> rs
  Just r  -> r:rs