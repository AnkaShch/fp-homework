{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  ) where

-- | This function demonstrate type Either.
-- | If only left is given, it constructs a pair from the left element.
-- | If there is a pair on the right, it will return this pair.
distributivity :: Either a (b, c) -> (Either a b, Either a c)
distributivity (Left x)  = (Left x, Left x)
distributivity (Right y) = (Right (fst y), Right (snd y))

-- | This function demonstrate associativity
associator :: (a, (b, c)) -> ((a, b), c)
associator arr = ((fst arr, fst $ snd arr), snd $ snd arr)

type (<->) a b = (a -> b, b -> a)

-- | This function demonstrate type equivalence.
-- | It accepts expressions a and b.
-- | Returns a pair (a -> b, b -> a)
eitherAssoc :: Either a (Either b c) <-> Either (Either a b) c
eitherAssoc = (aToB, bToA)
  where
    aToB :: Either a (Either b c) -> Either (Either a b) c
    aToB (Right (Left x))   = Left (Right x)
    aToB (Right (Right x))  = Right x
    aToB (Left x)           = Left (Left x)

    bToA :: Either (Either a b) c -> Either a (Either b c)
    bToA (Left (Left x))    = Left x
    bToA (Left (Right x))   = Right (Left x)
    bToA (Right x)          = Right (Right x)
