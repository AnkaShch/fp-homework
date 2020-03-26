module Block3.Task2
  ( NonEmpty (..)
  , ThisOrThat (..)
  ) where

-- | type of NonEmpty list
data NonEmpty a = a :| [a] deriving Show

-- | operator (<>) concat 2 NonEmpty lists
instance Semigroup (NonEmpty a) where
  (l :| []) <> (r :| right) = l :| (r : right)
  (l :| left) <> (r :| right) = l :| (left <> (r : right))

-- | set equivalence of two NonEmpty lists
instance Ord a => Eq (NonEmpty a) where
  (l:|[]) == (r:|[])          = l == r
  (l :| left) == (r :| right) = (l == r) && (left == right)
  
data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving Show

-- | operator (<>) concat 2 ThisOrThat pair
instance Semigroup (ThisOrThat a b) where
  This left   <> This _       = This left
  This left   <> That right   = Both left right
  This left   <> Both _ right = Both left right
  That left   <> That _       = That left
  That left   <> This right   = Both right left
  That left   <> Both right _ = Both right left
  Both left _ <> That right   = Both left right
  Both _ left <> This right   = Both right left
  Both left _ <> Both _ right = Both left right
