module Task3 
  ( composition
  , contraction
  , identity
  , permutation
  ) where

-- | It's S combinator
s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

-- | It's B basis
composition :: (b -> c) -> (a -> b) -> a -> c
composition = s (const s) const

-- | It's K basis
identity :: a -> a
identity = s const const

-- | It's W basis
contraction :: (a -> a -> b) -> a -> b
contraction = s s (s const)

-- | It's C basis
permutation :: (a -> b -> c) -> b -> a -> c
permutation = s (s (const (s (const s) const)) s) (const const)