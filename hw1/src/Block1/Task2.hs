module Block1.Task2
  ( Nat(..)
  , intToNat
  , isEven
  , divNat
  , natToInt
  , modNat
  , mulNat
  , subNat
  , sumNat
  ) where

-- | type of Church numbers 
data Nat
  = Z
  | S Nat
  deriving (Show)

-- | addition of two Church numbers
sumNat :: Nat -> Nat -> Nat
sumNat a Z     = a
sumNat a (S b) = S (sumNat a b)

-- | multiplication of two Church numbers
mulNat :: Nat -> Nat -> Nat
mulNat _ Z     = Z
mulNat a (S b) = sumNat a (mulNat a b)

-- | subtraction of two Church numbers
subNat :: Nat -> Nat -> Nat
subNat a Z         = a
subNat Z _         = Z
subNat (S a) (S b) = subNat a b

-- | converse Church Number to Int
natToInt :: Nat -> Int
natToInt Z     = 0
natToInt (S a) = natToInt a + 1

-- | converse Int to Church Number
intToNat :: Int -> Nat
intToNat 0 = Z
intToNat n = S (intToNat $ n - 1)

-- | parity check of Church Number
isEven :: Nat -> Bool
isEven n = natToInt n `mod` 2 == 0

-- | division two Church Numbers
-- | return pair of the integer part and remainder
divModNat :: Nat -> Nat -> (Nat, Nat)
divModNat a b
  | a < b = (Z, a)
  | otherwise = (S (fst ans), snd ans)
  where
    ans = divModNat (subNat a b) b

-- | division two Church Numbers
-- | return the integer part
divNat :: Nat -> Nat -> Nat
divNat a b = fst $ divModNat a b

-- | division two Church Numbers
-- | return the remainder
modNat :: Nat -> Nat -> Nat
modNat a b = snd $ divModNat a b

-- | set equivalence of two Church Numbers
instance Eq Nat where
  Z == Z = True
  Z == _ = False
  _ == Z = False
  (S a) == (S b) = a == b

-- | compare of two Church Numbers
instance Ord Nat where
  Z <= Z = True
  Z <= _ = True
  _ <= Z = False
  (S a) <= (S b) = a <= b
