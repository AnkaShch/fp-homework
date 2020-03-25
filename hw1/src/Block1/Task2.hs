module Block1.Task2
  (
    Nat (..)
  , intToNat
  , isEven
  , divNat
  , natToInt
  , modNat
  , mulNat
  , subNat
  , sumNat
  ) where

data Nat = Z | S Nat deriving Show

sumNat :: Nat -> Nat -> Nat
sumNat a Z     = a
sumNat a (S b) = S (sumNat a b)

mulNat :: Nat -> Nat -> Nat
mulNat _ Z     = Z
mulNat a (S b) = sumNat a (mulNat a b)

subNat :: Nat -> Nat -> Nat
subNat a Z         = a
subNat Z _         = Z
subNat (S a) (S b) = subNat a b

natToInt :: Nat -> Int
natToInt Z     = 0
natToInt (S a) = natToInt a + 1

intToNat :: Int -> Nat
intToNat 0 = Z
intToNat n = S (intToNat $ n - 1)

isEven :: Nat -> Bool
isEven n = natToInt n `mod` 2 == 0

divModNat :: Nat -> Nat -> (Nat, Nat)
divModNat a b
  | a < b = (Z, a)
  | otherwise = (S (fst ans), snd ans)
  where
    ans = divModNat (subNat a b) b

divNat :: Nat -> Nat -> Nat
divNat a b = fst $ divModNat a b

modNat :: Nat -> Nat -> Nat
modNat a b = snd $ divModNat a b

instance Eq Nat where
  Z == Z = True
  Z == _ = False
  _ == Z = False
  (S a) == (S b) = a == b

instance Ord Nat where
  Z <= Z = True
  Z <= _ = True
  _ <= Z = False
  (S a) <= (S b) = a <= b
