module Task5 where

type Nat a = (a -> a) -> a -> a

zero :: Nat a
zero f x = x

succChurch :: Nat a -> Nat a
succChurch n f x = f (n f x)

churchPlus :: Nat a -> Nat a -> Nat a
churchPlus a b f x = a f (b f x)

churchMult :: Nat a -> Nat a -> Nat a
churchMult a b f = a (b f)

churchToInt :: Nat Integer -> Integer
churchToInt n = n (1 +) 0