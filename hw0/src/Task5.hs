module Task5 
  ( churchPlus
  , churchMult
  , churchToInt
  , succChurch
  , zero
  )
  where

type Nat a = (a -> a) -> a -> a

-- | Zero is \f x -> x
zero :: Nat a
zero f x = x

-- | \n f x -> f (n f x)
succChurch :: Nat a -> Nat a
succChurch n f x = f (n f x)

-- | Addition Curch Numeral.
-- | \a b f x -> a f (b f x)
churchPlus :: Nat a -> Nat a -> Nat a
churchPlus a b f x = a f (b f x)

-- | Multiplication Curch Numeral.
-- | \a b f -> a (b f)
churchMult :: Nat a -> Nat a -> Nat a
churchMult a b f = a (b f)

-- | Convert a Nat to an Integer.
-- | This function add 1 to 0 n times
churchToInt :: Nat Integer -> Integer
churchToInt n = n (1 +) 0