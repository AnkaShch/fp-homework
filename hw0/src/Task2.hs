module Task2 
  ( doubleNeg
  , doubleNegElim
  , excludedNeg
  , pierce
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

-- | a -> (a -> Void) -> Void
doubleNeg :: a -> Neg (Neg a)
doubleNeg x f = f x

 -- | ((Either a (a -> Void)) -> Void) -> Void
excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg x = x (Right (x . Left))


-- | This expression cannot be proved
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- | This expression cannot be proved
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

-- | (((a -> Void) -> Void) -> Void) -> (a -> Void)
thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim f x = f (doubleNeg x)