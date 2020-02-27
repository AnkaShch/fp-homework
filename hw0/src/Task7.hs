module Task7
  ( expr1
  ) where

import Prelude hiding (null)

expr1 :: Bool
expr1 = nullHead mapArr
  where
    name1, name2 :: String
    name1 = "Dorian "
    name2 = " Grey"

    arr :: [(String -> String, String)]
    arr = [(op1 name1, name2)]

    mapArr :: [String]
    mapArr = map op2 arr

    nullHead :: [String] -> Bool
    nullHead = op3 null head 

    null :: [a] -> Bool
    null [] = True
    null _  = False

    op1 :: String -> String -> String
    op1 = (++)
    op2 :: (String -> String, String) -> String
    op2 = (uncurry id)
    op3 :: (b -> Bool) -> ([String] -> b) -> [String] -> Bool
    op3 = (.)