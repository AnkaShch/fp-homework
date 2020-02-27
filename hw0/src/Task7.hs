module Task7
  ( expr1
  ) where

expr1 :: Bool
expr1 = null op4 headOfMapArr
  where
    name1, name2 :: String
    name1 = "Dorian "
    name2 = " Grey"

    arr :: [([Char] -> [Char], [Char])]
    arr = [(op1 name1, name2)]

    mapArr :: [String]
    mapArr = map (uncurry id) arr

    head :: [String] -> String

    headOfMapArr :: String
    headOfMapArr =  head op3 mapArr

    null :: [a] -> Bool
    null [] = True
    null _  = False

    op1 :: String -> String -> String
    op1 = (++)
    op2 :: (String -> Stirng, String) -> Stirng
    op2 = (uncurry id)
    op3 :: ([String] -> String) -> [String] -> String
    op3 = ($)
    op4 = (b -> Bool) -> (String -> b) -> String -> Bool
    op4 = (.)


  


