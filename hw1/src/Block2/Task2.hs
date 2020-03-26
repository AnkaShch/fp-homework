module Block2.Task2
  (
    MyList (..)
    , splitOn
  ) where

data MyList =
  String :| [String]
  deriving (Eq, Show)

splitOn :: Char -> String -> MyList
splitOn sep = foldr func ("" :| [])
  where
    func ch (x :| xs)
      | ch == sep = [] :| (x : xs)
      | otherwise = (ch : x) :| xs

--joinWith :: Char -> MyList -> String
--joinWith sep (st:|strings) = foldr func st [""]
--  where
--    func elm str  = str ++ [sep] ++ elm