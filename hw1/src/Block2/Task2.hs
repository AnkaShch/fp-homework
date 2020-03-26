module Block2.Task2
  (
    MyList (..)
    , splitOn
  ) where

-- | Type of NonEmpty strings list
data MyList =
  String :| [String]
  deriving (Eq, Show)

-- | split string by separator
-- | return NonEmpty strings list with result
splitOn :: Char -> String -> MyList
splitOn sep = foldr func ("" :| [])
  where
    func ch (x :| xs)
      | ch == sep = [] :| (x : xs)
      | otherwise = (ch : x) :| xs