module Task6 
  ( expr1Ans
  , expr2Ans
  ) where

-- | The first task in WHNF
expr1Ans :: (Either String b, Either String c)
expr1Ans = 
  ( Left ("harold" ++ " hide " ++ "the " ++ "pain")
  , Left ("harold" ++ " hide " ++ "the " ++ "pain")
  )

-- | The seStringond task in WHNF
-- I calculated the value of the expression by my hands and I got False
expr2Ans :: Bool
expr2Ans = False