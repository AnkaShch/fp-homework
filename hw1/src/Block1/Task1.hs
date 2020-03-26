module Block1.Task1
  ( Day(..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

nextDay :: Day -> Day
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

afterDays :: Day -> Integer -> Day
afterDays day 0 = day
afterDays day n = afterDays (nextDay day) (n - 1)

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False

daysToParty :: Day -> Integer
daysToParty Friday = 0
daysToParty day = daysToParty (nextDay day) + 1

instance Eq Day where
  Monday == Monday = True
  Tuesday == Tuesday = True
  Wednesday == Wednesday = True
  Thursday == Thursday = True
  Friday == Friday = True
  Saturday == Saturday = True
  Sunday == Sunday = True
  _ == _ = False
