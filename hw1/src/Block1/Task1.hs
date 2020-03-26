module Block1.Task1
  (
    Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

-- | type of days
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving Show

-- | function, which return next day after taken
nextDay :: Day -> Day
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday
nextDay Sunday    = Monday

-- | function, which return day after n days
afterDays :: Day -> Integer -> Day
afterDays day 0 = day
afterDays day n = afterDays (nextDay day) (n - 1)

-- | check is it day off
isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | counts days until Friday
daysToParty :: Day -> Integer
daysToParty Friday = 0
daysToParty day    = daysToParty (nextDay day) + 1

-- | set equivalence
instance Eq Day where
  Monday == Monday       = True
  Tuesday == Tuesday     = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Friday == Friday       = True
  Saturday == Saturday   = True
  Sunday == Sunday       = True
  _ == _                 = False