{-# LANGUAGE LambdaCase #-}

module HW1.T1
  ( Day (..)
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  ) where

import GHC.Natural

data Day = Monday
           | Tuesday
           | Wednesday
           | Thursday
           | Friday
           | Saturday
           | Sunday
           deriving (Show)

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay = \case
    Monday    -> Tuesday
    Tuesday   -> Wednesday
    Wednesday -> Thursday
    Thursday  -> Friday
    Friday    -> Saturday
    Saturday  -> Sunday
    Sunday    -> Monday


-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays x day 
    | x > 7     = afterDays (x `mod` 7) day 
    | x == 0    = day
    | otherwise = afterDays (x - 1) (nextDay day) 
   
-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend = \case
    Saturday -> True
    Sunday   -> True
    _        -> False


-- | Computes the number of days until Friday.
daysToParty :: Day -> Natural
daysToParty = \case
    Friday -> 0
    day    -> daysToParty (nextDay day) + 1