module HW1.T5
  ( splitOn
  , joinWith
  ) where

import Data.List.NonEmpty

data SplitAccumulator a = Accumulator a (NonEmpty [a])

started :: NonEmpty [a]
started = []:|[]

emptyAccumulator :: a -> SplitAccumulator a
emptyAccumulator = flip Accumulator started

foldFunction :: Eq a => a -> SplitAccumulator a -> SplitAccumulator a
foldFunction a (Accumulator sep (h:|t)) =
    if a == sep
    then Accumulator sep ([]:|h:t)
    else Accumulator sep ((a:h):|t)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep list = case foldr foldFunction (emptyAccumulator sep) list of
    (Accumulator _ answer) -> answer

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (list:|[]) = list
joinWith s (h:|(t:v)) = h ++ [s] ++ joinWith s (t:|v)