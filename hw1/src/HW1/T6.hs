module HW1.T6
  ( mcat
  , epart
  ) where

import Data.Foldable

mcat :: Monoid a => [Maybe a] -> a
mcat = foldMap fold

eitherToPair :: (Monoid a, Monoid b) => Either a b -> (a, b)
eitherToPair (Left a1)  = (a1, mempty)
eitherToPair (Right b1) = (mempty, b1)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap eitherToPair