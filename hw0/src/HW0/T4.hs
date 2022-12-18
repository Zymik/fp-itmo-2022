{-# LANGUAGE LambdaCase #-}
module HW0.T4
  ( repeat'
  , map'
  , fix
  , fac
  , fib
  ) where

import GHC.Natural (Natural)
import Data.Function (fix)
  
repeat' :: a -> [a]  
repeat' x = fix (x:)

mapping :: (a -> b) -> ([a] -> [b]) -> [a] -> [b]
mapping func recursion = \case
  h:t -> func h:recursion t
  _   -> []

map' :: (a -> b) -> [a] -> [b]
map' func = fix $ mapping func

fac :: Natural -> Natural
fac = fix (\recursion n -> if n <= 1 then 1 else n * recursion (n - 1))

fib :: Natural -> Natural
fib = fix (\recursion n -> case n of
  0 -> 0
  1 -> 1
  _ -> recursion (n - 1) + recursion (n - 2)
 )
