module HW0.T5
  ( nz
  , ns
  , nplus
  , nmult
  , nFromNatural
  , nToNum
  , Nat
  ) where

import GHC.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ a = a 

ns :: Nat a -> Nat a
ns func f = f . func f

nplus, nmult :: Nat a -> Nat a -> Nat a
nplus a b f x = a f (b f x)
nmult a b = a . b

nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz
nFromNatural n = ns (nFromNatural (n - 1))

nToNum :: Num a => Nat a -> a
nToNum nat = nat (+1) 0