module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , ncmp
  , nFromNatural
  , nToNum
  -- * Advanced
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import GHC.Natural

data N
  = Z
  | S N

nplus :: N -> N -> N 
nplus n Z     = n
nplus n (S m) = nplus (S n) m

nmult :: N -> N -> N  
nmult Z _     = Z
nmult _ Z     = Z
nmult n (S m) = nplus n (nmult m n)

nsub :: N -> N -> Maybe N
nsub Z (S _)     = Nothing
nsub n Z         = Just n
nsub (S n) (S m) = nsub n m

ncmp :: N -> N -> Ordering 
ncmp Z Z         = EQ
ncmp Z _         = LT
ncmp _ Z         = GT
ncmp (S n) (S m) = ncmp n m

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural $ n - 1)

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S n) = 1 + nToNum n

nEven, nOdd :: N -> Bool    -- parity checking
nEven Z         = True
nEven (S Z)     = False
nEven (S (S n)) = nEven n

nOdd = not . nEven 

ndiv :: N -> N -> N         -- integer division
ndiv n m = let sub = nsub n m in
    case sub of
        (Just value) -> S (ndiv value m)
        _            -> Z

nmod :: N -> N -> N         -- modulo operation
nmod n m = let sub = nsub n m in
    case sub of
        (Just value) -> nmod value m
        _            -> n