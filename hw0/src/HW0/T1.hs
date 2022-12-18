{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( flipIso
  , runIso
  , distrib
  , assocPair
  , assocEither
  , type(<->)(..)
  ) where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a)       = (Left a, Left a) 
distrib (Right (b, c)) = (Right b, Right c)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso leftSwap rightSwap
  where
    leftSwap (a, (b, c))  = ((a, b), c)
    rightSwap ((a, b), c) = (a, (b, c))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso leftSwap rightSwap
  where
    leftSwap (Left a)          = Left (Left a)
    leftSwap (Right (Left b))  = Left (Right b)
    leftSwap (Right (Right c)) = Right c 
    rightSwap (Left (Left a))  = Left a
    rightSwap (Left (Right b)) = Right (Left b)
    rightSwap (Right c)        = Right (Right c) 