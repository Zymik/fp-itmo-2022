module HW0.T6 where

import HW0.T1 (distrib)
import Data.Char (isSpace)

a :: (Either [Char] b, Either [Char] c)
a = distrib (Left ("AB" ++ "CD" ++ "EF"))     -- distrib from HW0.T1

b :: [Bool]
b = map isSpace "Hello, World"

c :: String
c = if 1 > 0 || error "X" then "Y" else "Z"

a_whnf :: (Either [Char] b, Either [Char] c)
a_whnf = (left, left)
  where left = (Left ("AB" ++ "CD" ++ "EF"))

b_whnf :: [Bool]
b_whnf = (isSpace 'H') : (map isSpace "ello, World")

c_whnf :: String
c_whnf = "Y"