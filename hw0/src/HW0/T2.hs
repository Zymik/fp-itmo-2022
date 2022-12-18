module HW0.T2
 ( Not
 , toAny
 , addNegationRule
 , doubleNeg
 , reduceTripleNeg
 ) where

import Data.Void (Void, absurd)


type Not a = a -> Void

toAny :: a -> Not a -> b
toAny a f = absurd (f a)

addNegationRule :: (a -> b) -> (a -> Not b) -> Not a
addNegationRule f1 f2 a = f2 a (f1 a)

doubleNeg :: a -> Not (Not a)
doubleNeg a = addNegationRule (toAny a) (toAny a)

reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg tripleNot = addNegationRule doubleNeg (const tripleNot) 