{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
module HW2.T2
    ( distOption
    , distPair
    , distQuad
    , distAnnotated
    , distExcept
    , prioritisedLeftPair
    , prioritisedRightPair
    , distPrioritised
    , distStream
    , distList
    , distFun
    , wrapOption
    , wrapPair
    , wrapQuad
    , wrapAnnotated
    , wrapExcept
    , wrapPrioritised
    , wrapStream
    , wrapList
    , wrapFun
    , (!+!)
    ) where

import HW2.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b)  = Some (a, b)
distOption (_, _)            = None

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a1 a2, P b1 b2) = P (a1, b1) (a2, b2)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# e1, b :# e2) = (a, b) :# (e1 <> e2)

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e
distExcept (Success a, Success b) = Success (a, b)

prioritisedLeftPair :: Prioritised a -> b -> Prioritised (a, b)
prioritisedLeftPair prioritised b = mapPrioritised ( , b) prioritised

prioritisedRightPair :: b -> Prioritised a -> Prioritised (b, a)
prioritisedRightPair b = mapPrioritised (b, )

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (Low a, b)    = prioritisedRightPair a b
distPrioritised (a, Low b)    = prioritisedLeftPair  a b
distPrioritised (Medium a, b) = prioritisedRightPair a b
distPrioritised (a, Medium b) = prioritisedLeftPair  a b
distPrioritised (High a, b)   = prioritisedRightPair a b


distStream  :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> streamA, b :> streamB) = (a, b) :> distStream (streamA, streamB)

infixl 5 !+!
(!+!) ::  List a -> List a -> List a
(h :. t) !+! right = h :. (t !+! right)
Nil !+! right = right

distList :: (List a, List b) -> List (a, b)
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (a :. t, listB) = mapList (a,) listB !+! distList (t, listB)

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F fa, F fb) = F \i -> (fa i, fb i)


wrapOption :: a -> Option a
wrapOption = Some

wrapPair :: a -> Pair a
wrapPair a = P a a

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

wrapExcept :: a -> Except e a
wrapExcept = Success

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

wrapList :: a -> List a
wrapList a = a :. Nil

wrapFun :: a -> Fun i a
wrapFun = F . const