{-# LANGUAGE BlockArguments #-}
module HW2.T3 
    ( joinOption
    , joinExcept
    , joinAnnotated
    , joinList
    , joinFun
    ) where
        
import HW2.T1
import HW2.T2 ((!+!))

joinOption    :: Option (Option a) -> Option a
joinOption (Some (Some a)) = Some a
joinOption _               = None

joinExcept    :: Except e (Except e a) -> Except e a
joinExcept (Success inner) = inner
joinExcept (Error e)       = Error e

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# e2 <> e1

joinList      :: List (List a) -> List a
joinList Nil = Nil
joinList (h :. t) = h !+! joinList t

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F $ \i -> case f i of 
                            F g -> g i