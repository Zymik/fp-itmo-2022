module HW2.T5
    ( ExceptState(..)
    , EvaluationError(..)
    , mapExceptState
    , wrapExceptState
    , joinExceptState
    , modifyExceptState
    , throwExceptState
    , eval
    ) where

import HW2.T1
import Data.Function
import HW2.T2 (wrapExcept)
import HW2.T3 (joinExcept)
import qualified Control.Monad
import HW2.T4 (Expr(..), Prim(..))
import Control.Monad (when)

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }


mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f state = ES $ \s -> runES state s & mapExcept (mapAnnotated f)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> wrapExcept (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState expectState = ES $ \s -> let expect = runES expectState s
                                            in joinExcept $ expect 
                                                            & mapExcept (\(state :# st) -> runES state st)
                                        
modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> wrapExcept (() :# f s) 

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState s e) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState s e) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero
    deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double

eval (Val d) = pure d

eval (Op (Add a b)) =
    do  leftValue  <- eval a
        rightValue <- eval b
        modifyExceptState (Add leftValue rightValue : )
        return $ leftValue + rightValue

eval (Op (Sub a b)) =
    do  leftValue  <- eval a
        rightValue <- eval b
        modifyExceptState (Sub leftValue rightValue : )
        return $ leftValue - rightValue

eval (Op (Mul a b)) =
    do  leftValue  <- eval a
        rightValue <- eval b
        modifyExceptState (Mul leftValue rightValue : )
        return $ leftValue * rightValue

eval (Op (Div a b)) =
    do  leftValue  <- eval a
        rightValue <- eval b
        
        when (rightValue == 0) 
             (throwExceptState DivideByZero)
            
        modifyExceptState (Div leftValue rightValue : )
        return $ leftValue / rightValue

eval (Op (Sgn a)) =
    do  value <- eval a
        modifyExceptState (Sgn value : )
        return $ signum value

eval (Op (Abs a)) =
    do  value <- eval a
        modifyExceptState (Abs value : )
        return $ abs value
