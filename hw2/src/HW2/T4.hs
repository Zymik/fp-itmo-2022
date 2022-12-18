{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE BlockArguments #-}
module HW2.T4 
    ( State(..)
    , mapState
    , joinState
    , wrapState
    , Prim(..)
    , Expr(..)
    , eval
    ) where
  
import HW2.T1 ( mapAnnotated, Annotated(..) )
import Data.Function
import qualified Control.Monad

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f state = S $ \s -> runS state s & mapAnnotated f

wrapState :: a -> State s a
wrapState a = S $ \s -> a :# s

joinState :: State s (State s a) -> State s a
joinState state = S $
                    \s -> let innerState :# annotation = runS state s
                            in runS innerState annotation


modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum
  deriving Show 

data Expr = Val Double | Op (Prim Expr)
   deriving Show 

instance Num Expr where
  (+) x y = Op (Add x y)
  (*) x y = Op (Mul x y)
  abs = Op . Abs
  signum = Op . Sgn
  fromInteger = Val . fromInteger
  (-) x y = Op (Sub x y)

instance Fractional Expr where
  fromRational = Val . fromRational
  (/) x y = Op (Div x y) 

eval :: Expr -> State [Prim Double] Double
eval (Val d) = pure d

eval (Op (Add a b)) =
    do  leftValue <- eval a
        rightValue <- eval b
        modifyState (Add leftValue rightValue : )
        return $ leftValue + rightValue

eval (Op (Sub a b)) =
    do  leftValue <- eval a
        rightValue <- eval b
        modifyState (Sub leftValue rightValue : )
        return $ leftValue - rightValue

eval (Op (Mul a b)) =
    do  leftValue <- eval a
        rightValue <- eval b
        modifyState (Mul leftValue rightValue : )
        return $ leftValue * rightValue

eval (Op (Div a b)) =
    do  leftValue <- eval a
        rightValue <- eval b
        modifyState (Div leftValue rightValue : )
        return $ leftValue / rightValue

eval (Op (Sgn a)) =
    do  value <- eval a
        modifyState (Sgn value : )
        return $ signum value

eval (Op (Abs a)) =
    do  value <- eval a
        modifyState (Abs value : )
        return $ abs value
      