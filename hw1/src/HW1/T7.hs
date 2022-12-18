module HW1.T7
  ( ListPlus (..)
  , Inclusive (..)
  , DotString (..)
  , Fun (..)
  ) where

data ListPlus a
  = a :+ ListPlus a
  | Last a
    deriving(Show)

infixr 5 :+

instance Semigroup (ListPlus a) where  
    (<>) (Last a) right = a :+ right
    (<>) (h :+ t) right = h :+ (t <> right) 


data Inclusive a b = This a | That b | Both a b
    deriving(Show)

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where  
    (<>) (This aFirst) (This aSecond) = This $ aFirst <> aSecond
    (<>) (That bFirst) (That bSecond) = That $ bFirst <> bSecond
    
    (<>) (This aFirst) (That bSecond) = Both aFirst bSecond
    (<>) (That bFirst) (This aSecond) = Both aSecond bFirst

    (<>) (Both aFirst bFirst) (This aSecond) = Both (aFirst <> aSecond) bFirst
    (<>) (Both aFirst bFirst) (That bSecond) = Both aFirst (bFirst <> bSecond)

    (<>) (This aFirst) (Both aSecond bSecond) = Both (aFirst <> aSecond) bSecond
    (<>) (That bFirst) (Both aSecond bSecond) = Both aSecond (bFirst <> bSecond)
    
    (<>) (Both aFirst bFirst) (Both aSecond bSecond) = Both (aFirst <> aSecond) (bFirst <> bSecond)


newtype DotString = DS String
    deriving(Show)

instance Semigroup DotString where  
    (<>) (DS "") second         = second
    (<>) first (DS "")          = first 
    (<>) (DS first) (DS second) = DS $ first ++ "." ++ second

instance Monoid DotString where  
    mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where   
    (<>) (F a) (F b)= F $ a . b

instance Monoid (Fun a) where  
    mempty = F id
    