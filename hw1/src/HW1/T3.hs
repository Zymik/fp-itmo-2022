{-# LANGUAGE LambdaCase #-}

module HW1.T3
  ( Tree (..)
  , Meta
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

data Tree a
  = Leaf
  | Branch Meta (Tree a) a (Tree a)

data Meta = M Int Int Int

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (M size _ _) _ _ _) = size

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (M _ depth _) _ _ _) = depth

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember a (Branch _ left value right) = case compare a value of
    EQ -> True
    LT -> tmember a left
    GT -> tmember a right

mkMeta :: Tree a -> Tree a -> Meta
mkMeta left right = M (tsize left + tsize right + 1) ((max (tdepth left) (tdepth right)) + 1) (tdepth left - tdepth right)

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch left a right = Branch (mkMeta left right) left a right

-- | balancing of tree, O(1)
balance :: Tree a -> Tree a
balance = \case 
    Branch (M _ _ (-2)) left a (Branch (M _ _ x) rightLeft b rightRight) | x == 0 || x == -1             ->
            mkBranch (mkBranch left a rightLeft) b rightRight
    
    Branch (M _ _ (-2)) left a (Branch (M _ _ 1) (Branch _ rightLeftLeft c rightLeftRight) b rightRight) ->
        mkBranch (mkBranch left a rightLeftLeft) c (mkBranch rightLeftRight b rightRight)

    Branch (M _ _ 2) (Branch (M _ _ x) leftLeft b leftRight) a right | x == 0 || x == 1                  ->   
            mkBranch leftLeft b (mkBranch leftRight a right)
    
    Branch (M _ _ 2) (Branch (M _ _ (-1)) leftLeft b (Branch _ leftRightLeft c leftRightRight)) a right  ->
        mkBranch (mkBranch leftLeft b leftRightLeft) c (mkBranch leftRightRight a right)

    tree -> tree  

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = mkBranch Leaf a Leaf
tinsert a (Branch m left value right) = case compare a value of
    EQ -> Branch m left value right

    LT -> let insertedLeft = tinsert a left in
        balance $ mkBranch insertedLeft value right

    GT -> let insertedRight = tinsert a right in
        balance $ mkBranch left value insertedRight


-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf


instance Show a => Show (Tree a) where
    show Leaf = ""
    show (Branch _ left a right) =  show a ++ " (" ++ show left ++ "), " ++ "(" ++ show right ++ ")"