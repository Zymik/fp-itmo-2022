module HW1.T4
  ( tfoldr
  , treeToList
  ) where

import HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ base Leaf = base 
tfoldr func base (Branch _ left a right) = 
    let rightTreeFold = tfoldr func base right
        partFolded    = func a rightTreeFold
    in
        tfoldr func partFolded left

treeToList :: Tree a -> [a]    -- output list is sorted
treeToList = tfoldr (:) []