{-# LANGUAGE InstanceSigs #-}

module GeeksForGeeksProblems.Level2_21 where

-- https://practice.geeksforgeeks.org/problems/check-for-bst/1

import GeeksForGeeksProblems.Types ( Tree(..) )

-- | quick solution doesn't take advantage of BST assuming its built correctly, so foldl does unnecessary work
toLargest :: Tree Int -> Tree Int
toLargest t = fmap (\c -> foldl (\s v -> if v > c then s + v else s) 0 t) t

-- >>> tree
-- >>> toLargest tree
-- >>> toLargest tree2
-- >>> toLargest2 tree
-- >>> toLargest2 tree2
-- Node (Leaf 1) 2 (Node (Leaf 3) 6 (Leaf 7))
-- Node (Leaf 18) 16 (Node (Leaf 13) 7 (Leaf 0))
-- Node (Leaf 2) 0 Empty
-- Node (Leaf 18) 0 (Node (Leaf 0) 0 (Leaf 0))
-- Node (Leaf 2) 0 Empty

tree :: Tree Int
tree =
  Node
    ( Leaf 1
    )
    2
    ( Node
        ( Leaf 3
        )
        6
        (Leaf 7)
    )

tree2 :: Tree Int
tree2 =
  Node
    (Leaf 1)
    2
    Empty
