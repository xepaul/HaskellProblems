{-# LANGUAGE InstanceSigs #-}

module GeeksForGeeksProblems.Level1_1 where

-- https://practice.geeksforgeeks.org/problems/height-of-binary-tree/1
import GeeksForGeeksProblems.Types ( Tree(..) )


treeHeight :: Tree a -> Int
treeHeight = go
  where
    go Empty = 0
    go (Leaf _) = 1
    go (Node l _ r) = 1 + max (go l) (go r)

-- >>> treeHeight tree
-- 3


-- >>> tree == tree
-- True

tree :: Tree Integer
tree =
  Node
    ( Node
        (Leaf 1)
        2
        (Leaf 3)
    )
    4
    ( Node
        (Leaf 5)
        6
        (Leaf 7)
    )


