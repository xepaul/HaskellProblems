{-# LANGUAGE InstanceSigs #-}

module GeeksForGeeksProblems.Level1_2 where

-- https://practice.geeksforgeeks.org/problems/determine-if-two-trees-are-identical/1
-- done automatically via Eq derive
-- hand coded below

import GeeksForGeeksProblems.Types ( Tree(..) )

treeEq :: Eq a => Tree a -> Tree a -> Bool
treeEq  = go
  where
    go Empty Empty = True
    go (Leaf a) (Leaf b)= a == b
    go (Node l v1 r)  (Node l2 v2 r2) = v1 == v2 && (go l r == go l2 r2)
    go _ _ = False

-- >>> tree == tree
-- >>> treeEq tree tree
-- >>> tree == tree2
-- >>> treeEq tree  tree2
-- True
-- True
-- False
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

tree2 :: Tree Integer
tree2 =
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
        (Leaf 87)
    )

countElements :: Tree a -> Int
countElements = foldr (\_ b -> b + 1) (0 :: Int)

-- >>> countElements tree
-- 7

-- >>> foldl (flip (:)) [] tree
-- [7,6,5,4,3,2,1]

-- >>> foldr (:) [] tree
-- [1,2,3,4,5,6,7]
