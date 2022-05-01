{-# LANGUAGE InstanceSigs #-}

module GeeksForGeeksProblems.Level1_3 where

-- https://practice.geeksforgeeks.org/problems/mirror-tree/1

import GeeksForGeeksProblems.Types ( Tree(..) )


treeMirror ::  Tree a  -> Tree a
treeMirror  = go
  where
    go Empty  = Empty
    go l@(Leaf _) = l
    go (Node l v1 r)  = Node (go r) v1 (go l)
 

-- >>> tree 
-- >>> treeMirror tree 
-- >>> treeMirror tree 
-- >>> (treeMirror $ treeMirror tree) == tree 
-- >>> (treeMirror $ treeMirror tree) == tree 
-- Node (Leaf 3) 1 (Node (Leaf 5) 2 (Leaf 4))
-- Node (Node (Leaf 4) 2 (Leaf 5)) 1 (Leaf 3)
-- Node (Node (Leaf 4) 2 (Leaf 5)) 1 (Leaf 3)
-- True

tree :: Tree Integer
tree =
  Node
    ( Leaf 3)
    1
    ( Node
        (Leaf 5)
        2
        (Leaf 4)
    )

countElements :: Tree a -> Int
countElements = foldr (\_ b -> b + 1) (0 :: Int)
