{-# LANGUAGE InstanceSigs #-}

module GeeksForGeeksProblems.Level1_4 where

import GeeksForGeeksProblems.Types ( Tree(..) )

-- https://practice.geeksforgeeks.org/problems/symmetric-tree/1


treeHeight' :: Tree a -> Int
treeHeight' = go
  where
    go Empty = 0
    go (Leaf _) = 1
    go (Node l _ r) = 1 + max (go l) (go r)




treeSym :: Eq a =>  Tree a  -> Bool
treeSym Empty = True
treeSym (Leaf _) = True
treeSym (Node l _ r ) = r == treeMirror l

  where
    treeMirror ::  Tree a  -> Tree a
    treeMirror  = go
      where
        go Empty  = Empty
        go l@(Leaf _) = l
        go (Node l v1 r)  = Node (go r) v1 (go l)


-- >>> tree 
-- >>> treeSym tree 
-- >>> treeSym tree2 
-- Node (Node (Leaf 2) 1 Empty) 5 (Node Empty 1 (Leaf 2))
-- True
-- False



tree :: Tree Integer
tree =
  Node
    ( Node
      (Leaf 2)
      1
      Empty
    )
    5
    ( Node
        Empty
        1
        (Leaf 2)
    )

tree2 :: Tree Integer
tree2 =
  Node
    ( Node
      (Leaf 20)
      10
      (Leaf 20)
    )
    5
    ( Node
        Empty
        10
        (Leaf 30)
    )



