{-# LANGUAGE InstanceSigs #-}

module GeeksForGeeksProblems.Level1_8 where

-- https://practice.geeksforgeeks.org/problems/check-for-bst/1

import GeeksForGeeksProblems.Types ( Tree(..) )

isBst :: Tree Int -> Bool
isBst = go
  where
    go Empty = True
    go (Leaf _) = True
    go (Node l v r) =
      let lv = getValue l
          rv = getValue r
       in lv < v && rv > v && go l && go r
    getValue :: Tree Int -> Int
    getValue Empty = 0
    getValue (Leaf v) = v
    getValue (Node _ v _) = v

-- >>> tree
-- >>> isBst tree
-- >>> isBst tree2
-- Node (Leaf 1) 2 (Leaf 3)
-- True
-- False


tree :: Tree Int
tree =
  Node
    ( Leaf 1
    )
    2
    (Leaf 3)

tree2 :: Tree Int
tree2 =
  Node
    Empty
    2
    ( Node
        Empty
        7
        ( Node
            Empty
            6
            (Leaf 5)
        )
    )