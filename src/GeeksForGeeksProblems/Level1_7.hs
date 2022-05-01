{-# LANGUAGE InstanceSigs #-}

module GeeksForGeeksProblems.Level1_7 where


-- https://practice.geeksforgeeks.org/problems/children-sum-parent/1

import GeeksForGeeksProblems.Types ( Tree(..) )

allParentSum :: Tree Int -> Bool
allParentSum = go
  where
    go Empty = True
    go (Leaf _) = True
    go (Node l v r) =
      let lv = getValue l
          rv = getValue r
          childNodeSum = lv + rv
       in childNodeSum == v && go l && go r
    getValue :: Tree Int -> Int
    getValue Empty = 0
    getValue (Leaf v) = v
    getValue (Node _ v _) = v

-- >>> tree
-- >>> allParentSum tree
-- >>> allParentSum tree2
-- Node (Leaf 10) 10 Empty
-- True
-- False


tree :: Tree Int
tree =
  Node
    ( Leaf 10
    )
    10
    Empty

tree2 :: Tree Int
tree2 =
  Node
    ( Node
        (Leaf 5)
        4
        (Leaf 6)
    )
    7
    (Leaf 3)