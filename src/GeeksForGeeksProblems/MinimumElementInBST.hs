module GeeksForGeeksProblems.MinimumElementInBST where

-- https://practice.geeksforgeeks.org/problems/minimum-element-in-bst/1/

data Tree a
  = Empty
  | Node (Tree a) a (Tree a)
  deriving (Show)

minValue :: (Ord a, Bounded a) => Tree a -> a
minValue Empty = maxBound
minValue (Node l v _) = min v $ minValue l

-- >>> minValue t1
-- >>> minValue t2
-- 1
-- 9

t1 :: Tree Int
t1 =
  Node
    (Node (Node (Node Empty 1 Empty) 3 Empty) 4 Empty)
    5
    (Node Empty 6 (Node Empty 7 Empty))

t2 :: Tree Int
t2 =
  Node
    Empty
    9
    (Node Empty 10 (Node Empty 11 Empty))
