module GeeksForGeeksProblems.LeftViewOfBinaryTree where

-- https://practice.geeksforgeeks.org/problems/left-view-of-binary-tree/1

data Tree a
  = Empty
  | Node (Tree a) a (Tree a)
  deriving (Show)

leftView :: Tree a -> [a]
leftView Empty = []
leftView (Node Empty v tr) = v : leftView tr
leftView (Node tl v _) = v : leftView tl

printProb :: Tree Int -> [Char]
printProb = unwords . map show . leftView

-- >>> leftView t1
-- [1,2,4,8]

t1 :: Tree Int
t1 =
  Node
    (Node (Node Empty 4 (Node Empty 8 Empty)) 2 Empty)
    1
    (Node Empty 3 (Node Empty 4 Empty))

t2 :: Tree Int
t2 =
  Node
    (Node Empty 3  Empty)
    1
    (Node Empty 3 Empty)
