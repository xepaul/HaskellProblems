module GeeksForGeeksProblems.TopViewOfBinaryTree where

-- https://practice.geeksforgeeks.org/problems/top-view-of-binary-tree/1

data Tree a
  = Empty
  | Node (Tree a) a (Tree a)
  deriving (Show)

topTreeView :: Tree a -> [a]
topTreeView Empty = []
topTreeView (Node _ v tr) = v : topTreeView tr

topTreeView' :: Tree a -> [a]
topTreeView' Empty = []
topTreeView' (Node tr v _) = v : topTreeView' tr

topTreeView2 :: Tree a -> [a]
topTreeView2 x =
  let l = topTreeView' x
      r = tail $ topTreeView x
   in reverse l ++ r

printProb :: Tree Int -> [Char]
printProb = unwords . map show . topTreeView2

-- >>> topTreeView2 t1
-- >>> printProb t1
-- [2,1,3,4]
-- "2 1 3 4"

t1 :: Tree Int
t1 =
  Node
    (Node Empty 2 Empty)
    1
    (Node Empty 3 (Node Empty 4 Empty))