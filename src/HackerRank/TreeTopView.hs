module HackerRank.TreeTopView where

-- https://www.hackerrank.com/challenges/tree-top-view

data Tree a
  = Empty
  | Node (Tree a) a (Tree a)
  deriving (Show)

t1 :: Tree Int
t1 =
  Node
    (Node Empty 2 Empty)
    1
    (Node Empty 3 (Node Empty 4 Empty))

topTreeView :: Tree a -> [a]
topTreeView Empty = []
topTreeView (Node _ v tr) = v : topTreeView tr

printProb :: Tree Int -> [Char]
printProb = unwords . map show . topTreeView

-- >>> topTreeView t1
-- >>> printProb t1
-- [1,3,4]
-- "1 3 4"