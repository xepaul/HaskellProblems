module GeeksForGeeksProblems.BoundaryTraversalOfBinaryTree where

-- https://practice.geeksforgeeks.org/problems/boundary-traversal-of-binary-tree/1

data Tree a
  = Empty
  | Node (Tree a) a (Tree a)
  deriving (Show)

boundary :: Tree a -> [a]
boundary t = leftView t <> leafs  <> rights
  where
    leftView :: Tree a -> [a]
    leftView Empty = []
    leftView (Node tl v _) = v : leftView tl

    rightView :: Tree a -> [a]
    rightView Empty = []
    rightView (Node _ v tl) = v : rightView tl

    leafView :: Tree a -> [a]
    leafView Empty = []
    leafView (Node Empty v Empty) = [v]
    leafView (Node l _ r) = leafView l <> leafView r

    leafs = let xs = leafView t
            in if null xs then [] else tail xs
    rights = let xs = rightView t
             in if 2 <= length xs then  tail (reverse (tail  xs)) else []

-- >>> boundary t1
-- >>> boundary t3
-- [1,2,4,8,9,6,7,3]
-- [1,2,4,6,5,7,8]

t1 :: Tree Int
t1 =
  Node
    ( Node
        (Node Empty 4 Empty)
        2
        (Node (Node Empty 8 Empty) 5 (Node Empty 9 Empty))
    )
    1
    ( Node
        (Node Empty 6 Empty)
        3
        (Node Empty 7 Empty)
    )

t2 :: Tree Int
t2 =
  Node
    (Node Empty 3 Empty)
    1
    (Node Empty 3 Empty)

t3 =
  Node
    ( Node
        (Node (Node Empty 6 Empty) 4 (Node Empty 5 Empty))
        2
        ( Node
            Empty
            3
            ( Node
                (Node Empty (7 :: Int) Empty)
                3
                (Node Empty 8 Empty)
            )
        )
    )
    1
    Empty