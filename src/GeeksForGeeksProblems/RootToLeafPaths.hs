module GeeksForGeeksProblems.RootToLeafPaths where

-- https://practice.geeksforgeeks.org/problems/root-to-leaf-paths/1

data Tree a
  = Empty
  | Node (Tree a) a (Tree a)
  deriving (Show)


boundary :: Tree a -> [[a]]
boundary =  map reverse . go [] 
  where
    go :: [a] -> Tree a -> [[a]]
    go _ Empty = []
    go s (Node Empty v Empty) = [v:s]
    go s (Node l v r) = let sNext = (v:s) in go sNext l <> go sNext r

-- >>> boundary t2
-- >>> boundary t4
-- [[1,2],[1,3]]
-- [[10,20,40],[10,20,60],[10,30]]

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
    (Node Empty 2 Empty)
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

t4 :: Tree Int
t4 =
  Node
    (Node (Node Empty 40 Empty) 20 (Node Empty 60 Empty))
    10
    (Node Empty 30 Empty)
