module InterviewQuestions.TreeWalker2 where

-- Walk tree with bool list, if false go left if true go right. 
-- Either add a node value if the end of the walk is a Tree Empty 
--  or replace an existing node value

data Tree a
  = Empty
  | Node (Tree a) a (Tree a)
  deriving (Show)

addorModifyTreeUnsafe :: Tree a -> [Bool] -> a -> Tree a
addorModifyTreeUnsafe Empty (_ : _) _ = error "to short"
addorModifyTreeUnsafe Empty [] v = Node Empty v Empty
addorModifyTreeUnsafe (Node l _ r) [] v = Node l v r
addorModifyTreeUnsafe (Node l x r) (False : xs) v = Node (addorModifyTreeUnsafe l xs v) x r
addorModifyTreeUnsafe (Node l x r) (True : xs) v = Node l x (addorModifyTreeUnsafe r xs v)

addorModifyTree :: Tree a -> [Bool] -> a -> Either String (Tree a)
addorModifyTree Empty [] v = Right $ Node Empty v Empty
addorModifyTree Empty (_ : _) _ = Left "to short"
addorModifyTree (Node l _ r) [] v = Right $ Node l v r
addorModifyTree (Node l x r) (False : xs) v = (\nv -> Node nv x r) <$> addorModifyTree l xs v
addorModifyTree (Node l x r) (True : xs) v = Node l x <$> addorModifyTree r xs v

t1 :: Tree Char
t1 =
  Node
    (Node Empty 'b' Empty)
    'a'
    (Node Empty 'c' Empty)

-- >>> addorModifyTree t1 [True] 'e'
-- >>> addorModifyTree t1 [True,True,True] 'e'
-- >>> addorModifyTree t1 [True,False] 'e'
-- >>> addorModifyTree t1 [True,True,True] 'e'
-- >>> addorModifyTree t1 [] 'e'
-- Right (Node (Node Empty 'b' Empty) 'a' (Node Empty 'e' Empty))
-- Left "to short"
-- Right (Node (Node Empty 'b' Empty) 'a' (Node (Node Empty 'e' Empty) 'c' Empty))
-- Left "to short"
-- Right (Node (Node Empty 'b' Empty) 'e' (Node Empty 'c' Empty))

