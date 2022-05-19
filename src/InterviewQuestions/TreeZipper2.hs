{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module InterviewQuestions.TreeZipper2 where

-- Walk tree with bool list, if false go left if true go right. 
-- Either add a node value if the end of the walk is a Tree Empty 
--  or replace an existing node value. using a zipper

import Control.Monad (foldM)

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)

data TreeZip a
  = LeftTreeZip a (Tree a)
  | RightTreeZip a (Tree a)
  deriving (Show)

type TreeZips a = [TreeZip a]

type TreeZipper a = (Tree a, TreeZips a)

-- | go right in the tree saving movement in the given zipper
goLeft :: TreeZipper a -> Either String (TreeZipper a)
goLeft (Node l x r, bs) = Right (l, LeftTreeZip x r : bs)
goLeft _ = Left "can't go left"

-- | go right in the tree saving movement in the given zipper
goRight :: TreeZipper a -> Either String (TreeZipper a)
goRight (Node l x r, bs) = Right (r, RightTreeZip x l : bs)
goRight _ = Left "can't go right"

-- | go up a level in the tree as defined by the given zipper
goUp :: TreeZipper a -> TreeZipper a
goUp (t, LeftTreeZip x r : bs) = (Node t x r, bs)
goUp (t, RightTreeZip x l : bs) = (Node l x t, bs)
goUp (t, []) = (t, [])

-- | get to the top/root of the Tree
topMost :: TreeZipper a -> TreeZipper a
topMost (t, []) = (t, [])
topMost (z, c) = topMost $ goUp (z, c)

-- | modify a tree value at the current zipper Tree focus
modify :: (a -> a) -> TreeZipper a -> TreeZipper a
modify f (Node l x r, bs) = (Node l (f x) r, bs)
modify _ (Empty, bs) = (Empty, bs)

-- | replace a node value or add sub tree with the given value at the current zipper Tree focus
replaceNodeValueOrAddNode :: a -> TreeZipper a -> TreeZipper a
replaceNodeValueOrAddNode value (Node l _ r, bs) = (Node l value r, bs)
replaceNodeValueOrAddNode value (Empty, bs) = (Node Empty value Empty, bs)

-- | replace a node the current focus
attach :: Tree a -> TreeZipper a -> TreeZipper a
attach t (_, bs) = (t, bs)

data Direction = L | R deriving (Show, Eq)

type Directions = [Direction]

-- | make a tree zipper given a Tree
rootZipper :: Tree a -> TreeZipper a
rootZipper = (,[])

-- | replace or add a value by following the directions False go left, True go Right
followBooleansThenReplaceOrAdd :: a -> Tree a -> [Bool] -> Either String (Tree a)
followBooleansThenReplaceOrAdd v t =
  followThenReplaceOrAdd v t
    . fmap \case
      True -> R
      False -> L

-- | replace or add a value by following the directions False go left, True go Right
followThenReplaceOrAdd :: a -> Tree a -> [Direction] -> Either String (Tree a)
followThenReplaceOrAdd v t d =
  fst . topMost . replaceNodeValueOrAddNode v
    <$> foldM
      ( \s -> \case
          L -> goLeft s
          R -> goRight s
      )
      (rootZipper t)
      d

t1 :: Tree Char
t1 =
  Node
    ( Node
        (Node Empty 'a' Empty)
        'v'
        (Node Empty 'b' Empty)
    )
    'r'
    ( Node
        Empty
        'c'
        Empty
    )

-- >>> t1
-- >>> followBooleansThenReplaceOrAdd 'z' t1 [True,True]
-- >>> followBooleansThenReplaceOrAdd 'z' t1 [True,False]
-- >>> followThenReplaceOrAdd 'z' t1 [R,R]
-- >>> followThenReplaceOrAdd 'z' t1 [R,L]
-- >>> followThenReplaceOrAdd 'z' t1 [L,L,L]
-- Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node Empty 'c' Empty)
-- Right (Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node Empty 'c' (Node Empty 'z' Empty)))
-- Right (Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node (Node Empty 'z' Empty) 'c' Empty))
-- Right (Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node Empty 'c' (Node Empty 'z' Empty)))
-- Right (Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node (Node Empty 'z' Empty) 'c' Empty))
-- Right (Node (Node (Node (Node Empty 'z' Empty) 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node Empty 'c' Empty))
