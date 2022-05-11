{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module InterviewQuestions.TreeZipper where

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show)

data Crumb a
  = LeftCrumb a (Tree a)
  | RightCrumb a (Tree a)
  deriving (Show)

type Breadcrumbs a = [Crumb a]

type TreeZipper a = (Tree a, Breadcrumbs a)

-- | go right in the tree saving movement in the given zipper
goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goLeft (Node l x r, bs) = (l, LeftCrumb x r : bs)
goLeft _ = error "can't go left"

-- | go right in the tree saving movement in the given zipper
goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goRight (Node l x r, bs) = (r, RightCrumb x l : bs)
goRight _ = error "can't go right"

-- | go up a level in the tree as defined by the given zipper
goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
goUp (t, LeftCrumb x r : bs) = (Node t x r, bs)
goUp (t, RightCrumb x l : bs) = (Node l x t, bs)
goUp (t, []) = (t,[])

-- | get to the top/root of the Tree
topMost :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
topMost (t, []) = (t, [])
topMost (z, c) = topMost $goUp (z, c)

-- | modify a tree value at the current zipper Tree focus
modify :: (a -> a) -> (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
modify f (Node l x r, bs) = (Node l (f x) r, bs)
modify _ (Empty, bs) = (Empty, bs)

-- | replace a node value or add sub tree with the given value at the current zipper Tree focus
replaceOrAddValue :: a -> (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
replaceOrAddValue f (Node l _ r, bs) = (Node l f r, bs)
replaceOrAddValue f (Empty, bs) = (Node Empty f Empty, bs)

-- | replace a node the current focus
attach :: Tree a -> (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
attach t (_, bs) = (t, bs)

data Direction = L | R deriving (Show, Eq)

type Directions = [Direction]

-- | make a tree zipper given a Tree
rootZipper :: Tree a -> (Tree a, Breadcrumbs a)
rootZipper = (,[])

-- | replace or add a value by following the directions False go left, True go Right
followBooleansThenReplaceOrAdd :: a -> Tree a -> [Bool] -> Tree a
followBooleansThenReplaceOrAdd v t =
  followThenReplaceOrAdd v t
    . fmap \case
      True -> R
      False -> L

-- | replace or add a value by following the directions False go left, True go Right
followThenReplaceOrAdd :: a -> Tree a -> [Direction] -> Tree a
followThenReplaceOrAdd v t =
  fst
    . topMost
    . replaceOrAddValue v
    . foldl
      ( \s -> \case
          L -> goLeft s
          R -> goRight s
      )
      (rootZipper t)

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
-- >>> fst $ topMost $ goRight $ goRight (t1,[])
-- >>> fst $ topMost $ replaceOrAddValue 'z' $ goRight $ goRight (t1,[])
-- >>> followBooleansThenReplaceOrAdd 'z' t1 [True,True]
-- >>> followBooleansThenReplaceOrAdd 'z' t1 [True,False]
-- >>> followThenReplaceOrAdd 'z' t1 [R,R]
-- >>> followThenReplaceOrAdd 'z' t1 [R,L]
-- Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node Empty 'c' Empty)
-- Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node Empty 'c' Empty)
-- Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node Empty 'c' (Node Empty 'z' Empty))
-- Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node Empty 'c' (Node Empty 'z' Empty))
-- Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node (Node Empty 'z' Empty) 'c' Empty)
-- Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node Empty 'c' (Node Empty 'z' Empty))
-- Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node (Node Empty 'z' Empty) 'c' Empty)


-- >>> fst $ topMost $ replaceOrAddValue 'z' $  goRight (t1,[])
-- Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node Empty 'c' Empty)
-- Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node Empty 'c' Empty)
-- Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node Empty 'c' (Node Empty 'z' Empty))
-- Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node Empty 'c' (Node Empty 'z' Empty))
-- Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node (Node Empty 'z' Empty) 'c' Empty)
-- Node (Node (Node Empty 'a' Empty) 'v' (Node Empty 'b' Empty)) 'r' (Node Empty 'z' Empty)

