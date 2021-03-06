{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ImportQualifiedPost #-}
module GeeksForGeeksProblems.Types where
import Data.List qualified as List
data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show,Eq)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Empty = Empty
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r

countElements :: Tree a -> Int
countElements = foldr (\_ b -> b + 1) (0 :: Int)

valuesAllUnique :: (Foldable t, Eq a) => t a -> Bool
valuesAllUnique  t = let values = foldr (:) [] t
                         noDups = List.nub values
                     in length values == length noDups

treeIsSorted :: (Ord a, Foldable t) => t a -> Bool
treeIsSorted t = isSorted $ foldr (:) [] t

-- |  The 'isSorted' predicate returns 'True' if the elements of a list occur
-- in non-descending order,  equivalent to @'isSortedBy' ('<=')@.
isSorted :: Ord a => [a] -> Bool
isSorted = isSortedBy (<=)

-- |  The 'isSortedBy' function returns 'True' if the predicate returns true
-- for all adjacent pairs of elements in the list.
isSortedBy :: (a -> a -> Bool) -> [a] -> Bool
isSortedBy lte = loop
  where
    loop []       = True
    loop [_]      = True
    loop (x:y:zs) = (x `lte` y) && loop (y:zs)

-- >>> countElements treeEx1
-- 7

-- >>> foldl (flip (:)) [] treePreorder
-- >>> foldl (flip (:)) [] treeEx1
-- [2,4,4,1]
-- [7,6,5,4,3,2,1]

-- >>> foldr (:) [] treePreorder
-- >>> foldr (:) [] treeEx1
-- [1,4,4,2]
-- [1,2,3,4,5,6,7]

-- >>> foldl (+) 0 treeEx1
-- 28


treeEx1 :: Tree Integer
treeEx1 =
  Node
    ( Node
        (Leaf 1)
        2
        (Leaf 3)
    )
    4
    ( Node
        (Leaf 5)
        6
        (Leaf 7)
    )

treeEx1NotBST :: Tree Integer
treeEx1NotBST =
  Node
    ( Node
        (Leaf 1)
        2
        (Leaf 3)
    )
    4
    ( Node
        (Leaf 5)
        8
        (Leaf 6)
    )


treePreorder :: Tree Integer
treePreorder =
  Node
    Empty
    1
    ( Node
        (Leaf 4)
        4
        (Leaf 2)
    )
