{-# LANGUAGE NamedFieldPuns #-}

module Exercism.Zipper
  ( BinTree (BT),
    fromTree,
    left,
    right,
    setLeft,
    setRight,
    setValue,
    toTree,
    up,
    value,
  )
where

data BinTree a = BT
  { btValue :: a,
    btLeft :: Maybe (BinTree a),
    btRight :: Maybe (BinTree a)
  }
  deriving (Eq, Show)

data TreeZip a
  = LeftZip a (Maybe (BinTree a))
  | RightZip a (Maybe (BinTree a))
  deriving (Eq, Show)

data Zipper a = Zipper (BinTree a) [TreeZip a] deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper t []) = t
toTree (Zipper t (LeftZip zv z : xs)) = toTree (Zipper (BT zv (Just t) z) xs)
toTree (Zipper t (RightZip zv z : xs)) = toTree (Zipper (BT zv z (Just t)) xs)

value :: Zipper a -> a
value (Zipper BT {btValue} _) = btValue

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v l r) xs) = flip Zipper (LeftZip v r : xs) <$> l

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v l r) xs) = flip Zipper (RightZip v l : xs) <$> r

up :: Zipper a -> Maybe (Zipper a)
up (Zipper l ((RightZip v r) : xs)) = Just $ Zipper (BT v r (Just l)) xs
up (Zipper l ((LeftZip v r) : xs)) = Just $ Zipper (BT v (Just l) r) xs
up _ = Nothing

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) zips) = Zipper (BT x l r) zips

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l (Zipper (BT v _ r) zips) = Zipper (BT v l r) zips

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r (Zipper (BT v l _) zips) = Zipper (BT v l r) zips