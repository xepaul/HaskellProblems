{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module WikiQuestions.Problem62B where

import WikiQuestions.WikiTypes (Tree (Branch, Empty))

atlevel :: Int -> Tree a -> [a]
atlevel level = go 1
  where
    go :: Int -> Tree a -> [a]
    go i Empty = []
    go i (Branch v l r) | i == level = [v]
    go i (Branch _ l r) = go (i + 1) l ++ go (i + 1) r

atlevel' :: Int -> Tree a -> [a]
atlevel' _ Empty = []
atlevel' i (Branch v l r) | i == 1 = [v]
atlevel' i (Branch _ l r) = atlevel' (i -1) l ++ atlevel' (i -1) r

atLevel''' :: Tree a -> Int -> [a]
atLevel''' Empty _ = []
atLevel''' (Branch v l r) n
  | n == 1 = [v]
  | n > 1 = atLevel''' l (n -1) ++ atLevel''' r (n -1)
  | otherwise = []