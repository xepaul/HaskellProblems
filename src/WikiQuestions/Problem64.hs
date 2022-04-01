{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module WikiQuestions.Problem64 where

import Data.List qualified as List
import WikiQuestions.WikiTypes (Tree (Branch, Empty))

layout :: Tree a -> [(a, (Int, Int))]
layout t = List.zipWith (\x (v, y) -> (v, (x, y))) [1 ..] (go 1 t)
  where
    go :: Int -> Tree a -> [(a, Int)]
    go _ Empty = []
    go i (Branch v l r) = go (i + 1) l ++ [(v, i)] ++ go (i + 1) r
