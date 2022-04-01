{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module WikiQuestions.Problem61A where

import WikiQuestions.WikiTypes (Tree (Branch, Empty))
-- |  Wiki Problem 61A.
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch v Empty Empty) = [v]
leaves (Branch _ l r) = leaves l ++ leaves r