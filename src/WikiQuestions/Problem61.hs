{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WikiQuestions.Problem61 where

import Data.Eq (Eq)
import Data.Int (Int)
import GHC.Num (Num ((+)))
import GHC.Show (Show)
import WikiQuestions.WikiTypes (Tree (Branch, Empty))

-- |  Wiki Problem 61.
-- Count the leaves of a binary tree
-- A leaf is a node with no successors. Write a predicate count_leaves/2 to count them.
countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r