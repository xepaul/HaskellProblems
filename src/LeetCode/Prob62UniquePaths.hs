{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module LeetCode.Prob62UniquePaths where

-- https://leetcode.com/problems/unique-paths/

import Data.Set qualified as Set

uniquePaths :: Int -> Int -> Int
uniquePaths m n = go (0, 0)
  where
    finishLocation = (m -1, n -1)
    gridPoints = Set.fromList [(x, y) | x <- [0 .. m -1], y <- [0 .. n -1]]
    moveDown (x,y) =  (x, y + 1)
    moveRight (x,y) =  (x +1, y)
    go :: (Int, Int) -> Int
    go loc
      | not (Set.member loc gridPoints) = 0
      | loc == finishLocation = 1
      | otherwise = go (moveDown loc) + go (moveRight loc)

-- >>> uniquePaths 3 7
-- 28

-- >>> uniquePaths 3 2
-- 3

