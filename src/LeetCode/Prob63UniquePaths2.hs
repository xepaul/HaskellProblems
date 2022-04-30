{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module LeetCode.Prob63UniquePaths2 where

-- https://leetcode.com/problems/unique-paths-ii/

import Data.Set qualified as Set

uniquePaths :: [[Int]] -> Int
uniquePaths obstaclesGrid = go (0, 0)
  where
    n = length $ head obstaclesGrid
    m = length obstaclesGrid
    finishLocation = (n -1, m -1)
    obstaclesMap = Set.fromList [(x, y) | x <- [0 .. m -1], y <- [0 .. n -1], 1 == (obstaclesGrid !! y) !! x]
    gridPoints = Set.fromList [(x, y) | x <- [0 .. m -1], y <- [0 .. n -1]]
    moveDown (x, y) = (x, y + 1)
    moveRight (x, y) = (x + 1, y)
    go :: (Int, Int) -> Int
    go loc
      | not (Set.member loc gridPoints) || Set.member loc obstaclesMap = 0
      | loc == finishLocation = 1
      | otherwise = go (moveDown loc) + go (moveRight loc)

-- >>> uniquePaths [[0,0,0],[0,1,0],[0,0,0]]
-- 2

-- >>> uniquePaths [[0,1],[0,0]]
-- 1
