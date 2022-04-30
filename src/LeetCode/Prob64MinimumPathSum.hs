{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}

module LeetCode.Prob63UniquePaths2 where

-- https://leetcode.com/problems/minimum-path-sum/

import Data.Set qualified as Set
uniquePaths :: [[Int]] -> Int
uniquePaths valuesGrid = minimum $ go 0 (0, 0)
  where
    n = length $ head valuesGrid
    m = length valuesGrid
    finishLocation = (n -1, m -1)
    gridPoints = Set.fromList [(x, y) | x <- [0 .. n -1], y <- [0 .. m -1]]
    moveDown (x, y) = (x, y + 1)
    moveRight (x, y) = (x + 1, y)
    getValue (x, y) = (valuesGrid !! y) !! x
    go :: Int -> (Int, Int) -> [Int]
    go v loc
      | not (Set.member loc gridPoints) = []
      | loc == finishLocation = [v + getValue finishLocation]
      | otherwise =
        let newSum = v + getValue loc
         in go newSum (moveDown loc) ++ go newSum (moveRight loc)

-- >>> uniquePaths [[1,3,1],[1,5,1],[4,2,1]]
-- 7

-- >>> uniquePaths [[1,2,3],[4,5,6]]
-- 12
