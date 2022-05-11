module LeetCode.Prob164MaximumGap where

maxGap :: [Int] -> Int
maxGap [] =0
maxGap [_] =0
maxGap xs =maximum  $ zipWith (-) (tail xs)   xs

-- >>> maxGap [3,6,9,1]
-- >>> maxGap []
-- 3
-- 0