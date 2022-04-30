module LeetCode.Prob1TwoSum where
import Data.Maybe (catMaybes, listToMaybe)

-- https://leetcode.com/problems/two-sum/

twoSum :: [Int] -> Int -> Maybe (Int,Int)
twoSum [] _ = Nothing
twoSum [_] _ = Nothing
twoSum xs v =
  listToMaybe
    $ catMaybes
    $ concatMap (\(x,i) ->
       zipWith (\ y j -> if i /= j && x + y == v then Just (i, j)
                                            else Nothing)
         xs [0..] )
    $  zip xs [0..]

twoSum' :: [Int] -> Int -> Maybe (Int,Int)
twoSum' nums target = listToMaybe $ catMaybes go
  where go = do
              (x,i) <-  zip  nums [0::Int ..]
              (y,j) <-  zip  nums [0::Int ..]
              if i /= j && x + y == target 
              then [Just (i, j)]
              else [Nothing]

twoSum'' :: [Int] -> Int -> Maybe (Int,Int)
twoSum'' nums target = listToMaybe go
  where go = [ (i,j) | (x,i) <- zip  nums [0::Int ..],
                       (y,j) <-  zip  nums [0::Int ..],
                       i /= j && x + y == target ]

-- >>> twoSum'' [2,7,11,15] 9
-- Just (0,1)





