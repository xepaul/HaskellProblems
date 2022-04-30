module LeetCode.Prob9PalindromeNumber where

-- https://leetcode.com/problems/palindrome-number/

isPalindrome :: (Show a, Ord a, Num a) => a -> Bool
isPalindrome n
  | n < 0 = False
  | show n == reverse (show n) = True
  | otherwise = False

-- >>> isPalindrome (121)
-- True