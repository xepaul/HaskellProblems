{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module LeetCode.Prob3LengthOfLongestSubstring where

import Data.Set (Set)
import Data.Set qualified as Set

-- https://leetcode.com/problems/longest-substring-without-repeating-characters/

lengthOfLongestSubstring :: String -> Int
lengthOfLongestSubstring [] = 0
lengthOfLongestSubstring (x : xs) = go (Set.singleton x) (Set.singleton x) xs
  where
    go :: Set Char -> Set Char -> [Char] -> Int
    go m _ [] = length m
    go m r (x : xs) =
      if Set.member x r
        then
          let r' =
                if length r > length m
                  then r
                  else m
           in go r' (Set.singleton x) xs
        else go r (Set.insert x r) xs

-- >>> lengthOfLongestSubstring "abcabcbb"
-- 3
