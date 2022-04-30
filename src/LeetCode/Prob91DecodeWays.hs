{-# LANGUAGE ImportQualifiedPost #-}

module LeetCode.Prob91DecodeWays where

-- https://leetcode.com/problems/decode-ways/

import Data.Map qualified as Map
import Debug.Trace
import Text.Read(readMaybe)

data Tree a
  = Empty
  | Branch a (Tree a) (Tree a)
  | Dead
  deriving (Show, Eq)

numDecodings :: String -> Int
numDecodings = countLeaf . decode
  where
    makeMapperDecode1 = Map.fromList $ zip (map show [1 :: Int .. 9]) ['A' ..]
    makeMapperDecode2 = Map.fromList $ zip (map show [10 :: Int .. 26]) ['J' ..]
    decode :: [Char] -> Tree String
    decode [] = Empty
    decode (d1 : d2 : ds) =
      let s2 = Map.lookup [d1, d2] makeMapperDecode2
          s1 = Map.lookup [d1] makeMapperDecode1
       in case (s1, s2) of
            (Just v1, Just v2) -> Branch ([v1] ++ "," ++ [v2]) (Branch [v1] (decode (d2 : ds)) Empty) (Branch [v2] Empty (decode ds))
            (Just v1, Nothing) -> Branch [v1] (decode (d2 : ds)) Empty
            (Nothing, Just v2) -> Branch [v2] Empty (decode ds)
            (Nothing, Nothing) -> Dead
    decode (x : xs) = case Map.lookup [x] makeMapperDecode1 of
      Nothing -> Empty
      Just v -> Branch [v] (decode xs) Empty
    countLeaf :: Tree String -> Int
    countLeaf (Branch _ Empty Empty) = 1
    countLeaf (Branch _ Empty r) = countLeaf r
    countLeaf (Branch _ l Empty) = countLeaf l
    countLeaf (Branch _ l r) = countLeaf l + countLeaf r
    countLeaf Empty = 0
    countLeaf Dead = 0

numDecodings' :: String -> Int
numDecodings' = decode
  where
    convertToInt :: String -> Maybe [Int]
    convertToInt = traverse (\a -> readMaybe [a])

    singleDigitMap = Map.fromList $ zip (map (head . show) [1 :: Int .. 9]) ['A' ..]
    doubleDigitMap = Map.fromList $ zip (map show [10 :: Int .. 26]) ['J' ..]
    decode :: [Char] -> Int
    decode [] = 1
    decode (d1 : d2 : ds) =
      let s2 = Map.lookup [d1, d2] doubleDigitMap
          s1 = Map.lookup d1 singleDigitMap
       in case (s1, s2) of
            (Just _, Just _) -> decode (d2 : ds) + decode ds
            (Just _, Nothing) -> decode (d2 : ds)
            (Nothing, Just _) -> decode ds
            (Nothing, Nothing) -> 0
    decode (y : ys) = case Map.lookup y singleDigitMap of
      Nothing -> 0
      Just _ -> decode ys

-- >>> numDecodings' "12"
-- 2

-- >>> numDecodings' "226"
-- 3

-- >>> numDecodings' "06"
-- 0
