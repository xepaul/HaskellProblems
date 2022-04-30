{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module WikiQuestions.Problems where

import Data.List qualified as List

pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' (y : ys) = go y [y] ys
  where
    go :: (Eq a) => a -> [a] -> [a] -> [[a]]
    go _ w [] = [w]
    go a w (x : xs) =
      if x == a
        then go a (x : w) xs
        else w : go x [x] xs

encode' :: Eq a => [a] -> [(Int, a)]
encode' = map (\a -> (length a, head a)) . pack'

data Prob11Encoding a
  = Single a
  | Multiple Int a
  deriving (Show, Eq)

-- | Wiki Problem 11. Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list.
encodeModified :: Eq a => [a] -> [Prob11Encoding a]
encodeModified =
  map
    ( \(n, v) ->
        if n > 1
          then Multiple n v
          else Single v
    )
    . encode'

-- | Wiki Problem 12. Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
decodeModified :: [Prob11Encoding a] -> [a]
decodeModified = go
  where
    go [] = []
    go ((Single v) : xs) = v : go xs
    go ((Multiple c v) : xs) = replicate c v ++ go xs

-- >>> decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',  Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

prob13 :: Eq a => [a] -> [Prob11Encoding a]
prob13 [] = []
prob13 (y : ys) = go 1 y ys
  where
    go :: Eq a => Int -> a -> [a] -> [Prob11Encoding a]
    go c i (x : xs)
      | i == x = go (c + 1) i xs
      | c == 1 = Single i : go 1 x xs
      | otherwise = Multiple c i : go 1 x xs
    go c i []
      | c == 1 = [Single i]
      | c > 1 = [Multiple c i]
      | otherwise = []

encodeDirect' :: Eq a => [a] -> [Prob11Encoding a]
encodeDirect' [] = []
encodeDirect' (x : xs) = go x 1 xs
  where
    go :: (Eq a) => a -> Int -> [a] -> [Prob11Encoding a]
    go a w [] = [encode a w]
    go a w (x : xs)
      | x == a = go a (w + 1) xs
      | otherwise = encode a w : go x 1 xs
    encode :: a -> Int -> Prob11Encoding a
    encode v c
      | c == 1 = Single v
      | otherwise = Multiple c v

-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']

-- >>> prob13 "aaaabccaadeeee"

-- >>> encodeDirect' "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']

-- | Wiki Problem 16. Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery ys c = go (c -1) ys
  where
    go :: Int -> [a] -> [a]
    go _ [] = []
    go 0 (_ : xs) = go (c -1) xs
    go n (x : xs) = x : go (n -1) xs

--                123123123123
-- >>> dropEvery "abcdefghikj" 3
-- "abdeghkj"

-- | Wiki Problem 20. Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt i s =
  let b = go 1 s
      c = s List.!! (i -1)
   in (c, b)
  where
    go :: Int -> [a] -> [a]
    go _ [] = []
    go j (x : xs) =
      if j == i
        then go (j + 1) xs
        else x : go (j + 1) xs

-- >>> removeAt 2 "abcd"
-- ('b',"acd")

