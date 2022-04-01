{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module WikiQuestions.Problem1 where

dropEvery :: [a] -> Int -> [a]
dropEvery s c = go (c -1) s
  where
    go :: Int -> [a] -> [a]
    go _ [] = []
    go 0 (_ : xs) = go (c -1) xs
    go n (x : xs) = x : go (n -1) xs

--                123123123123
-- >>> dropEvery "abcdefghikj" 3
-- "abdeghkj"
