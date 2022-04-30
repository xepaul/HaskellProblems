{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module WikiQuestions.Problem1 where
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
