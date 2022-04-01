{-# LANGUAGE NoImplicitPrelude #-}

module WikiQuestions.Problem9 (pack) where

import Data.Eq (Eq ((==)))

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x : xs) = go x [x] xs
  where
    go :: (Eq a) => a -> [a] -> [a] -> [[a]]
    go _ w [] = [w]
    go a w (y : ys) =
      if y == a
        then go a (y : w) ys
        else w : go y [y] ys

-- >>> pack' ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',  'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
