{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module InteviewQuestions.Q2 where

import Data.List (sortOn)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord qualified

-- Reduce the number of transactions moving value from one account to another 

qwishPartials :: (Ord a, Eq a) => [(a, a, Int)] -> Maybe [(a, a, Int)]
qwishPartials  = go . computeDeltas
  where
    go m = do
      if Map.null m
        then do return []
        else do
          mt@(mk, mv) <- findMaxMovement m
          (ma, mav) <- findMatchingOpTransaction mv mk m
          let dmap = Map.filter ((/=) 0) $ Map.delete ma $ Map.updateWithKey (\k v -> Just $ v + mav) mk m
          (\r -> (ma, mk, mav) : r) <$> go dmap

    findMaxMovement :: Map a Int -> Maybe (a, Int)
    findMaxMovement m = if Map.null m then Nothing else Just $ List.minimumBy (\(_, v1) (_, v2) -> compare v1 v2) $ Map.toList m

    findMatchingOpTransaction :: (Ord a, Eq a) => Int -> a -> Map a Int -> Maybe (a, Int)
    findMatchingOpTransaction v a m =
      List.find
        (\(k, v2) -> v <= (- v2))
        (sortOn (Data.Ord.Down . snd) (Map.toList $ Map.filterWithKey (\k _ -> a /= k) m))
    computeDeltas :: (Ord a, Eq a) => [(a, a, Int)] -> Map a Int
    computeDeltas xs = Map.filter ((/=) 0) $ foldl (\s (a, v) -> Map.insertWith (+) a v s) Map.empty $ flattenMovements xs
      where
        flattenMovements = concatMap (\(fromAccount, toAccount, value) -> [(fromAccount, - value), (toAccount, value)])

testTransactions :: [(Char, Char, Int)]
testTransactions = transactions2

-- >>> qwishPartials    testTransactions
-- Just [('D','A',100),('C','A',50)]

transactions1 :: [(Char, Char, Int)]
transactions1 =
  [ ('A', 'B', 100),
    ('B', 'C', 100)
  ]

transactions2 :: [(Char, Char, Int)]
transactions2 =
  [ ('A', 'B', 150),
    ('B', 'C', 50),
    ('B', 'D', 100 :: Int)
  ]
