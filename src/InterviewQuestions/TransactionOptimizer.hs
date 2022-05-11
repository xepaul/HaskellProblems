{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns  #-}

module InterviewQuestions.TransactionOptimizer where

import Data.List (sortOn, sortBy)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord qualified

-- Reduce the number of transactions which should be completed one batch, 
--  each transaction moves value from one account to another. 

data (Ord a, Eq a) => Transaction a =
  Transaction { fromAccount :: a, toAccount:: a, value :: Int   }
  deriving Show

reduceNumberofTransactions :: (Ord a, Eq a) => [Transaction a ] -> [Transaction a]
reduceNumberofTransactions  = orderTransactions .  go . computeDeltas
  where
    -- | order calculated transactions
    orderTransactions :: Ord a => [Transaction a ] -> [Transaction a ]
    orderTransactions = sortBy  (\(Transaction f1 t1 _ ) (Transaction f2 t2 _) -> compare (f1,t1) (f2,t2))
    -- | calculate individual account value movement
    computeDeltas :: (Ord a, Eq a) => [Transaction a] -> Map a Int
    computeDeltas =
      Map.filter (0 /=)
          . foldl (\s (a, v) -> Map.insertWith (+) a v s) Map.empty
          . concatMap (\ Transaction {fromAccount, toAccount,value} ->
                          [(fromAccount, - value), (toAccount, value)])
    go accountMovementMap =
      if Map.null accountMovementMap
        then []
        else
          let (maxMovementAccount, maxValue) = findMaxMovement accountMovementMap
              (maxMatchedAccount, maxMatchedValue) =
                findBestMatchingOpTransaction maxValue
                $ Map.filterWithKey (\k _ -> maxMovementAccount /= k)  accountMovementMap
          in Transaction maxMovementAccount maxMatchedAccount maxMatchedValue :
             go (Map.filter (0 /=)
                    $ Map.delete maxMatchedAccount
                    $ Map.updateWithKey (\_ v -> Just $ v + maxMatchedValue) maxMovementAccount accountMovementMap)
    findMaxMovement :: Map a Int ->  (a, Int)
    findMaxMovement accountMovementMap =
      List.minimumBy (\(_, v1) (_, v2) -> compare v1 v2) $ Map.toList accountMovementMap
    findBestMatchingOpTransaction :: (Ord a, Eq a) => Int -> Map a Int -> (a, Int)
    findBestMatchingOpTransaction sourceValue =
      fromMaybe (error "") -- this shouldn't be possible with the way the account movement map is made
        . List.find (\(_, v2) -> sourceValue <= (- v2))
        . sortOn (Data.Ord.Down . snd)
        . Map.toList

-- Examples
-- >>> reduceNumberofTransactions    transactions1
-- >>> reduceNumberofTransactions    transactions2
-- >>> reduceNumberofTransactions    []
-- >>> reduceNumberofTransactions    transactions0
-- >>> reduceNumberofTransactions    transactions01
-- [Transaction {fromAccount = 'A', toAccount = 'C', value = 100}]
-- [Transaction {fromAccount = 'A', toAccount = 'C', value = 50},Transaction {fromAccount = 'A', toAccount = 'D', value = 100}]
-- []
-- [Transaction {fromAccount = 'A', toAccount = 'B', value = 100}]
-- [Transaction {fromAccount = 'D', toAccount = 'B', value = 100},Transaction {fromAccount = 'D', toAccount = 'E', value = 50},Transaction {fromAccount = 'F', toAccount = 'C', value = 100}]


transactions1 :: [Transaction Char]
transactions1 =
  [ Transaction 'A' 'B' 100,
    Transaction 'B' 'C' 100
  ]

transactions2 :: [Transaction Char]
transactions2 =
  [ Transaction 'A' 'B' 150,
    Transaction 'B' 'C' 50,
    Transaction 'B' 'D' (100 :: Int)
  ]
transactions0 :: [Transaction Char]
transactions0 =
  [ Transaction 'A' 'B' 100
  ]
transactions01 :: [Transaction Char]
transactions01 =
  [
    Transaction 'A' 'B' 100,
    Transaction 'A' 'C' 100,
    Transaction 'D' 'E' 150,
    Transaction 'E' 'F' 100,
    Transaction 'F' 'A' 200
  ]
