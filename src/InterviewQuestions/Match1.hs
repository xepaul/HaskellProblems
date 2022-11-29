{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module InterviewQuestions.Match1 where

import Data.Either (partitionEithers)
import Data.Foldable (find, minimumBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)

data OrderType = Buy | Sell deriving (Show, Eq)

data Order = Order
  { orderId :: Int,
    orderType :: OrderType,
    orderPrice :: Int,
    quantity :: Int
  }
  deriving (Show)

data BuyOrder = BuyOrder
  { buyOrderId :: Int,
    buyOrderPrice :: Int,
    buyOrderquantity :: Int
  }
  deriving (Show)

data Trade = Trade
  { tradeBuyId :: Int,
    tradeSellId :: Int,
    tradePrice :: Int,
    tradeQuantityTraded :: Int
  }
  deriving (Show,Eq,Ord)

match :: [Order] -> ([Order], [Trade])
match orders =
  let buys = Map.fromList $ map (\o@Order {..} -> (orderId, o)) $ filter (\Order {orderType} -> orderType == Buy) orders
      sells = Map.fromList $ map (\o@Order {..} -> (orderId, o)) $filter (\Order {orderType} -> orderType == Sell) orders
      (a, b) = partitionEithers $ go buys sells
      c = foldl (<>) [] a
   in (c, b)
  where
    go :: Map Int Order -> Map Int Order -> [Either [Order] Trade]
    go buys sells = fromMaybe [Left $ Map.elems buys <> Map.elems sells] $ do
      Order {orderId = sellOrderId, orderPrice = sellPrice, quantity = sellQuantity} <-
        minimumByMaybe (\Order {orderPrice = price1} Order {orderPrice = price2} -> compare price1 price2) sells
      Order {orderId = buyOrderId, quantity = buyQuantity} <-
        find (\Order {orderPrice = price2} -> sellPrice <= price2) $ Map.elems buys
      let tradeQuantity = min buyQuantity sellQuantity
      let updateOrders = Map.update \o@Order {quantity} ->
            let nQuantity = quantity - tradeQuantity
             in if nQuantity > 0
                  then Just $ o {quantity = nQuantity}
                  else Nothing
      let sells' = updateOrders sellOrderId sells
      let buys' = updateOrders buyOrderId buys
      return $
        ( Right $
            Trade
              { tradeBuyId = buyOrderId,
                tradeSellId = sellOrderId,
                tradePrice = sellPrice,
                tradeQuantityTraded = tradeQuantity
              }
        ) :
        go buys' sells'


          

minimumByMaybe :: (Foldable t) => (a -> a -> Ordering) -> t a -> Maybe a
minimumByMaybe f = safeCall (minimumBy f)
  where
    safeCall fl xs
      | null xs = Nothing
      | otherwise = Just $ fl xs
