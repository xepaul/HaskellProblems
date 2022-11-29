{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module InterviewQuestions.Match2 where

import Data.Either (partitionEithers)
import Data.Either.Combinators (mapLeft)
import Data.Foldable (find, minimumBy, null)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Base (Symbol)

data OrderType = Buy | Sell deriving (Show, Eq)

data Order = Order
  { orderId :: Int,
    orderType :: OrderType,
    orderPrice :: Int,
    quantity :: Int
  }
  deriving (Show)

data OOrder (dir::Symbol)= OOrder
  { oOrderId :: Int,
    oOrderPrice :: Int,
    oOrderQuantity :: Int
  }
  deriving (Show)

data BuyOrder = BuyOrder
  { buyOrderId :: Int,
    buyOrderPrice :: Int,
    buyOrderQuantity :: Int
  }
  deriving (Show)

data SellOrder = SellOrder
  { sellOrderId :: Int,
    sellOrderPrice :: Int,
    sellOrderQuantity :: Int
  }
  deriving (Show)

data PartialOrder = PartialOrder
  { partialOrderId :: Int,
    partialOrderType :: OrderType,
    partialOrderPrice :: Int,
    partialOrderQuantity :: Int
  }
  deriving (Show)

data Trade = Trade
  { tradeBuyId :: Int,
    tradeSellId :: Int,
    tradePrice :: Int,
    tradeQuantityTraded :: Int
  }
  deriving (Show,Eq)

match :: [Order] -> ([Order], [Trade])
match orders =
  let (buys, sells) =
        foldl
          ( \(b, s) Order {orderType, orderId,quantity,orderPrice} -> case orderType of
              Buy -> (Map.insert orderId (OOrder orderId orderPrice quantity) b, s)
              Sell -> (b, Map.insert orderId (OOrder  orderId orderPrice quantity) s))
          
          (Map.empty, Map.empty)
          orders
      -- sells = Map.fromList $
      --     mapMaybe
      --       ( \o@Order {orderType, orderId} -> case orderType of
      --           Sell -> Just (orderId, o)
      --           Buy -> Nothing
      --       )
      --       orders
      (a, b) = partitionEithers $ go (buys ) (sells )
      c = foldl (<>) [] a
   in (c, b)
  where
    go :: Map Int (OOrder "Buy") -> Map Int (OOrder "Sell") -> [Either [Order] Trade]
    --go buys sells = fromMaybe [Left $ Map.elems buys <> Map.elems sells] $ do
    go buys sells = 
      
      fromMaybe [] $ do
      OOrder {oOrderId = sellOrderId, oOrderPrice = sellPrice, oOrderQuantity = sellQuantity} <-
        minimumByMaybe (\OOrder {oOrderPrice = price1} OOrder {oOrderPrice = price2} -> compare price1 price2) sells
      OOrder {oOrderId = buyOrderId, oOrderQuantity = buyQuantity} <-
        find (\OOrder {oOrderPrice = price2} -> sellPrice <= price2) $ Map.elems buys
      let tradeQuantity = min buyQuantity sellQuantity
      let updateOrders = Map.update \o@OOrder {oOrderQuantity} ->
            let nQuantity = oOrderQuantity - tradeQuantity
             in if nQuantity > 0
                  then Just $ o {oOrderQuantity = nQuantity}
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
