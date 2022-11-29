{-# LANGUAGE ImportQualifiedPost #-}
module Spec.InterviewQuestions.Match2Spec (spec) where

import Test.Hspec (Spec, it, shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import InterviewQuestions.Match2 (Trade(..))
import InterviewQuestions.Match2
import qualified Data.Set as Set

spec :: Spec
spec =  do
        it "t1" $
          let processedInput = snd $ match input
          in processedInput `shouldBe` expected1

        it"t2" $
          let processedInput = snd $ match input2
          in processedInput `shouldBe` expected2
        it "t3" $
          let processedInput = snd $ match input3
          in processedInput `shouldBe` expected3

expected1 = [Trade {tradeBuyId = 2, tradeSellId = 1, tradePrice = 5000, tradeQuantityTraded = 50}]
expected2 = [Trade {tradeBuyId = 3, tradeSellId = 2, tradePrice = 5000, tradeQuantityTraded = 25},Trade {tradeBuyId = 3, tradeSellId = 1, tradePrice = 5001, tradeQuantityTraded = 25}]
expected3 =  [Trade {tradeBuyId = 2, tradeSellId = 1, tradePrice = 5000, tradeQuantityTraded = 50},Trade {tradeBuyId = 3, tradeSellId = 1, tradePrice = 5000, tradeQuantityTraded = 25}]



input :: [Order]
input =
  [ Order {orderId = 1, orderType = Sell, orderPrice = 5000, quantity = 100},
    Order {orderId = 2, orderType = Buy, orderPrice = 6000, quantity = 50}
  ]

input2 :: [Order]
input2 =
  [ Order {orderId = 1, orderType = Sell, orderPrice = 5001, quantity = 100},
    Order {orderId = 2, orderType = Sell, orderPrice = 5000, quantity = 25},
    Order {orderId = 3, orderType = Buy, orderPrice = 6000, quantity = 50}
  ]

input3 :: [Order]
input3 =
  [ Order {orderId = 1, orderType = Sell, orderPrice = 5000, quantity = 75},
    Order {orderId = 2, orderType = Buy, orderPrice = 6000, quantity = 50},
    Order {orderId = 3, orderType = Buy, orderPrice = 6000, quantity = 50}
  ]
