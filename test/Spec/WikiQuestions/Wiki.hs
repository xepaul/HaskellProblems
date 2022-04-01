module Spec.WikiQuestions.Wiki (tests) where

import Test.Hspec (shouldBe)
import Test.Tasty
import Test.Tasty.HUnit
import WikiQuestions.Problem1 ( dropEvery )

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests Examples"
    [ 
      testCase "wiki dropEvery' tests" $ dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"
    ]