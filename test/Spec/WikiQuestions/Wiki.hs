module Spec.WikiQuestions.Wiki (tests) where

import Test.Hspec (shouldBe)
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase )
import WikiQuestions.Problem1 ( dropEvery )
import WikiQuestions.Problem9 ( pack )

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests Examples"
    [ 
      testCase "wiki dropEvery' tests" $ dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"
      ,testCase "wiki pack' tests" $ 
        pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',  'a', 'd', 'e', 'e', 'e', 'e'] 
        `shouldBe` ["aaaa","b","cc","aa","d","eeee"]
    ]