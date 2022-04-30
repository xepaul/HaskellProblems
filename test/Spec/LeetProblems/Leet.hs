{-# LANGUAGE ImportQualifiedPost #-}
module Spec.LeetProblems.Leet (tests) where

import Test.Hspec (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import LeetCode.Prob91DecodeWays (numDecodings,numDecodings')
import LeetCode.Prob3LengthOfLongestSubstring
import LeetCode.Prob1TwoSum
tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests Examples"
    [ 
       testCase "Dummy" $ True `shouldBe` True
      , testCase "leet p1 twoSum test case1" $ twoSum [2,7,11,15] 9 `shouldBe` Just (0,1)
      , testCase "leet p1 twoSum test case1" $ twoSum [3,2,4] 6 `shouldBe` Just (1,2)
      , testCase "leet p1 twoSum test case1" $ twoSum [3,3] 6 `shouldBe` Just (0,1)
      , testCase "leet p1 twoSum' test case1" $ twoSum' [2,7,11,15] 9 `shouldBe` Just (0,1)
      , testCase "leet p1 twoSum' test case1" $ twoSum' [3,2,4] 6 `shouldBe` Just (1,2)
      , testCase "leet p1 twoSum' test case1" $ twoSum' [3,3] 6 `shouldBe` Just (0,1)
      , testCase "leet p1 twoSum'' test case1" $ twoSum'' [2,7,11,15] 9 `shouldBe` Just (0,1)
      , testCase "leet p1 twoSum'' test case1" $ twoSum'' [3,2,4] 6 `shouldBe` Just (1,2)
      , testCase "leet p1 twoSum'' test case1" $ twoSum'' [3,3] 6 `shouldBe` Just (0,1)
      , testCase "leet p3 lengthOfLongestSubstring test case1" $ lengthOfLongestSubstring "abcabcbb" `shouldBe` 3
      , testCase "leet p3 lengthOfLongestSubstring test case2" $ lengthOfLongestSubstring "bbbbb" `shouldBe` 1
      , testCase "leet p3 lengthOfLongestSubstring test case3" $ lengthOfLongestSubstring "pwwkew" `shouldBe` 3
      , testCase "leet p91 numDecodings' test case1" $ numDecodings' "12" `shouldBe` 2
      , testCase "leet p91 numDecodings' test case2" $ numDecodings' "226" `shouldBe` 3
      , testCase "leet p91 numDecodings' test case3" $ numDecodings' "06" `shouldBe` 0
      , testCase "leet p91 numDecodings test case1" $ numDecodings "12" `shouldBe` 2
      , testCase "leet p91 numDecodings test case2" $ numDecodings "226" `shouldBe` 3
      , testCase "leet p91 numDecodings test case3" $ numDecodings "06" `shouldBe` 0
    ]