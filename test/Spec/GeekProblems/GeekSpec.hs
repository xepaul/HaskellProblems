{-# LANGUAGE ImportQualifiedPost #-}

module Spec.GeekProblems.GeekSpec (tests) where

import GeeksForGeeksProblems.Level1_1 (treeHeight)
import GeeksForGeeksProblems.Types (Tree (..), valuesAllUnique, treeIsSorted)
import Test.Hspec (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Geek tests"
    [ testCase "Dummy" $ True `shouldBe` True,
      testCase "Height of leaf is 1" $ treeHeight (Leaf (1::Int)) `shouldBe` 1,
      testCase "verify all values unique" $ valuesAllUnique treeEx1 `shouldBe` True,
      testCase "verify all values not unique" $ valuesAllUnique treeEx1NotUnique `shouldBe` False,
      testCase "verify sorted" $ treeIsSorted treeEx1 `shouldBe` True,
      testCase "verify not sorted" $ treeIsSorted treeEx1NotBST `shouldBe` False
    ]

-- >>> 1
-- 1

treeEx1 :: Tree Integer
treeEx1 =
  Node
    ( Node
        (Leaf 1)
        2
        (Leaf 3)
    )
    4
    ( Node
        (Leaf 5)
        6
        (Leaf 7)
    )

treeEx1NotBST :: Tree Integer
treeEx1NotBST =
  Node
    ( Node
        (Leaf 1)
        2
        (Leaf 3)
    )
    4
    ( Node
        (Leaf 5)
        8
        (Leaf 6)
    )

treeEx1NotUnique :: Tree Integer
treeEx1NotUnique =
  Node
    ( Node
        (Leaf 1)
        2
        (Leaf 3)
    )
    4
    ( Node
        (Leaf 5)
        6
        (Leaf 5)
    )