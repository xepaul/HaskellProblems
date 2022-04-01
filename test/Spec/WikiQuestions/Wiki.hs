module Spec.WikiQuestions.Wiki (tests) where

import Test.Hspec (shouldBe)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import WikiQuestions.Problem1 (dropEvery)
import WikiQuestions.Problem61 (countLeaves)
import WikiQuestions.Problem61A (leaves)
import WikiQuestions.Problem62 (internals)
import WikiQuestions.Problem64 (layout)
import WikiQuestions.Problem9 (pack)
import WikiQuestions.WikiTypes (Tree (Branch, Empty))

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests Examples"
    [ testCase "wiki p1 dropEvery tests" $ dropEvery "abcdefghik" 3 `shouldBe` "abdeghk",
      testCase "wiki p9 pack tests" $
        pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
          `shouldBe` ["aaaa", "b", "cc", "aa", "d", "eeee"],
      testCase "wiki p61 countLeaves tests" $ countLeaves tree4 `shouldBe` 2,
      testCase "wiki p61a countLeaves tests" $ leaves tree4 `shouldBe` [4, 2],
      testCase "wiki p62 internals tests" $ internals tree4 `shouldBe` [1, 2],
      testCase "wiki p64 layout tests" $ layout tree64 `shouldBe` [('a', (1, 4)), ('c', (2, 3)), ('e', (3, 6)), ('g', (4, 5)), ('h', (5, 4)), ('k', (6, 2)), ('m', (7, 3)), ('n', (8, 1)), ('p', (9, 3)), ('q', (10, 5)), ('s', (11, 4)), ('u', (12, 2))]
    ]

tree4 :: Tree Integer
tree4 =
  Branch
    1
    (Branch 2 Empty (Branch 4 Empty Empty))
    (Branch 2 Empty Empty)

tree64 :: Tree Char
tree64 =
  Branch
    'n'
    ( Branch
        'k'
        ( Branch
            'c'
            (Branch 'a' Empty Empty)
            ( Branch
                'h'
                ( Branch
                    'g'
                    (Branch 'e' Empty Empty)
                    Empty
                )                Empty
            )
        )
        (Branch 'm' Empty Empty)
    )
    ( Branch
        'u'
        ( Branch
            'p'
            Empty
            ( Branch
                's'
                (Branch 'q' Empty Empty)
                Empty
            )
        )
        Empty
    )
