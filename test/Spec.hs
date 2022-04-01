{-# LANGUAGE ImportQualifiedPost #-}
module Spec(main) where

import Test.Tasty
import Test.Tasty.Hedgehog       (HedgehogTestLimit (..))

import Spec.WikiQuestions.Wiki  qualified


main :: IO ()
main = defaultMain tests

limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 5)

tests :: TestTree
tests = localOption limit $ testGroup "use cases" [
     Spec.WikiQuestions.Wiki.tests
    ]

    