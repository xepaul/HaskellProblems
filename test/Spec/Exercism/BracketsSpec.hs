{-# LANGUAGE RecordWildCards #-}
module Spec.Exercism.BracketsSpec where
import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

import Exercism.Brackets (arePaired)

-- from https://github.com/exercism/haskell/blob/main/exercises/practice/matching-brackets/test/Tests.hs

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} spec

spec :: Spec
spec = describe "arePaired" $ for_ cases test
  where
    test Case{..} = it description $ arePaired input `shouldBe` expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: Bool
                 }

cases :: [Case]
cases = [ Case { description = "paired square brackets"
               , input       = "[]"
               , expected    = True
               }
        , Case { description = "empty string"
               , input       = ""
               , expected    = True
               }
        , Case { description = "unpaired brackets"
               , input       = "[["
               , expected    = False
               }
        , Case { description = "wrong ordered brackets"
               , input       = "}{"
               , expected    = False
               }
        , Case { description = "wrong closing brackets"
               , input       = "{]"
               , expected    = False
               }
        , Case { description = "paired with whitespace"
               , input       = "{ }"
               , expected    = True
               }
        , Case { description = "partially paired brackets"
               , input       = "{[])"
               , expected    = False
               }
        , Case { description = "simple nested brackets"
               , input       = "{[]}"
               , expected    = True
               }
        , Case { description = "several paired brackets"
               , input       = "{}[]"
               , expected    = True
               }
        , Case { description = "paired and nested brackets"
               , input       = "([{}({}[])])"
               , expected    = True
               }
        , Case { description = "unopened closing brackets"
               , input       = "{[)][]}"
               , expected    = False
               }
        , Case { description = "unpaired and nested brackets"
               , input       = "([{])"
               , expected    = False
               }
        , Case { description = "paired and wrong nested brackets"
               , input       = "[({]})"
               , expected    = False
               }
        , Case { description = "paired and incomplete brackets"
               , input       = "{}["
               , expected    = False
               }
        , Case { description = "too many closing brackets"
               , input       = "[]]"
               , expected    = False
               }
        , Case { description = "math expression"
               , input       = "(((185 + 223.85) * 15) - 543)/2"
               , expected    = True
               }
        , Case { description = "complex latex expression"
               , input       = "\\left(\\begin{array}{cc} \\frac{1}{3} & x\\\\ \\mathrm{e}^{x} &... x^2 \\end{array}\\right)"
               , expected    = True
               }
        ]

-- 2e4715e581e5764c80f6e8e1c53a5c3ffbbd2659