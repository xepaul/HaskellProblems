{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module LeetCode.Prob7ReverseInteger where

-- https://leetcode.com/problems/reverse-integer/

import Data.Int (Int32)
import Prelude hiding (reverse)
import Prelude qualified as P

reverse :: Int32 -> Int32
reverse v =
  let r = signum (toInteger v) * (read @Integer $ P.reverse $ show $ abs v)
   in if (r > toInteger (maxBound @Int32)) || (r < toInteger (minBound @Int32))
        then 0
        else fromInteger r

-- >>> reverse (123)
-- 321
-- >>> reverse (-123)
-- -321
-- >>> reverse (120)
-- 21

-- >>> maxBound @Int
-- 9223372036854775807
