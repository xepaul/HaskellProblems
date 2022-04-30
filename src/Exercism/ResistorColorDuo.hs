module Exercism.ResistorColorDuo where

data Color
  = Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Eq, Show, Enum, Bounded)

value :: (Color, Color) -> Int
value (a, b) = 10 * fromEnum a + fromEnum b

-- >>> fromEnum Brown
-- 1

-- >>>value (Blue,Grey)
-- 68
