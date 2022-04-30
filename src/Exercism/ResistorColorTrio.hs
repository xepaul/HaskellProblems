module Exercism.ResistorColorTrio where

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

value :: Resistor -> Int
value (Resistor (a, b,c)) = (10 * fromEnum a + fromEnum b) * (10^fromEnum  c)


label :: Resistor -> String
label resistor = error "You need to implement this function."

ohms :: Resistor -> Int
ohms resistor = error "You need to implement this function."

-- >>> value Resistor { bands=(Blue,Grey,Orange)}
-- 68000
