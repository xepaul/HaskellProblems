module Exercism.Brackets (arePaired) where

arePaired :: String -> Bool
arePaired = go [] 
  where
    go bs [] = null bs
    go bs (y : ys)
      | (x : xs) <- bs, matchingOpenCloseBracket (x,y) = go xs ys
      | openingBracket y = go (y : bs) ys
      | closingBracket y = False
      | otherwise = go bs ys
    openingBracket = flip elem openBs
    closingBracket = flip elem closeBs
    matchingOpenCloseBracket = flip elem  $ zip openBs closeBs
    openBs = "({["
    closeBs = ")}]"