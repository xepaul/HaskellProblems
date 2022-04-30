{-# LANGUAGE ImportQualifiedPost #-}

module LeetCode.Prob20ValidParentheses where

-- https://leetcode.com/problems/valid-parentheses/

isValid :: String -> Bool
isValid s = maybe False (validateBrackets []) (parsBrackets s)
  where
    parsBrackets :: String -> Maybe String
    parsBrackets = traverse (\a -> if a `elem` "{}()[]" then Just a else Nothing)
    validateBrackets :: [Char] -> [Char] -> Bool
    validateBrackets [] [] = True
    validateBrackets (_ : _) [] = False
    validateBrackets allxs@(x : xs) (y : ys)
      | y `elem` "{([" = validateBrackets (y : allxs) ys
      | x == closeToOpen y = validateBrackets xs ys
      | otherwise = False
    validateBrackets [] (y : ys) = validateBrackets [y] ys
    closeToOpen a
      | a == '}' = '{'
      | a == ']' = '['
      | a == ')' = '('
      | otherwise = a

-- >>> isValid ""
-- True