module WikiQuestions.WikiTypes where
data Tree a = Empty 
              | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)