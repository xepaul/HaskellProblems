module WikiQuestions.Problem62 where

import WikiQuestions.WikiTypes (Tree (Branch, Empty))


-- | Wiki Problem 62. Collect the internal nodes of a binary tree in a list
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch v l r) = v : (internals l ++ internals r)