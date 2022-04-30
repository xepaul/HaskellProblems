module InteviewQuestions.Q1 where
import Data.Aeson (Value(Bool))


-- Follow tree with bool list, if false fo left if true go right. Either change Empty to Left of change leaf value

data Tree a = Empty
              | Leaf a
              | Node (Tree a) (Tree a)
              deriving Show

t1 =  Node
        (Node
          (Leaf 'a')
          (Leaf 'b'))
        (Node
          (Leaf 'c') 
          Empty)

-- >>> t1
-- Node (Node (Leaf 'a') (Leaf 'b')) (Node (Leaf 'c') Empty)


-- >>> walk t1 [True] 'e'
-- gg4

walk :: Tree a -> [Bool] -> a ->  Tree a
walk Empty [] v = Leaf v
walk Empty (_:_) _ =  error "gg2"
walk (Leaf _) [] v =  Leaf v
walk (Leaf _) [_] v =  Leaf v
walk (Leaf _) (_:_)  v =  error "gg3"
walk (Node _ _) [] v = error "gg4"
walk (Node l r) (False:xs) v =  Node (walk l xs v ) r
walk (Node l r) (True:xs ) v = Node  l  (walk r xs v )


-- >>> walk2 t1 [True,True] 'e'
-- Right (Node (Node (Leaf 'a') (Leaf 'b')) (Node (Leaf 'c') (Leaf 'e')))

walk2 :: Tree a -> [Bool] -> a -> Either String  (Tree a)
walk2 Empty [] v = Right $ Leaf v
walk2 Empty (_:_) _ =  Left "to short"
walk2 (Leaf _) [] v =  Right $ Leaf v
walk2 (Leaf _) [_] v =  Right $ Leaf v
walk2 (Leaf _) (_:_)  _ =  Left "toshort"
walk2 (Node _ _) [] _ = Left "to long"
walk2 (Node l r) (False:xs) v =  (`Node` r)  <$> walk2 l xs v
walk2 (Node l r) (True:xs ) v = Node l <$> walk2 r xs v