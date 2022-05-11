
{-# LANGUAGE NoImplicitPrelude #-}
module InterviewQuestions.CustomList where
  
import Control.Applicative (Applicative ((<*>), pure), (<$>))
import Control.Monad (Functor, Monad ((>>=)))
import GHC.Base (Functor(fmap))
import GHC.Num (Num((+)))
import GHC.Show (Show)
import Data.Eq (Eq((==)))
import Data.Function (flip)
import Data.Int (Int)

data CList a = CEmpty
             | CCons a (CList a)
             deriving (Show,Eq)

instance Functor CList where
  fmap _  CEmpty = CEmpty
  fmap f  (CCons a r) = CCons (f a) (fmap f r)

instance Applicative  CList where
  pure a = CCons a CEmpty
  (<*>) CEmpty _ = CEmpty
  (<*>) (CCons f ar) r = cconcat (go f r) (ar <*> r )
    where
      go :: (a -> b) -> CList a -> CList b
      go _ CEmpty = CEmpty
      go f' (CCons a r') = CCons (f' a ) (go f' r' )

cconcat :: CList a -> CList a -> CList a
cconcat CEmpty r = r
cconcat (CCons a l)  r= CCons a  (cconcat l r)

instance Monad CList where
  (>>=)  CEmpty _ = CEmpty
  (>>=)  b f = flatten (f <$> b)

flatten :: CList (CList a) -> CList a
flatten = go CEmpty
  where
    go :: CList a -> CList (CList a) -> CList a
    go _ CEmpty = CEmpty
    go s (CCons a r') = cconcat a (go s r')

(-:) ::  CList a -> a -> CList a
(-:) = flip CCons

infixl 5 -:
l3 :: CList Int
l3 =  CEmpty -: 1 -: (2::Int)

l1 :: CList Int
l1 = CEmpty -: 1 -: (2::Int)
l2 :: CList Int
l2 = CEmpty -: 3 -: (4::Int)

listofLists :: CList (CList Int)
listofLists = CCons l2 (CCons l1 CEmpty)

t1 = pure (+ 1) <*> l1
t2 = (+ 1) <$> l1

-- >>> t1 == t2
-- >>> flatten listofLists
-- >>> l1 >>= (\a -> CCons a (CCons a CEmpty))
-- >>> cconcat l2 l1
-- >>> pure (\a -> a+1) <*> l1
-- >>>  CEmpty <*> (CCons (1::Int) CEmpty)
-- >>> (CCons (\a -> a+2)  (CCons (\a -> a+1) CEmpty))  <*> (CCons (1::Int) CEmpty)
-- >>> (CCons (\a -> a+2)  (CCons (\a -> a+100) CEmpty))  <*> (CCons (10::Int) (CCons (1::Int) CEmpty))
-- True
-- CCons 4 (CCons 3 (CCons 2 (CCons 1 CEmpty)))
-- CCons 2 (CCons 2 (CCons 1 (CCons 1 CEmpty)))
-- CCons 4 (CCons 3 (CCons 2 (CCons 1 CEmpty)))
-- CCons 3 (CCons 2 CEmpty)
-- CEmpty
-- CCons 3 (CCons 2 CEmpty)
-- CCons 12 (CCons 3 (CCons 110 (CCons 101 CEmpty)))
