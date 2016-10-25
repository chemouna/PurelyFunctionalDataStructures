module Ch05 where

import Prelude hiding (head, tail)
import Test.QuickCheck
import Test.QuickCheck.All

class Queue q where
  empty :: q a
  isEmpty :: q a -> Bool

  snoc :: q a -> a -> q a
  head :: q a -> a
  tail :: q a -> q a

fromList :: Queue q => [a] -> q a
fromList = foldl snoc empty

toList :: Queue q  => q a -> [a]
toList q | isEmpty q = []
         | otherwise = head q : toList (tail q)


-- Queue
data BatchedQueue a = BQ [a] [a]
  deriving (Eq, Show)

checkf [] r = BQ (reverse r) []
checkf f r = BQ f r

instance Queue BatchedQueue where
  empty = BQ [] []
  isEmpty (BQ f r) = null f --because we have the invariant if front is empty, rear must be too
  snoc (BQ f r) x = checkf f (x:r) 

  head (BQ [] _) = error "BatchedQueue.head : the queue is empty"
  head (BQ (x:f) r) = x

  tail (BQ [] _) =  error "BatchedQueue.head : the queue is empty"
  tail (BQ (x:f) r) = checkf f r


-- Queue Tests
-- applying toList fromList gives back the same list
prop_toListFromListIdempotent xs = toList (fromList' xs) == xs
  where fromList' :: [a] -> BatchedQueue a
        fromList' = fromList
