{-# LANGUAGE CPP #-}

module Ch05 where

import Prelude hiding (head, tail, length)
import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad
import qualified Data.List as List

class Queue q where
  empty :: q a
  isEmpty :: q a -> Bool

  snoc :: q a -> a -> q a
  headq :: q a -> a
  tailq :: q a -> q a
  length :: q a -> Int

fromList :: Queue q => [a] -> q a
fromList = foldl snoc empty

toList :: Queue q  => q a -> [a]
toList q | isEmpty q = []
         | otherwise = headq q : toList (tailq q)

-- newtype Queue a = Queue [a] deriving (Read, Show)

data BatchedQueue a = BQ [a] [a]

checkf [] r = BQ (reverse r) []
checkf f r = BQ f r

instance Queue BatchedQueue where
  empty = BQ [] []
  isEmpty (BQ f r) = null f --because we have the invariant if front is empty, rear must be too
  snoc (BQ f r) x = checkf f (x:r)

  headq (BQ [] _) = error "BatchedQueue.head : the queue is empty"
  headq (BQ (x:f) r) = x

  tailq (BQ [] _) =  error "BatchedQueue.head : the queue is empty"
  tailq (BQ (x:f) r) = checkf f r
  length (BQ [] _) = 0
  length (BQ (x:f) []) = 1 + List.length f
  length (BQ (x:f) (y:r)) = 1 + List.length f + 1 + List.length r

showQueue :: (Queue q, Show a) => q a -> String
showQueue q = show $ (toList q)

instance (Show a) => (BatchedQueue a) where
  show q = showQueue q

instance (Arbitrary a) => Arbitrary (BatchedQueue a) where
    arbitrary = (liftM fromList) arbitrary

#if MIN_VERSION_QuickCheck(2,0,0)
#else
    coarbitrary (BatchedQueue _ front _ rear) =
        variant 0 . coarbitrary front . coarbitrary rear
#endif

-- Queue Tests
-- applying toList fromList gives back the same list
prop_toListFromListIdempotent xs = toList (fromList' xs) == xs
  where fromList' :: [a] -> BatchedQueue a
        fromList' = fromList

-- | Validates that the length of a queue is the same as the length of the
--   list generated from the queue.
prop_length_toList :: (Queue q, Eq (q a)) => q a -> Bool
prop_length_toList q = List.length (toList q) == length q
  -- where toList' :: BatchedQueue a -> [a]
  --       toList' = toList
