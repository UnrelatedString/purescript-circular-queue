module Data.CircularQueue
 ( CircularQueue(..)
 , size
 , population
 , toArray
 , toUnfoldable
 ) where

import Prelude
import Data.Array (length, take, drop, slice)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Data.Tuple.Nested (type (/\), (/\))

-- | A datatype representing a (frozen, immutable) optionally-self-resizing circular queue.
-- | For this to behave as desired, it is expected to maintain the invariants that:
-- | - read and write are both < the length of the array
-- | - write cannot move past read (this either triggers a resize, moves both, or fails)
data CircularQueue a = OQ (Array a) Int Int

-- | Gets the size of the underlying buffer, including elements which have already been read.
-- Note that Array.length is O(1) :)
size :: forall a. CircularQueue a -> Int
size (OQ arr _ _) = length arr

-- | Gets the number of unread elements.
population :: forall a. CircularQueue a -> Int
population (OQ arr read write) = write - read `mod` length arr

-- | Creates an `Array` of every unread element.
-- | May be more performant than `toUnfoldable`.
toArray :: CircularQueue ~> Array
toArray (OQ arr read write)
  | read > write = drop read arr <> take write arr
  | otherwise = slice read write arr

-- | Unfolds every unread element into an `Unfoldable` structure.
toUnfoldable :: forall u a. Unfoldable u => CircularQueue a -> u a
toUnfoldable (OQ arr read write) = unfoldr generate read
  where generate :: Int -> Maybe (a /\ Int)
        generate i
          | i == write = Nothing
          | otherwise = (_ /\ (i + 1) `mod` length arr) <$> Array.index arr i
