module Data.CircularQueue.ST
 ( STCircularQueue
 , run
 , new
 , fromArray
 , size
 , population
 , pop
 , peek
 , push
 , growPush
 , zealousGrowPush
 , bulldoze
 ) where

import Prelude

import Control.Monad.ST (ST, Region)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array.ST (STArray, length)
import Data.Array.ST as STArray
import Data.Maybe (Maybe(..), isJust)
import Data.CircularQueue (CircularQueue(..))

type Cursor r = STRef r Int

data STCircularQueue :: Region -> Type -> Type
data STCircularQueue r a = STOQ (STArray r a) (Cursor r) (Cursor r)

run :: forall a. (forall r. ST r (STCircularQueue r a)) -> CircularQueue a
run action = ST.run do
  STOQ ringo readRef writeRef <- action
  arr <- STArray.unsafeFreeze ringo -- like STArray.run, this can't leak
  read <- STRef.read readRef
  write <- STRef.read writeRef
  pure $ OQ arr read write

new :: forall r a. ST r (STCircularQueue r a)
new = STOQ <$> STArray.new <*> STRef.new 0 <*> STRef.new 0

fromArray :: forall r a. Array a -> ST r (STCircularQueue r a)
fromArray arr = do
  ringo <- STArray.new -- this has nothing to do with apples in case I confuse myself in a week
  void $ STArray.pushAll arr ringo
  STOQ ringo <$> STRef.new 0 <*> STRef.new 0

-- | Gets the size of the underlying buffer, including elements which have already been read.
size :: forall r a. STCircularQueue r a -> ST r Int
size (STOQ arr _ _) = length arr

-- | Gets the number of unread elements.
population :: forall r a. STCircularQueue r a -> ST r Int
population (STOQ arr read write) = do
  diff <- (-) <$> STRef.read write <*> STRef.read read
  mod diff <$> length arr

-- | Internal helper.
whenA :: forall m a. Applicative m => Boolean -> m a -> m (Maybe a)
whenA true action = Just <$> action
whenA false _ = pure Nothing

-- | Internal helper.
uncheckedPush :: forall r a. STArray r a -> Cursor r -> a -> ST r Unit
uncheckedPush arr writeRef elem = do
  len <- length arr
  write <- STRef.read writeRef
  void $ STArray.poke write elem arr
  void $ STRef.write (write + 1 `mod` len) writeRef

-- | Read and consume an element from the queue.
pop :: forall r a. STCircularQueue r a -> ST r (Maybe a)
pop stoq
  | STOQ arr readRef writeRef <- stoq = do
  result <- peek stoq
  when (isJust result) do
    len <- length arr
    read <- STRef.read readRef
    void $ STRef.write (read + 1 `mod` len) readRef
  pure result

-- | Get an element from the queue without reading/consuming it.
peek :: forall r a. STCircularQueue r a -> ST r (Maybe a)
peek (STOQ arr readRef writeRef) = do
  read <- STRef.read readRef
  write <- STRef.read writeRef
  join <$> whenA (read /= write) do
    STArray.peek read arr

-- | Attempt to add a new element to the end of the queue if there's space,
-- | returning `true` if successful and `false` if not.
push :: forall r a. STCircularQueue r a -> a -> ST r Boolean
push (STOQ arr readRef writeRef) elem = do
  read <- STRef.read readRef
  write <- STRef.read writeRef
  let successful = write /= read
  when successful do
    uncheckedPush arr writeRef elem
  pure successful

-- | Add a new element to the end of the queue, growing the buffer if needed.
-- | Growing the buffer can be expensive if the read cursor is nonzero, but consecutive
-- | grows do not incur this cost multiple times.
growPush :: forall r a. STCircularQueue r a -> a -> ST r Unit
growPush (STOQ arr readRef writeRef) elem = do
  read <- STRef.read readRef
  write <- STRef.read writeRef
  if read == 0 && write == 0 then do
    void $ STArray.push elem arr
  else if read == write then do
    -- Nuclear option!
    prefix <- STArray.splice 0 read [] arr
    void $ STArray.pushAll prefix arr
    void $ STArray.push elem arr
    void $ STRef.write 0 readRef
    void $ STRef.write 0 writeRef
  else do
    uncheckedPush arr writeRef elem

-- | Add a new element to the end of the queue, growing the buffer if it would wrap to
-- | index 0 regardless of the position of the read cursor.
zealousGrowPush :: forall r a. STCircularQueue r a -> a -> ST r Unit
zealousGrowPush (STOQ arr _ writeRef) elem = do
  write <- STRef.read writeRef
  if write == 0 then do
    void $ STArray.push elem arr
  else do
    uncheckedPush arr writeRef elem

-- | Add a new element to the end of the queue, overwriting the first
-- | unread element (returned) if needed.
bulldoze :: forall r a. STCircularQueue r a -> a -> ST r (Maybe a)
bulldoze stoq elem
  | STOQ arr readRef writeRef <- stoq = do
  read <- STRef.read readRef
  write <- STRef.read writeRef
  result <- join <$> whenA (read == write) do
    pop stoq
  uncheckedPush arr writeRef elem
  pure result
