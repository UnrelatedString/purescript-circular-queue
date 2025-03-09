module Data.Array.Sparse.ST
 ( STReserveArray
 , new
 , length
 , size
 ) where

import Prelude

import Control.Monad.ST (ST, Region)
import Control.Monad.STRef (STRef, read, write)
import Data.Array.ST (STArray)
import Data.Array.ST as STArray

-- | An `STArray` which additionally tracks its length independently from the underlying
-- | JavaScript array, which may hold inaccessible empty slots past the exposed length.
data STReserveArray :: Region -> Type -> Type
data STReserveArray r a = STRA (STArray r a) (STRef r Int)
--type role STReserveArray nominal representational

-- Relies heavily on STArray having the same runtime representation as an immutable Array!
foreign import newArrayWithSize :: forall r a. Int -> ST r (STArray r a)

new :: forall r a. Int -> ST r (STReserveArray r a)
new 

-- | The length which contains elements.
length :: forall r a. STReserveArray r a -> ST r Int
length (STRA _ len) = read len

-- | The reserved size of the underlying array.
size :: forall r a. STReserveArray r a -> ST r Int
size (STRA arr _) = STArray.length arr
