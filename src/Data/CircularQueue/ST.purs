module Data.CircularQueue.ST
 ( STCircularQueue
 ) where

import Prelude

import Control.Monad.ST (ST, Region, run)
import Control.Monad.STRef (STRef)
import Data.Array.Sparse.ST (STReserveArray)

data STCircularQueue :: Region -> Type -> Type
data STCircularQueue r a = STOQ (STReserveArray r a) (STRef r Int) (STRef r Int)
