module Data.CircularQueue.ST
 ( STCircularQueue
 ) where

import Prelude

import Control.Monad.ST (ST, Region, run)
import Control.Monad.ST.Ref (STRef)
import Control.Monad.ST.Ref as STRef
import Data.Array.ST (STArray)
import Data.Array.ST as STArray

data STCircularQueue :: Region -> Type -> Type
data STCircularQueue r a = STOQ (STArray r a) (STRef r Int) (STRef r Int)
