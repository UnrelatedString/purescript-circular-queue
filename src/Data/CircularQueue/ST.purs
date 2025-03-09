module Data.CircularQueue.ST
 ( STCircularQueue
 ) where

import Prelude

import Control.Monad.ST (Region, run)
import Data.Array.ST (STArray)

data STCircularQueue
