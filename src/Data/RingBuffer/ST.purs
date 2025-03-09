module Data.RingBuffer.ST
 ( 
 ) where

import Prelude

import Control.Monad.ST (Region, run)
import Data.Array.ST (STArray)

data STRingBuffer -- this is kind of unfortunate because it starts with "string" aaaa
