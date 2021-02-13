module Util (
    repeatedly
  ) where

import Data.List (foldl')

repeatedly :: (a -> b -> b) -> ([a] -> b -> b)
repeatedly = flip . foldl' . flip
