module Util.IntervalMap (
    intervalsIntersect
  , toList
  , fromList
  ) where

import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import qualified Data.IntervalMap.FingerTree as IM

import Util

-- | Check if two intervals overlap
--
-- >  fr     to              fr     to      fr     to       fr   to
-- >  /-------\              /-------\      /-------\        /---\
-- >  |       |              |       |      |       |        |   |
-- >  |   /---+---\      /---+---\   |      | /---\ |      /-+---+-\
-- >  |   |   |   |  or  |   |   |   |  or  | |   | |  or  | |   | |
-- >  \---+---/   |      |   \---+---/      \-+---+-/      | \---/ |
-- >      |       |      |       /            |   |        |       |
-- >      \-------/      \-------/            \---/        \-------/
-- >      fr'    to'     fr'    to'          fr' to'       fr'    to'
-- >
-- >       (A)                (B)              (C)            (D)
intervalsIntersect :: Ord v => Interval v -> Interval v -> Bool
intervalsIntersect (Interval fr to) (Interval fr' to') =
       (fr  <= fr' && fr' <= to ) -- (A) or (C)
    || (fr' <= fr  && fr  <= to') -- (B) or (D)

-- | Convert 'IntervalMap' to list, preserving intervals
toList :: Ord v => IntervalMap v a -> [(Interval v, a)]
toList xs =
    case IM.leastView xs of
      Nothing       -> []
      Just (x, xs') -> x : toList xs'

fromList :: Ord v => [(Interval v, a)] -> IntervalMap v a
fromList as = repeatedly (uncurry IM.insert) as IM.empty
