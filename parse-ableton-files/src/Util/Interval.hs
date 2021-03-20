module Util.Interval (
    intersects
  , showInterval
  , nonEmpty
    -- * Re-exports
  , Interval(..)
  , high
  , low
  ) where

import Data.IntervalMap.FingerTree (Interval(..), high, low)
import GHC.Stack

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
intersects :: Ord v => Interval v -> Interval v -> Bool
intersects (Interval fr to) (Interval fr' to') =
       (fr  <= fr' && fr' <= to ) -- (A) or (C)
    || (fr' <= fr  && fr  <= to') -- (B) or (D)

showInterval :: Show v => Interval v -> String
showInterval (Interval fr to) = concat [show fr, "-", show to]

-- | Make non-empty interval
--
-- Throws a run-time exception if the interval is empty.
nonEmpty :: (HasCallStack, Ord v) => v -> v -> Interval v
nonEmpty fr to
  | fr <= to  = Interval fr to
  | otherwise = error "nonEmpty: not an empty interval"
