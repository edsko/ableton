-- | Intended for qualified import
--
-- > import Util.Interval (Interval(..))
-- > import Util.Interval qualified as I
module Util.Interval (
    intersects
  , union
  , pretty
  , nonEmpty
    -- * Re-exports
  , Interval(..)
  , high
  , low
  , point
  ) where

import Data.IntervalMap.FingerTree (Interval(..), high, low, point)
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

union :: Ord v => Interval v -> Interval v -> Interval v
union (Interval fr to) (Interval fr' to') =
    Interval (min fr fr') (max to to')

pretty :: Show v => Interval v -> String
pretty (Interval fr to) = concat [show fr, "-", show to]

-- | Make non-empty interval
--
-- Throws a run-time exception if the interval is empty.
nonEmpty :: (HasCallStack, Ord v) => v -> v -> Interval v
nonEmpty fr to
  | fr <= to  = Interval fr to
  | otherwise = error "nonEmpty: not an empty interval"
