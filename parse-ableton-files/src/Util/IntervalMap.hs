module Util.IntervalMap (
    intervalsIntersect
  ) where

import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import qualified Data.IntervalMap.FingerTree as IM

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
