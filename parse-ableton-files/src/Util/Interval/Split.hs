module Util.Interval.Split (
    Split
  , empty
  , modify
  ) where

import Data.Map (Map)
import Data.Map qualified as Map

import Util.Interval

-- | Map intervals to values
--
-- Invariant: the intervals do not overlap.
--
-- In order to preserve the invariant, we may have to split intervals when
-- inserting new values into the map; hence the name.
newtype Split v a = Split (Map (Interval v) a)
  deriving (Show, Functor)

empty :: Split v a
empty = Split Map.empty

modify ::
    (Ord v, Enum v)
  => a         -- ^ Initial value
  -> (a -> a)  -- ^ Update (applied to initial value if no value present)
  -> Interval v -> Split v a -> Split v a
modify e f i (Split im) = Split $
    let (intersect_i, rest) = Map.partitionWithKey (const . intersects i) im
    in Map.union (split e f i (Map.toList intersect_i)) rest

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

split :: forall v a.
     (Ord v, Enum v)
  => a                   -- ^ Initial value
  -> (a -> a)            -- ^ Update function
  -> Interval v          -- ^ Interval to insert
  -> [(Interval v, a)]   -- ^ Intersecting intervals
  -> Map (Interval v) a
split e f = \i -> Map.fromList . go i
  where
    -- PRE1:  all intervals in the list intersect with @i@
    -- PRE2:  the intervals in the list are non-empty and do not overlap
    -- PRE3:  i is non-empty
    -- POST1: function @f@ has been applied on (and only on) interval @i@
    -- POST2: the intervals in the result are non-empty and do not overlap
    go :: Interval v -> [(Interval v, a)] -> [(Interval v, a)]
    go i [] = [(i, f e)]
    go i js@((j, a) : js')
      --
      -- Rule out impossible cases
      --

      -- >  {...i...}
      -- >             {...j...}
      | high i < low j =
          error "split: PRE1.a"
      -- >             {...i...}
      -- >  {...j...}
      | high j < low i =
          error "split: PRE1.b"

      --
      -- Cases where one interval starts before the other
      --

      -- >  {...i...}     or  {...i...}
      -- >     {...j...}        {.j.}
      | low i < low j =
          -- > low i <= pred (low j)   (from guard)
          -- > low j <= high i         (from PRE1.a)
            (nonEmpty (low i) (pred (low j)), f e)
          : go (nonEmpty (low j) (high i)) js
      -- >     {...i...}  or    {.i.}
      -- >  {...j...}         {...j...}
      | low j < low i =
          -- > low j <= pred (low i)   (from guard)
          -- > low i <= high j         (from PRE1.b)
            (nonEmpty (low j) (pred (low i)), a)
          : go i ((nonEmpty (low i) (high j), a) : js')

      --
      -- At this point we have established the lower bounds are equal
      --

      -- > {.i.}
      -- > {...j...}
      | high i < high j =
          -- > low j         <= high i   (from PRE1.a)
          -- > succ (high i) <= high j   (from guard)
            (nonEmpty (low j) (high i), f a)
          : (nonEmpty (succ (high i)) (high j), a)
          : js'

      -- >  {...i...}
      -- >  {.j.}
      | high j < high i =
           -- > low i         <= high j   (from PRE1.b)
           -- > succ (high j) <= high i   (from guard)
            (nonEmpty (low i) (high j), f a)
          : go (nonEmpty (succ (high j)) (high i)) js'

      -- high j == high i
      -- > {..i..}
      -- > {..j..}
      | otherwise =
          (i, f a) : js'
