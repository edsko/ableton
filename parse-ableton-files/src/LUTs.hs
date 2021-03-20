module LUTs (createLUTs) where

import Prelude hiding (id)

import Control.Monad
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Ableton.MultiSampleParts
import Ableton.Schema (Name, SampleStart, SampleEnd)
import Ableton.Types

import Util
import Util.Interval (Interval)
import Util.Interval qualified as I
import Util.Interval.Split (Split)
import Util.Interval.Split qualified as Split

createLUTs :: [MSP] -> IO ()
createLUTs msps = do
    writeFile "out/LUTs.log" $ show luts
    writeFile "out/rangeIDs.log" $ show rangeIds
    writeFile "out/statistics.log" $ unlines [
        "# Statistics"
      , "Number of different sample ranges: " ++ show (Map.size sampleIds)
      , "## Distribution over the various ranges"
      , "Unique combinations: " ++ show (product [
            Split.size splitSelector
          , Split.size splitKey
          , Split.size splitVelocity
          , Split.size splitChain
          ])
      , "### Selector"
      , show $ Set.size <$> splitSelector
      , "### Key"
      , show $ Set.size <$> splitKey
      , "### Velocity"
      , show $ Set.size <$> splitVelocity
      , "### Articulation"
      , show $ Set.size <$> splitChain
      , "## Ambiguous combinations"
      , show $ overlaps luts
      ]
    writeFile "out/selector_range_id.table" (toRangeId $ Split.keysSet splitSelector)
  where
    luts :: LUTs
    luts@LUTs{..} = simplify $ repeatedly insert msps empty

    rangeIds :: RangeIds
    rangeIds = assignRangeIds luts

data LUTs = LUTs {
      -- | Unique ID assigned to each sample range
      sampleIds :: Map SampleRange SampleId

      -- | Split (non-overlapping) selector ranges
    , splitSelector :: Split Int (Set SampleId)

      -- | Split key ranges
    , splitKey :: Split MidiNote (Set SampleId)

      -- | Split velocity ranges
    , splitVelocity :: Split Int (Set SampleId)

      -- | Split articulation ranges
    , splitChain :: Split Int (Set SampleId)

      -- | Unique selector ranges in the original data (possibly overlapping)
    , uniqueSelector :: Set (Interval Int)

      -- | Unique key ranges in the original data (possibly overlapping)
    , uniqueKey :: Set (Interval MidiNote)

      -- | Unique velocity ranges in the original data (possibly overlapping)
    , uniqueVelocity :: Set (Interval Int)

      -- | Unique chain ranges in the original data (possibly overlapping)
    , uniqueChain :: Set (Name, Interval Int)
    }
  deriving (Show)

newtype SampleId = SampleId Int
  deriving newtype (Show, Eq, Ord)

data SampleRange = SampleRange {
      sample   :: Name
    , range    :: (SampleStart, SampleEnd)
    }
  deriving (Show, Eq, Ord)

empty :: LUTs
empty = LUTs {
      sampleIds      = Map.empty
    , splitSelector  = Split.empty
    , splitKey       = Split.empty
    , splitVelocity  = Split.empty
    , splitChain     = Split.empty
    , uniqueSelector = Set.empty
    , uniqueKey      = Set.empty
    , uniqueVelocity = Set.empty
    , uniqueChain    = Set.empty
    }

insert :: MSP -> LUTs -> LUTs
insert MSP{..} LUTs{..} = LUTs{
      sampleIds      = Map.insert sampleRange sampleId sampleIds
    , splitSelector  = modifySplit selector   splitSelector
    , splitKey       = modifySplit key        splitKey
    , splitVelocity  = modifySplit velocity   splitVelocity
    , splitChain     = modifySplit chainRange splitChain
    , uniqueSelector = Set.insert selector            uniqueSelector
    , uniqueKey      = Set.insert key                 uniqueKey
    , uniqueVelocity = Set.insert velocity            uniqueVelocity
    , uniqueChain    = Set.insert (chain, chainRange) uniqueChain
    }
  where
    modifySplit ::
         (Ord v, Enum v)
      => Interval v
      -> Split v (Set SampleId) -> Split v (Set SampleId)
    modifySplit = Split.modify Set.empty (Set.insert sampleId)

    sampleRange :: SampleRange
    sampleRange = SampleRange { sample = sample, range = range }

    sampleId :: SampleId
    sampleId =
        case Map.lookup sampleRange sampleIds of
          Nothing -> SampleId $ Map.size sampleIds
          Just id -> id

{-------------------------------------------------------------------------------
  Assign IDs to each range
-------------------------------------------------------------------------------}

newtype RangeIdSelector = RangeIdSelector Int deriving newtype (Show, Num)
newtype RangeIdKey      = RangeIdKey      Int deriving newtype (Show, Num)
newtype RangeIdVelocity = RangeIdVelocity Int deriving newtype (Show, Num)
newtype RangeIdChain    = RangeIdChain    Int deriving newtype (Show, Num)

-- | Range IDs
--
-- Once all data is processed, we assign a unique ID to each /split/ range.
-- It is this range ID that the LUTs work with.
data RangeIds = RangeIds {
      -- | Unique ID assigned to each selector range
      rangesSelector :: Map (Interval Int) RangeIdSelector

      -- | Unique ID assigned to each key range
    , rangesKey :: Map (Interval MidiNote) RangeIdKey

      -- | Unique ID assigned to each velocity range
    , rangesVelocity :: Map (Interval Int) RangeIdVelocity

      -- | Unique ID assigned to each chain range
    , rangesChain :: Map (Interval Int) RangeIdChain
    }
  deriving (Show)

assignRangeIds :: LUTs -> RangeIds
assignRangeIds LUTs{..} = RangeIds {
      rangesSelector = numberRanges splitSelector
    , rangesKey      = numberRanges splitKey
    , rangesVelocity = numberRanges splitVelocity
    , rangesChain    = numberRanges splitChain
    }
  where
    numberRanges :: (Ord v, Num b) => Split v a -> Map (Interval v) b
    numberRanges xs =
        Map.fromList $
          zip (Set.toList (Split.keysSet xs)) (map fromInteger [0..])

{-------------------------------------------------------------------------------
  Simplification
-------------------------------------------------------------------------------}

simplify :: LUTs -> LUTs
simplify = mergeIdentical

mergeIdentical :: LUTs -> LUTs
mergeIdentical LUTs{..} = LUTs{
      splitSelector = merge splitSelector
    , splitKey      = merge splitKey
    , splitVelocity = merge splitVelocity
    , splitChain    = merge splitChain
    , ..
    }
  where
    merge :: (Ord v, Eq a) => Split v a -> Split v a
    merge = Split.mergeAdjacentIf $ \(i, xs) (j, ys) -> do
            guard (xs == ys)
            return (I.union i j, xs)

{-------------------------------------------------------------------------------
  Checking for overlaps
-------------------------------------------------------------------------------}

data Overlap a = Overlap {
      overlapSelector     :: Interval Int
    , overlapKey          :: Interval MidiNote
    , overlapVelocity     :: Interval Int
    , overlapArticulation :: Interval Int
    , overlapValues       :: [a] -- ^ At least two values
    }
  deriving (Show, Functor)

overlaps :: LUTs -> [Overlap SampleId]
overlaps LUTs{..} = [
        overlap
      | (selector, samplesForSelector) <- Split.toList splitSelector
      , (key,      samplesForKey)      <- Split.toList splitKey
      , (velocity, samplesForVelocity) <- Split.toList splitVelocity
      , (chain,    samplesForChain)    <- Split.toList splitChain
      , let intersection = foldr1 Set.intersection [
                samplesForSelector
              , samplesForKey
              , samplesForVelocity
              , samplesForChain
              ]
      , Set.size intersection > 1
      , let overlap = Overlap {
                overlapSelector     = selector
              , overlapKey          = key
              , overlapVelocity     = velocity
              , overlapArticulation = chain
              , overlapValues       = Set.toList intersection
              }
      ]

{-------------------------------------------------------------------------------
  Generate JavaScript
-------------------------------------------------------------------------------}

toRangeId :: Set (Interval Int) -> String
toRangeId _ = "table"
