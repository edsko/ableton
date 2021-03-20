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
    writeFile "out/statistics.log" $ unlines [
        "# Statistics"
      , "Number of different sample ranges: " ++ show (Map.size sampleRanges)
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

data LUTs = LUTs {
      -- | Unique ID assigned to each sample range
      sampleRanges :: Map SampleRange SampleRangeId

      -- | Split (non-overlapping) selector ranges
    , splitSelector :: Split Int (Set SampleRangeId)

      -- | Split key ranges
    , splitKey :: Split MidiNote (Set SampleRangeId)

      -- | Split velocity ranges
    , splitVelocity :: Split Int (Set SampleRangeId)

      -- | Split articulation ranges
    , splitChain :: Split Int (Set SampleRangeId)

      -- | All unique sample ranges in the original data
    , uniqueSelector :: Set (Interval Int)

      -- | All unique key ranges in the original data
    , uniqueKey :: Set (Interval MidiNote)

      -- | All unique velocity ranges in the original data
    , uniqueVelocity :: Set (Interval Int)

      -- | All unique articulatiomn ranges
    , uniqueChain :: Set (Name, Interval Int)
    }
  deriving (Show)

newtype SampleRangeId = SampleRangeId Int
  deriving newtype (Show, Eq, Ord)

data SampleRange = SampleRange {
      sample   :: Name
    , range    :: (SampleStart, SampleEnd)
    }
  deriving (Show, Eq, Ord)

empty :: LUTs
empty = LUTs {
      sampleRanges   = Map.empty
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
      sampleRanges       = Map.insert sampleRange sampleRangeId sampleRanges
    , splitSelector      = modifySplit selector     splitSelector
    , splitKey           = modifySplit key          splitKey
    , splitVelocity      = modifySplit velocity     splitVelocity
    , splitChain         = modifySplit chainRange   splitChain
    , uniqueSelector     = Set.insert selector            uniqueSelector
    , uniqueKey          = Set.insert key                 uniqueKey
    , uniqueVelocity     = Set.insert velocity            uniqueVelocity
    , uniqueChain        = Set.insert (chain, chainRange) uniqueChain
    }
  where
    modifySplit ::
         (Ord v, Enum v)
      => Interval v
      -> Split v (Set SampleRangeId) -> Split v (Set SampleRangeId)
    modifySplit = Split.modify Set.empty (Set.insert sampleRangeId)

    sampleRange :: SampleRange
    sampleRange = SampleRange { sample = sample, range = range }

    sampleRangeId :: SampleRangeId
    sampleRangeId =
        case Map.lookup sampleRange sampleRanges of
          Nothing -> SampleRangeId $ Map.size sampleRanges
          Just id -> id

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

overlaps :: LUTs -> [Overlap SampleRangeId]
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
