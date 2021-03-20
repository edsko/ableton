module LUTs (createLUTs) where

import Prelude hiding (id)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Ableton.MultiSampleParts
import Ableton.Schema (Name, SampleStart, SampleEnd)
import Ableton.Types

import Util
import Util.Interval
import Util.Interval.Split (Split)
import Util.Interval.Split qualified as Split

createLUTs :: [MSP] -> IO ()
createLUTs msps = do
    print luts
    putStrLn $ "# Statistics"
    putStrLn $ "Number of different sample ranges: " ++ show (Map.size sampleRanges)
    putStrLn $ "## Distribution over the various ranges"
    putStrLn $ "Unique combinations: " ++ show (product [
                   Split.size splitSelectors
                 , Split.size splitKeys
                 , Split.size splitVelocity
                 , Split.size splitArticulation
                 ])
    putStrLn $ "### Selectors"
    print    $ Set.size <$> splitSelectors
    putStrLn $ "### Keys"
    print    $ Set.size <$> splitKeys
    putStrLn $ "### Velocity"
    print    $ Set.size <$> splitVelocity
    putStrLn $ "### Articulation"
    print    $ Set.size <$> splitArticulation
  where
    luts :: LUTs
    luts@LUTs{..} = repeatedly insert msps empty

data LUTs = LUTs {
      -- | Unique ID assigned to each sample range
      sampleRanges :: Map SampleRange SampleRangeId

      -- | Range assigned to each chain (for articulation)
    , chainRanges :: Map Name (Interval Int)

      -- | Split (non-overlapping) selector ranges
    , splitSelectors :: Split Int (Set SampleRangeId)

      -- | Split key ranges
    , splitKeys :: Split MidiNote (Set SampleRangeId)

      -- | Split velocity ranges
    , splitVelocity :: Split Int (Set SampleRangeId)

      -- | Split articulation ranges
    , splitArticulation :: Split Int (Set SampleRangeId)

      -- | All unique sample ranges in the original data
    , uniqueSelectors :: Set (Interval Int)

      -- | All unique key ranges in the original data
    , uniqueKeys :: Set (Interval MidiNote)

      -- | All unique velocity ranges in the original data
    , uniqueVelocity :: Set (Interval Int)

      -- | All unique articulatiomn ranges
    , uniqueArticulation :: Set (Interval Int)
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
      sampleRanges       = Map.empty
    , chainRanges        = Map.empty
    , splitSelectors     = Split.empty
    , splitKeys          = Split.empty
    , splitVelocity      = Split.empty
    , splitArticulation  = Split.empty
    , uniqueSelectors    = Set.empty
    , uniqueKeys         = Set.empty
    , uniqueVelocity     = Set.empty
    , uniqueArticulation = Set.empty
    }

insert :: MSP -> LUTs -> LUTs
insert MSP{..} LUTs{..} = LUTs{
      sampleRanges       = Map.insert sampleRange sampleRangeId sampleRanges
    , chainRanges        = Map.insert chain articulation chainRanges
    , splitSelectors     = modifySplit selector     splitSelectors
    , splitKeys          = modifySplit key          splitKeys
    , splitVelocity      = modifySplit velocity     splitVelocity
    , splitArticulation  = modifySplit articulation splitArticulation
    , uniqueSelectors    = Set.insert selector     uniqueSelectors
    , uniqueKeys         = Set.insert key          uniqueKeys
    , uniqueVelocity     = Set.insert velocity     uniqueVelocity
    , uniqueArticulation = Set.insert articulation uniqueArticulation
    }
  where
    articulation :: Interval Int
    articulation =
        case Map.lookup chain chainRanges of
          Just i  -> i
          Nothing -> point (Map.size chainRanges)

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
