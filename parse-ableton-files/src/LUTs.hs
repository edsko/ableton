module LUTs (createLUTs) where

import Prelude hiding (id)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Ableton.MultiSampleParts
import Ableton.Schema (Name, SampleStart, SampleEnd)

import Util
import Util.Interval
import Util.Interval.Split (Split)
import Util.Interval.Split qualified as Split

createLUTs :: [MSP] -> IO ()
createLUTs msps = do
    print luts
    putStrLn $ "# Statistics"
    putStrLn $ "Number of different sample ranges: " ++ show (Map.size sampleRanges)
    putStrLn $ "Distribution over the selector ranges:"
    print (Set.size <$> selectorsSplit)
  where
    luts :: LUTs
    luts@LUTs{..} = repeatedly insert msps empty

data LUTs = LUTs {
      -- | Unique ID assigned to each sample range
      sampleRanges :: Map SampleRange SampleRangeId

      -- | Selector ranges split into unique ranges
    , selectorsSplit :: Split Int (Set SampleRangeId)

      -- | All unique sample ranges in the original data
      --
      -- This is mostly for debugging.
    , selectorsUnique :: Set (Interval Int)
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
      sampleRanges    = Map.empty
    , selectorsSplit  = Split.empty
    , selectorsUnique = Set.empty
    }

insert :: MSP -> LUTs -> LUTs
insert MSP{..} LUTs{..} = LUTs{
      sampleRanges    = Map.insert sampleRange sampleRangeId sampleRanges
    , selectorsSplit  = Split.modify Set.empty (Set.insert sampleRangeId) selector selectorsSplit
    , selectorsUnique = Set.insert selector selectorsUnique
    }
  where
    sampleRange :: SampleRange
    sampleRange = SampleRange { sample = sample, range = range }

    sampleRangeId :: SampleRangeId
    sampleRangeId =
        case Map.lookup sampleRange sampleRanges of
          Nothing -> SampleRangeId $ Map.size sampleRanges
          Just id -> id
