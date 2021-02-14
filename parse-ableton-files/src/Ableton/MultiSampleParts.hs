{-# LANGUAGE DuplicateRecordFields #-}

-- | Collect and process multi-sample parts
--
-- This is useful to analyse an xisting Sampler instance.
module Ableton.MultiSampleParts (
    allMultiSampleParts
  , invertMultiSampleParts
  , multiSampleStats
  ) where

import Data.Bifunctor (second)
import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)
import Data.Set (Set)

import qualified Data.Map     as Map
import qualified Data.Set     as Set
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import Ableton.Schema
import Ableton.Types
import XML.TypeDriven
import Util
import Util.SYB

import qualified Util.IntervalMap as IM

{-------------------------------------------------------------------------------
  Our own view on multi-sample parts
-------------------------------------------------------------------------------}

data MSP = MSP {
      chain    :: Name
    , key      :: (MidiNote, MidiNote)
    , velocity :: (Int, Int)
    , selector :: (Int, Int)
    , sample   :: Name
    , range    :: (SampleStart, SampleEnd)
    }
  deriving (Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

type PerSample a = Map Name a
type PerOffset a = IntervalMap Int a

{-------------------------------------------------------------------------------
  Inverted view: from sample range to settings
-------------------------------------------------------------------------------}

data InvMSP = InvMSP {
      chain    :: Name
    , key      :: (MidiNote, MidiNote)
    , velocity :: (Int, Int)
    , selector :: (Int, Int)
    }
  deriving (Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

invertMultiSampleParts :: [MSP] -> PerSample (PerOffset [InvMSP])
invertMultiSampleParts =
      Map.fromList
    . map (second (IM.fromList . mergeAdjacent . splitFst))
    . splitOn (\MSP{..} -> (sample, (interval range, InvMSP{..})))
    . sortBy (comparing (\MSP{..} -> (sample, range)))
  where
    interval :: (SampleStart, SampleEnd) -> Interval Int
    interval (SampleStart fr, SampleEnd to) = Interval fr to

{-------------------------------------------------------------------------------
  Statistics
-------------------------------------------------------------------------------}

data InvStats = InvStats {
      -- | Does /part/ of one sample range overlap with /part/ of another?
      overlapPartSampleRange :: Bool

      -- | Is the /same/ sample range used for multiple parts?
    , overlapFullSampleRange :: Bool

      -- | Are there samples that are used for non-singleton key ranges?
      --
      -- This implies that these samples would have to be transposed.
    , nonSingletonKeys :: Bool

      -- | All supported keys (if a sample supports a non-singleton key range,
      -- we include all keys here)
    , supportedKeys :: Set MidiNote
    }
  deriving (Show)

multiSampleStats :: PerOffset [InvMSP] -> InvStats
multiSampleStats msps = InvStats {
      overlapPartSampleRange =
        checkOverlap $ map fst (IM.toList msps)
    , overlapFullSampleRange =
        any (\xs -> length xs > 1) msps
    , nonSingletonKeys =
        any (any isNonSingletonKey) msps
    , supportedKeys =
        Set.fromList $ foldMap (concatMap sampleKeyRanges) msps
    }
  where
    checkOverlap :: [Interval Int] -> Bool
    checkOverlap []        = False
    checkOverlap [_]       = False
    checkOverlap (i:i':is) = IM.intervalsIntersect i i' || checkOverlap (i':is)

    isNonSingletonKey :: InvMSP -> Bool
    isNonSingletonKey InvMSP{key = (fr, to)} = fr /= to

    sampleKeyRanges :: InvMSP -> [MidiNote]
    sampleKeyRanges InvMSP{key = (fr, to)} = [fr .. to]

{-------------------------------------------------------------------------------
  Translate from the XML structure into our own more manageable type
-------------------------------------------------------------------------------}

allMultiSampleParts :: Node Ableton -> [MSP]
allMultiSampleParts = concatMap multiSampleParts . collect

multiSampleParts :: Node InstrumentBranchPreset -> [MSP]
multiSampleParts ibp =
    map (mkMultiSamplePart name) (collect ibp)
  where
    Node{required = Required_InstrumentBranchPreset{
        name
      }} = ibp

mkMultiSamplePart :: Name -> Node MultiSamplePart -> MSP
mkMultiSamplePart chain msp = MSP {
      chain
    , key      = (keyRangeMin, keyRangeMax)
    , velocity = (velocityMin, velocityMax)
    , selector = (selectorMin, selectorMax)
    , sample   = fileRefName
    , range    = (sampleStart, sampleEnd)
    }
  where
    Node{required = Required_MultiSamplePart{
        keyRange
      , velocityRange
      , selectorRange
      , sampleStart
      , sampleEnd
      , sampleRef
      }} = msp

    Node{required = Required_KeyRange{
        min = Min keyRangeMin
      , max = Max keyRangeMax
      }} = keyRange

    Node{required = Required_VelocityRange{
        min = Min velocityMin
      , max = Max velocityMax
      }} = velocityRange

    Node{required = Required_SelectorRange{
        min = Min selectorMin
      , max = Max selectorMax
      }} = selectorRange

    Node{required = Required_SampleRef{
        fileRef
      }} = sampleRef

    Node{required = Required_FileRef{
        name = fileRefName
      }} = fileRef
