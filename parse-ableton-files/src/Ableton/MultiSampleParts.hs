{-# LANGUAGE DuplicateRecordFields #-}

-- | Collect and process multi-sample parts
--
-- This is useful to analyse an xisting Sampler instance.
module Ableton.MultiSampleParts (
    allMultiSampleParts
  , invertMultiSampleParts
  , multiSampleStats
  ) where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.IntervalMap.FingerTree (IntervalMap, Interval(..))
import Data.Set (Set)

import qualified Data.IntervalMap.FingerTree as IM
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import qualified GHC.Generics                as GHC
import qualified Generics.SOP                as SOP

import Ableton.Schema
import Ableton.Types
import XML.TypeDriven
import Util
import Util.SOP
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
  Inverted view: from sample range to settings
-------------------------------------------------------------------------------}

data InvMSP = InvMSP {
      chain    :: Name
    , key      :: (MidiNote, MidiNote)
    , velocity :: (Int, Int)
    , selector :: (Int, Int)
    }
  deriving (Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

type PerSample a = Map Name a
type PerOffset a = IntervalMap Int InvMSP

data InvStats = InvStats {
      -- | Is the same part of the same used multiple times?
      overlaps :: [(Interval Int, Interval Int, RecordDiff InvMSP)]

      -- | Are there samples that are used for non-singleton key ranges?
      --
      -- This implies that these samples would have to be transposed.
    , nonSingletonKeys :: Bool

      -- | All supported keys (if a sample supports a non-singleton key range,
      -- we include all keys here)
    , supportedKeys :: Set MidiNote
    }
  deriving (Show)

invertMultiSampleParts :: [MSP] -> PerSample (PerOffset MSP)
invertMultiSampleParts xs =
    repeatedly insert xs Map.empty
  where
    insert ::
         MSP
      -> PerSample (PerOffset MSP)
      -> PerSample (PerOffset MSP)
    insert msp@MSP{sample} =
        Map.alter (Just . insert' msp . fromMaybe IM.empty) sample

    insert' ::
         MSP
      -> PerOffset MSP
      -> PerOffset MSP
    insert' MSP{range = (SampleStart fr, SampleEnd to), ..} =
        IM.insert (Interval fr to) InvMSP{..}

multiSampleStats :: PerOffset MSP -> InvStats
multiSampleStats msps = InvStats {
      overlaps         = checkOverlap $ IM.toList msps
    , nonSingletonKeys = any isNonSingletonKey msps
    , supportedKeys    = Set.fromList $ foldMap sampleKeyRanges msps
    }
  where
    checkOverlap ::
         [(Interval Int, InvMSP)]
      -> [(Interval Int, Interval Int, RecordDiff InvMSP)]
    checkOverlap []                      = []
    checkOverlap [_]                     = []
    checkOverlap ((i, x) : (i', y) : zs)
            | IM.intervalsIntersect i i' =
                 (i, i', recordDiff x y) : checkOverlap ((i', y) : zs)
            | otherwise                  =
                                           checkOverlap ((i', y) : zs)

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
