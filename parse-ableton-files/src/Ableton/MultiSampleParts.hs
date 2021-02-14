{-# LANGUAGE DuplicateRecordFields #-}

-- | Collect and process multi-sample parts
--
-- This is useful to analyse an xisting Sampler instance.
module Ableton.MultiSampleParts (
    allMultiSampleParts
  , invertMultiSampleParts
  ) where

import Prelude hiding (id)
import qualified Prelude

import Control.Monad (guard)
import Data.Foldable (toList)
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

type PerSample a = Map Name a
type PerOffset a = IntervalMap Int MSP

data InvStats = InvStats {
      -- | Is the same part of the same used multiple times?
      overlaps :: [(MSP, MSP, RecordDiff MSP)]

      -- | Are there samples that are used for non-singleton key ranges?
      --
      -- This implies that these samples would have to be transposed.
    , nonSingletonKeys :: Bool

      -- | All supported keys (if a sample supports a non-singleton key range,
      -- we include all keys here)
    , supportedKeys :: Set MidiNote
    }
  deriving (Show)

invertMultiSampleParts ::
     [MSP]
  -> ( PerSample (PerOffset MSP)
     , PerSample InvStats
     )
invertMultiSampleParts xs =
    (inverted, stats <$> inverted)
  where
    inverted :: PerSample (PerOffset MSP)
    inverted = repeatedly insert xs Map.empty

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
    insert' msp@MSP{range = (SampleStart fr, SampleEnd to)} =
        IM.insert (Interval fr to) msp

stats :: PerOffset MSP -> InvStats
stats msps = InvStats {
      overlaps         = checkOverlap $ toList msps
    , nonSingletonKeys = any isNonSingletonKey msps
    , supportedKeys    = Set.fromList $ foldMap sampleKeyRanges msps
    }
  where
    checkOverlap :: [MSP] -> [(MSP, MSP, RecordDiff MSP)]
    checkOverlap []       = []
    checkOverlap [_]      = []
    checkOverlap (x:y:zs) = maybe Prelude.id (:) (overlaps x y)
                          $ checkOverlap (y:zs)

    -- Both ends of the range are inclusive
    overlaps :: MSP -> MSP -> Maybe (MSP, MSP, RecordDiff MSP)
    overlaps x@MSP{range = (SampleStart fr , SampleEnd to )}
             y@MSP{range = (SampleStart fr', SampleEnd to')} = do
        guard $ IM.intervalsIntersect (Interval fr to) (Interval fr' to')
        return (x, y, recordDiff x y)

    isNonSingletonKey :: MSP -> Bool
    isNonSingletonKey MSP{key = (fr, to)} = fr /= to

    sampleKeyRanges :: MSP -> [MidiNote]
    sampleKeyRanges MSP{key = (fr, to)} = [fr .. to]

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
