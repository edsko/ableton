{-# LANGUAGE DuplicateRecordFields #-}

-- | Collect and process multi-sample parts
--
-- This is useful to analyse an xisting Sampler instance.
module Ableton.MultiSampleParts (
    allMSP
  , invertMSP
  , statsMSP
  , summariseMSP
  ) where

import Control.Monad.State (State)
import Data.Bifunctor
import Data.IntervalMap.FingerTree (IntervalMap)
import Data.List (sortOn, intercalate)
import Data.Map (Map)
import Data.Set (Set)

import qualified Control.Monad.State as State
import qualified Data.Foldable       as Foldable
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import qualified GHC.Generics        as GHC
import qualified Generics.SOP        as SOP

import Ableton.Schema
import Ableton.Types
import CmdLine
import XML.TypeDriven
import Util
import Util.Interval
import Util.SYB

import qualified Util.IntervalMap as IM

{-------------------------------------------------------------------------------
  Our own view on multi-sample parts
-------------------------------------------------------------------------------}

data MSP = MSP {
      chain    :: Name
    , key      :: (MidiNote, MidiNote)
    , velocity :: Interval Int
    , selector :: Interval Int
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
    , velocity :: Interval Int
    , selector :: Interval Int
    }
  deriving (Show, Eq, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

invertMSP :: [MSP] -> PerSample (PerOffset [InvMSP])
invertMSP =
      Map.fromList
    . map (second (IM.fromList . mergeAdjacent . splitFst))
    . splitOn (\MSP{..} -> (sample, (interval range, InvMSP{..})))
    . sortOn (\MSP{..} -> (sample, range))
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

statsMSP :: PerOffset [InvMSP] -> InvStats
statsMSP msps = InvStats {
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
    checkOverlap (i:i':is) = intersects i i' || checkOverlap (i':is)

    isNonSingletonKey :: InvMSP -> Bool
    isNonSingletonKey InvMSP{key = (fr, to)} = fr /= to

    sampleKeyRanges :: InvMSP -> [MidiNote]
    sampleKeyRanges InvMSP{key = (fr, to)} = [fr .. to]

{-------------------------------------------------------------------------------
  Summarize
-------------------------------------------------------------------------------}

-- | Summary
--
-- The levels of this map are
--
-- 1. Note
-- 2. Chain (collated "A/B/C" if the same sample is used for multiple chains)
-- 3. Velocity range (collated if overlap /and/ different selector ranges)
-- 4. Selector range
data Summary =
    Summary (Map MidiNote NoteSummary)

-- | All chains, velocities and selectors available for this note
data NoteSummary =
    NoteSummary (Map Name ChainSummary)

-- | Velocities and selectors available for this chain
data ChainSummary =
    ChainSummary (Map (Set (Interval Int)) VelocitySummary)

-- | Selector ranges available at this velocity
--
-- In other words, how many (randomly chosen) samples are available for this
-- note in this chain at this velocity?
data VelocitySummary =
    VelocitySummary (Set (Interval Int))

-- | Number of different samples available at this velocity
velocitySummarySize :: VelocitySummary -> Int
velocitySummarySize (VelocitySummary vs) = Set.size vs

instance Show Summary where
  show (Summary ns) = showMap show show ns

instance Show NoteSummary where
  show (NoteSummary cs) = showMap getName show cs

instance Show ChainSummary where
  show (ChainSummary vs) = intercalate " " $ [
        showMap (showSet showInterval) show vs
      , "(" ++ show (Map.size vs) ++ " velocities,"
      , show (sum (map velocitySummarySize (Map.elems vs))) ++ " total samples)"
      ]

instance Show VelocitySummary where
  show (VelocitySummary vs) = showSet showInterval vs

summariseMSP ::
     OptionsSummarise
  -> PerSample (PerOffset [InvMSP])
  -> ([String], Summary)
summariseMSP OptionsSummarise{..} = \msps ->
      let msps'    :: [InvMSP]
          warnings :: [String]
          (msps', warnings) = flip State.runState [] $
                mapM collateChains $ concatMap Foldable.toList (Map.elems msps)
      in ( warnings
         , Summary
         . fmap (NoteSummary . fmap (ChainSummary . (group collateVelocityRanges Prelude.id)))
         . fmap (group Prelude.id Prelude.id)
         . group Prelude.id expand
         $ msps'
         )
  where
    group ::
         (Ord a, Ord k)
      => ([(a, [b])] -> [(k, v)])
      -> (x -> (a, b))
      -> [x] -> Map k v
    group f g = Map.fromList . f . splitSortedOn g

    collateChains :: [InvMSP] -> State [String] InvMSP
    collateChains msps
        | Just msp' <- allEqualUpTo (\msp -> (msp :: InvMSP) { chain = chainsCombined }) msps =
            return msp'
        | otherwise = do
            State.modify (\errs -> errs ++ ["Warning: using first element in " ++ show msps])
            return (head msps)
      where
        chains :: Set Name
        chains = Set.fromList $ map (\InvMSP{..} -> chain) msps

        chainsCombined :: Name
        chainsCombined = Name $ showSet getName chains

    expand :: InvMSP -> (MidiNote, (Name, (Interval Int, Interval Int)))
    expand InvMSP{key = (n, n'), ..}
      | n == n'   = (n, (chain, (velocity, selector)))
      | otherwise = error "expand: non-singleton key range"

    collateVelocityRanges ::
         [(Interval Int, [Interval Int])]
      -> [(Set (Interval Int), VelocitySummary)]
    collateVelocityRanges
      | not optCollateVelocities =
          map $ \(i, xs) -> (Set.singleton i, VelocitySummary (Set.fromList xs))
      | otherwise = \case
          []                              -> []
          ((i, Set.fromList -> xs) : xss) -> go (Set.singleton i, xs) xss
      where
        go :: (Set (Interval Int), Set (Interval Int)) -- Accumulator
           -> [(Interval Int, [Interval Int])]
           -> [(Set (Interval Int), VelocitySummary)]
        go (is, acc) [] = [(is, VelocitySummary acc)]
        go (is, acc) ((i, Set.fromList -> xs) : xss)
          | any (intersects i) is
          , Set.disjoint acc xs
          = go (Set.insert i is, acc `Set.union` xs) xss
          | otherwise
          = (is, VelocitySummary acc) : go (Set.singleton i, xs) xss

{-------------------------------------------------------------------------------
  Translate from the XML structure into our own more manageable type
-------------------------------------------------------------------------------}

allMSP :: Node Ableton -> [MSP]
allMSP = concatMap multiSampleParts . collect

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
    , velocity = Interval velocityMin velocityMax
    , selector = Interval selectorMin selectorMax
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
