{-# LANGUAGE DuplicateRecordFields #-}

-- | Partial XML schema for Ableton files
module Ableton.Schema where

import Data.Data
import Data.Text (Text)
import XML.TypeDriven

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

{-------------------------------------------------------------------------------
  Domain definition (used lifted only)
-------------------------------------------------------------------------------}

data Domain =
    Ableton
  | AbletonDevicePreset
  | BranchPresets
  | Device
  | DevicePresets
  | GroupDevicePreset
  | InstrumentBranchPreset
  | KeyRange
  | Max
  | Min
  | MultiSampleMap
  | MultiSamplePart
  | MultiSampler
  | Name
  | Player
  | SampleParts
  | SelectorRange
  | VelocityRange

{-------------------------------------------------------------------------------
  Parser instances
-------------------------------------------------------------------------------}

deriving via ParseNode (Node Ableton)                instance Parse (Node Ableton)
deriving via ParseNode (Node AbletonDevicePreset)    instance Parse (Node AbletonDevicePreset)
deriving via ParseNode (Node BranchPresets)          instance Parse (Node BranchPresets)
deriving via ParseNode (Node Device)                 instance Parse (Node Device)
deriving via ParseNode (Node DevicePresets)          instance Parse (Node DevicePresets)
deriving via ParseNode (Node GroupDevicePreset)      instance Parse (Node GroupDevicePreset)
deriving via ParseNode (Node InstrumentBranchPreset) instance Parse (Node InstrumentBranchPreset)
deriving via ParseNode (Node KeyRange)               instance Parse (Node KeyRange)
deriving via ParseNode (Node Max)                    instance Parse (Node Max)
deriving via ParseNode (Node Min)                    instance Parse (Node Min)
deriving via ParseNode (Node MultiSampleMap)         instance Parse (Node MultiSampleMap)
deriving via ParseNode (Node MultiSamplePart)        instance Parse (Node MultiSamplePart)
deriving via ParseNode (Node MultiSampler)           instance Parse (Node MultiSampler)
deriving via ParseNode (Node Name)                   instance Parse (Node Name)
deriving via ParseNode (Node Player)                 instance Parse (Node Player)
deriving via ParseNode (Node SampleParts)            instance Parse (Node SampleParts)
deriving via ParseNode (Node SelectorRange)          instance Parse (Node SelectorRange)
deriving via ParseNode (Node VelocityRange)          instance Parse (Node VelocityRange)

{-------------------------------------------------------------------------------
  Ableton
-------------------------------------------------------------------------------}

data instance Attrs Ableton = Attrs_Ableton {
      creator :: Text
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required Ableton = Required_Ableton {
      -- No required children
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional Ableton =
    Ableton_GroupDevicePreset (Node GroupDevicePreset)
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  GroupDevicePreset
-------------------------------------------------------------------------------}

data instance Attrs GroupDevicePreset = Attrs_GroupDevicePreset {
      -- No attributes
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required GroupDevicePreset = Required_GroupDevicePreset {
      -- No required children
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional GroupDevicePreset =
    GroupDevicePreset_BranchPresets (Node BranchPresets)
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  BranchPresets
-------------------------------------------------------------------------------}

data instance Attrs BranchPresets = Attrs_BranchPresets {
      -- No attributes
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required BranchPresets = Required_BranchPresets {
      -- No required children
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional BranchPresets =
    BranchPresets_InstrumentBranchPreset (Node InstrumentBranchPreset)
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  InstrumentBranchPreset
-------------------------------------------------------------------------------}

data instance Attrs InstrumentBranchPreset = Attrs_InstrumentBranchPreset {
      id :: Int
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required InstrumentBranchPreset = Required_InstrumentBranchPreset {
      name :: Node Name
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional InstrumentBranchPreset =
    InstrumentBranchPreset_DevicePresets (Node DevicePresets)
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  Name
-------------------------------------------------------------------------------}

data instance Attrs Name = Attrs_Name {
      value :: Text
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required Name = Required_Name {
      -- No required children
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional Name
    -- No optional children
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  DevicePresets
-------------------------------------------------------------------------------}

data instance Attrs DevicePresets = Attrs_DevicePresets {
      -- No attributes
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required DevicePresets = Required_DevicePresets {
      -- No required children
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional DevicePresets =
    DevicePresets_AbletonDevicePreset (Node AbletonDevicePreset)
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  AbletonDevicePreset
-------------------------------------------------------------------------------}

data instance Attrs AbletonDevicePreset = Attrs_AbletonDevicePreset {
      id :: Int
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required AbletonDevicePreset = Required_AbletonDevicePreset {
      -- No required children
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional AbletonDevicePreset =
    AbletonDevicePreset_Device (Node Device)
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  Device
-------------------------------------------------------------------------------}

data instance Attrs Device = Attrs_Device {
      -- No attributes
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required Device = Required_Device {
      -- No required children
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional Device =
    Device_MultiSampler (Node MultiSampler)
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  MultiSampler
-------------------------------------------------------------------------------}

data instance Attrs MultiSampler = Attrs_MultiSampler {
      id :: Int
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required MultiSampler = Required_MultiSampler {
      -- No required children
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional MultiSampler =
    MultiSampler_Player (Node Player)
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  Player
-------------------------------------------------------------------------------}

data instance Attrs Player = Attrs_Player {
      -- No attributes
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required Player = Required_Player {
      -- No required children
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional Player =
    Player_MultiSampleMap (Node MultiSampleMap)
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  MultiSampleMap
-------------------------------------------------------------------------------}

data instance Attrs MultiSampleMap = Attrs_MultiSampleMap {
      -- No attributes
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required MultiSampleMap = Required_MultiSampleMap {
      -- No required children
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional MultiSampleMap =
    MultiSampleMap_SampleParts (Node SampleParts)
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  SampleParts
-------------------------------------------------------------------------------}

data instance Attrs SampleParts = Attrs_SampleParts {
      -- No attributes
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required SampleParts = Required_SampleParts {
      -- No required children
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional SampleParts =
    SampleParts_MultiSamplePart (Node MultiSamplePart)
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  MultiSamplePart
-------------------------------------------------------------------------------}

data instance Attrs MultiSamplePart = Attrs_MultiSamplePart {
      id :: Int
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required MultiSamplePart = Required_MultiSamplePart {
      keyRange      :: Node KeyRange
    , velocityRange :: Node VelocityRange
    , selectorRange :: Node SelectorRange
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional MultiSamplePart
    -- No optional children
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  KeyRange
-------------------------------------------------------------------------------}

data instance Attrs KeyRange = Attrs_KeyRange {
      -- No attributes
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required KeyRange = Required_KeyRange {
      min :: Node Min
    , max :: Node Max
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional KeyRange
    -- No optional children
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  VelocityRange
-------------------------------------------------------------------------------}

data instance Attrs VelocityRange = Attrs_VelocityRange {
      -- No attributes
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required VelocityRange = Required_VelocityRange {
      min :: Node Min
    , max :: Node Max
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional VelocityRange
    -- No optional children
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  SelectorRange
-------------------------------------------------------------------------------}

data instance Attrs SelectorRange = Attrs_SelectorRange {
      -- No attributes
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required SelectorRange = Required_SelectorRange {
      min :: Node Min
    , max :: Node Max
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional SelectorRange
    -- No optional children
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  Min
-------------------------------------------------------------------------------}

data instance Attrs Min = Attrs_Min {
      value :: Int
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required Min = RequiredMin {
      -- No required children
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional Min
    -- No optional children
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

{-------------------------------------------------------------------------------
  Max
-------------------------------------------------------------------------------}

data instance Attrs Max = Attrs_Max {
      value :: Int
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Required Max = RequiredMax {
      -- No required children
    }
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

data instance Optional Max
    -- No optional children
  deriving (Show, Data, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)
