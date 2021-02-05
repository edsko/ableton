-- | Parsing attributes
module XML.Attr (
    ParseAttr(..)
  ) where

import Data.XML.Types
import Data.Text (Text)
import Text.XML.Stream.Parse

import XML.Aux

{-------------------------------------------------------------------------------
  Class for attributes of different types
-------------------------------------------------------------------------------}

class ParseAttr a where
  parseAttr :: Name -> AttrParser a

instance ParseAttr Text where
  parseAttr = requireAttrCI

instance ParseAttr Int where
  parseAttr = attrRead  
