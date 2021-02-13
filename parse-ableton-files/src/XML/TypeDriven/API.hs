module XML.TypeDriven.API (
    Parse(..)
  , ParseAttr(..)
  ) where

import Data.Text (Text)
import Data.Typeable
import Data.XML.Types (Name)
import Text.XML.Stream.Parse (AttrParser)

import XML.Aux
import XML.Parser

{-------------------------------------------------------------------------------
  Class for parsers
-------------------------------------------------------------------------------}

-- | Parse XML nodes
--
-- Typeable is used for error messages
class Typeable a => Parse a where
  parse :: Parser (Maybe a)

-- | Parse XML attributes
class ParseAttr a where
  parseAttr :: Name -> AttrParser a

{-------------------------------------------------------------------------------
  Default 'ParseAttr' instances
-------------------------------------------------------------------------------}

instance ParseAttr Text where
  parseAttr = requireAttrCI

instance ParseAttr Int where
  parseAttr = attrRead