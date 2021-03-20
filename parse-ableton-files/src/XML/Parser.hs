-- | Thin wrapper around a parser for an XML node
--
-- Intended for qualified import.
module XML.Parser (
    Parser -- opaque
  , runParser
    -- * Construction
  , tag'
    -- * Combinators
  , choose
  , many
  , many'
  , force
  ) where

import Prelude hiding (maybe)

import Conduit
import Data.Typeable
import Data.XML.Types
import Data.String (fromString)

import Text.XML.Stream.Parse (AttrParser, NameMatcher)
import Text.XML.Stream.Parse qualified as X

newtype Parser a = Parser { runParser :: ConduitT Event Void IO a }
  deriving newtype (Functor, Applicative, Monad, MonadThrow)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

tag' :: String
     -> AttrParser attr
     -> (attr -> Parser a)
     -> Parser (Maybe a)
tag' name attrs =
    Parser . X.tag' nameMatcher attrs . (runParser .)
  where
    nameMatcher :: NameMatcher Name
    nameMatcher = X.matching (== (fromString name))

{-------------------------------------------------------------------------------
  Running
-------------------------------------------------------------------------------}

many :: Parser (Maybe a) -> Parser [a]
many = Parser . X.many . runParser

many' :: Parser (Maybe a) -> Parser [a]
many' = Parser . X.many' . runParser

force :: forall a. Typeable a => Parser (Maybe a) -> Parser a
force = Parser . X.force msg . runParser
  where
    msg :: String
    msg = "Missing " ++ show (typeRep (Proxy @a))

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

choose :: [Parser (Maybe a)] -> Parser (Maybe a)
choose = Parser . X.choose . map runParser
