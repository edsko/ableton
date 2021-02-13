{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Prelude hiding (id)

import Conduit
import Data.Conduit.Zlib (ungzip)
import Text.XML.Stream.Parse
import Data.Generics

import Ableton.Schema
import CmdLine
import XML.TypeDriven
import qualified XML.Parser as P

main :: IO ()
main = do
    Options{..} <- getOptions
    processed :: Maybe (Node Ableton) <-
      withSourceFile input $ \inp -> runConduit $
           inp
        .| ungzip
        .| parseBytes def
        .| P.runParser parse
    print $ processed

allMultiSamplePart :: Node Ableton -> [Node MultiSamplePart]
allMultiSamplePart = everything (++) (mkQ [] (:[]))

lacksKeyRange :: Node MultiSamplePart -> Bool
lacksKeyRange (Node _ _ opt) =
    case opt of
      [] -> True
      _  -> False
