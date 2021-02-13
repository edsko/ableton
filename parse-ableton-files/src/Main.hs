module Main where

import Conduit
import Data.Conduit.Zlib (ungzip)
import Text.XML.Stream.Parse

import Ableton.MultiSampleParts
import CmdLine
import XML.TypeDriven
import qualified XML.Parser as P

main :: IO ()
main = do
    Options{..} <- getOptions
    mParsed     <- withSourceFile input $ \inp -> runConduit $
                        inp
                     .| ungzip
                     .| parseBytes def
                     .| P.runParser parse
    case mParsed of
      Nothing ->
        putStrLn "XML failed to parse"
      Just parsed ->
        case cmd of
          DumpParsed ->
            print $ parsed
          ShowMultiSampleParts -> do
            print $ allMultiSampleParts parsed
          InvertMultiSampleParts -> do
            let (inverted, stats) = invertMultiSampleParts $
                                      allMultiSampleParts parsed
            print inverted
            print stats
