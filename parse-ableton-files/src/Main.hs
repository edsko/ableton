module Main where

import Conduit
import Data.Conduit.Zlib (ungzip)
import Text.XML.Stream.Parse

import Ableton.MultiSampleParts
import CmdLine
import XML.TypeDriven

import qualified Util.IntervalMap as IM
import qualified XML.Parser       as P

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
          ShowMSP -> do
            print $ allMSP parsed
          InvertMSP -> do
            let inverted = invertMSP $ allMSP parsed
            print $ IM.toList <$> inverted
            print $ statsMSP  <$> inverted
          SummariseMSP options -> do
            let inverted = invertMSP $ allMSP parsed
            print $ summariseMSP options inverted
