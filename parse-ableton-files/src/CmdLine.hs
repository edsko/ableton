module CmdLine (
    Options(..)
  , Command(..)
  , getOptions
  ) where

import Options.Applicative

data Options = Options {
      input :: FilePath
    , cmd   :: Command
    }
  deriving (Show)

data Command =
    -- | Dump the parsed XML
    DumpParsed

    -- | Show all multi-sample parts
  | ShowMultiSampleParts

    -- | Invert the multi-sample parts (from part of the sample to settings)
  | InvertMultiSampleParts
  deriving (Show)

getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (parseOptions <**> helper) $ mconcat [
          fullDesc
        , progDesc "Parse Ableton Sampler Instrument Rack data"
        , header "parse-sampler-rack - a parser for Ableton sampler data"
        ]

{-------------------------------------------------------------------------------
  Parsers
-------------------------------------------------------------------------------}

parseOptions :: Parser Options
parseOptions = Options
    <$> argument str (metavar "INPUT")
    <*> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
      aux "dump-parsed"             (pure DumpParsed)             "Dump parsed XML"
    , aux "show-multisampleparts"   (pure ShowMultiSampleParts)   "Show all multi-sample parts"
    , aux "invert-multisampleparts" (pure InvertMultiSampleParts) "Show mapping from sample range to multi-sample settings"
    ]
  where
    aux :: String -> Parser Command -> String -> Mod CommandFields Command
    aux c p h = command c $ info (p <**> helper) $ mconcat [
          progDesc h
        ]
