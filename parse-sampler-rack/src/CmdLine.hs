module CmdLine (
    Options(..)
  , getOptions
  ) where

import Options.Applicative

data Options = Options {
      input :: FilePath
    }
  deriving (Show)

getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (options <**> helper) $ mconcat [
          fullDesc
        , progDesc "Parse Ableton Sampler Instrument Rack data"
        , header "parse-sampler-rack - a parser for Ableton sampler data"
        ]

options :: Parser Options
options = Options
    <$> argument str (metavar "INPUT")
