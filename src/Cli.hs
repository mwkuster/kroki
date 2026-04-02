module Cli
  ( Options(..)
  , Command(..)
  , parseCli
  ) where

import Options.Applicative

data Command
  = WhoAmI
  | Reviews
  | Study
  deriving (Show, Eq)

data Options = Options
  { optToken        :: Maybe String
  , optBatchSize    :: Maybe Int
  , optRequeueAfter :: Maybe Int
  , optVerbose      :: Bool
  , optCommand      :: Command
  } deriving (Show, Eq)

parseCli :: IO Options
parseCli = execParser parserInfo

parserInfo :: ParserInfo Options
parserInfo =
  info
    (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "kroki: tiny WaniKani CLI"
   <> header "kroki" )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional tokenOption
    <*> optional batchSizeOption
    <*> optional requeueAfterOption
    <*> verboseSwitch
    <*> commandParser

tokenOption :: Parser String
tokenOption =
  strOption
    ( long "token"
   <> metavar "TOKEN"
   <> help "WaniKani API token (otherwise read WANIKANI_API_TOKEN)" )

batchSizeOption :: Parser Int
batchSizeOption =
  option auto
    ( long "batch-size"
   <> metavar "N"
   <> help "Max reviews to include in a study batch (overrides config batch_size)" )


requeueAfterOption :: Parser Int
requeueAfterOption =
  option auto
    ( long "requeue-after"
   <> metavar "K"
   <> help "Requeue a missed question K positions later (overrides config requeue_after)" )

verboseSwitch :: Parser Bool
verboseSwitch =
  switch
    ( long "verbose"
   <> short 'v'
   <> help "Print informational log messages" )

commandParser :: Parser Command
commandParser =
  hsubparser $
       command "whoami"
         (info (pure WhoAmI) (progDesc "Show current WaniKani user"))
    <> command "reviews"
         (info (pure Reviews) (progDesc "Show number of reviews available now"))
    <> command "study"
         (info (pure Study) (progDesc "Start a review batch (max N items)"))
