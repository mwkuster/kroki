module Cli
  ( Options(..)
  , Command(..)
  , StudyOpts(..)
  , parseCli
  ) where

import Options.Applicative

data StudyOpts = StudyOpts
  { studyBatchSize    :: Maybe Int
  , studyRequeueAfter :: Maybe Int
  } deriving (Show, Eq)

data Command
  = WhoAmI
  | Reviews
  | Study StudyOpts
  | Init
  deriving (Show, Eq)

data Options = Options
  { optToken   :: Maybe String
  , optCommand :: Command
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
    <*> commandParser

tokenOption :: Parser String
tokenOption =
  strOption
    ( long "token"
   <> metavar "TOKEN"
   <> help "WaniKani API token (overrides WANIKANI_API_TOKEN env var and config file)" )

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

commandParser :: Parser Command
commandParser =
  hsubparser
    (  command "whoami"
         (info (pure WhoAmI) (progDesc "Show current WaniKani user"))
    <> command "reviews"
         (info (pure Reviews) (progDesc "Show review schedule for the next 24 hours"))
    <> command "study"
         (info studyParser   (progDesc "Start a review batch (max N items)"))
    <> command "init"
         (info (pure Init)   (progDesc "Create or overwrite ~/.config/kroki/config interactively"))
    )
  <|> pure (Study (StudyOpts Nothing Nothing))

studyParser :: Parser Command
studyParser =
  fmap Study $
    StudyOpts
      <$> optional batchSizeOption
      <*> optional requeueAfterOption
