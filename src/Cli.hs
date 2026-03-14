module Cli
  ( Options(..)
  , Command(..)
  , parseCli
  ) where

import Options.Applicative

data Command
  = WhoAmI
  | Reviews
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
   <> help "WaniKani API token (otherwise read WANIKANI_API_TOKEN)" )

commandParser :: Parser Command
commandParser =
  hsubparser $
       command "whoami"
         (info (pure WhoAmI) (progDesc "Show current WaniKani user"))
    <> command "reviews"
         (info (pure Reviews) (progDesc "Show number of reviews available now"))
