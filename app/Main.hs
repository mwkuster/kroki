module Main (main) where

import qualified Cli
import qualified Api
import qualified Config
import Control.Applicative ((<|>))

import System.Environment (lookupEnv)
import System.Exit (die)

import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  opts <- Cli.parseCli
  cfg  <- Config.loadConfig

  envToken <- lookupEnv "WANIKANI_API_TOKEN"

  let token =
        Cli.optToken opts
        <|> envToken
        <|> Config.cfgToken cfg

  t <- maybe
        (die "Missing API token. Provide --token, set WANIKANI_API_TOKEN, or put token=... into ~/.config/kroki/config")
        pure
        token

  let batchSize =
        Cli.optBatchSize opts
        <|> Config.cfgBatchSize cfg
        <|> Just 10

  case Cli.optCommand opts of
    Cli.WhoAmI -> do
      user <- Api.getUser t
      putStrLn ("Username: " <> Api.userUsername user)
      putStrLn ("Level:    " <> show (Api.userLevel user))
      putStrLn ("Profile:  " <> Api.userProfileUrl user)


    Cli.Reviews -> do
      now <- getCurrentTime
      tz  <- getCurrentTimeZone
      summary <- Api.getSummary t

      putStrLn "Hour (local)           New  Open"
      putStrLn "---------------------------------"

      let rows = Api.reviewsPerHourNext24 now summary
          fmtHour utc =
            let lt = utcToLocalTime tz utc
            in formatTime defaultTimeLocale "%F %H:00" lt

      mapM_ (\(hStart, newN, openN) ->
            putStrLn (padRight 20 (fmtHour hStart) <> "  "
                   <> padLeft 3 (show newN) <> "  "
                   <> padLeft 4 (show openN)))
           rows

    Cli.Study -> do
      now <- getCurrentTime
      let n = fromMaybe 10 batchSize
      as <- Api.getAvailableAssignments t n
      putStrLn ("Batch size: " <> show (length as) <> " (max " <> show n <> ")")
      mapM_ (putStrLn . ("Subject: " <>) . show . Api.asSubjectId) as

padLeft :: Int -> String -> String
padLeft n s = replicate (max 0 (n - length s)) ' ' <> s

padRight :: Int -> String -> String
padRight n s = s <> replicate (max 0 (n - length s)) ' '
