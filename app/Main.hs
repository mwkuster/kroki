module Main (main) where

import qualified Cli
import qualified Api
import qualified Config
import Control.Applicative ((<|>))

import System.Environment (lookupEnv)
import System.Exit (die)

import Data.Time (getCurrentTime)

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

  case Cli.optCommand opts of
    Cli.WhoAmI -> do
      user <- Api.getUser t
      putStrLn ("Username: " <> Api.userUsername user)
      putStrLn ("Level:    " <> show (Api.userLevel user))
      putStrLn ("Profile:  " <> Api.userProfileUrl user)
    Cli.Reviews -> do
      now <- getCurrentTime
      summary <- Api.getSummary t
      let n = Api.reviewsAvailableNow now summary
      putStrLn ("Reviews available now: " <> show n)
      case Api.nextReviewBucket now summary of
        Just (at, cnt) -> putStrLn ("Next reviews: " <> show cnt <> " at " <> show at)
        Nothing        -> putStrLn "No upcoming reviews found."
