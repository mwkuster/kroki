module Main (main) where

import qualified Cli
import qualified Api
import qualified Config
import qualified Tui

import Control.Applicative ((<|>))
import Control.Monad (when)
import System.Environment (lookupEnv)
import System.Exit (die)

import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Maybe (fromMaybe)
import Data.Map.Strict qualified as M
import qualified Data.Text as T

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

  let verbose = Cli.optVerbose opts
      logInfo  = when verbose . putStrLn

      batchSize =
        Cli.optBatchSize opts
        <|> Config.cfgBatchSize cfg
        <|> Just 10

      rqAfter =
        fromMaybe 7 (Cli.optRequeueAfter opts <|> Config.cfgRequeueAfter cfg)

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

      mapM_
        (\(hStart, newN, openN) ->
          putStrLn
            ( padRight 20 (fmtHour hStart) <> "  "
           <> padLeft 3 (show newN) <> "  "
           <> padLeft 4 (show openN)
            )
        )
        rows

    Cli.Study -> do
      let n = fromMaybe 10 batchSize

          runBatch = do
            now <- getCurrentTime
            as  <- Api.getAvailableAssignments t now n
            if null as
              then putStrLn "No reviews available right now."
              else do
                let subjectIds = map Api.asSubjectId as
                    subjToAsg  = M.fromList [ (Api.asSubjectId a, Api.asId a) | a <- as ]
                subjects <- Api.getSubjectsByIds t subjectIds
                let asgToSubj = M.fromList
                      [ (asgId, subj)
                      | subj <- subjects
                      , Just asgId <- [M.lookup (Api.subjId subj) subjToAsg]
                      ]
                logInfo ("Batch: " <> show (length subjects) <> " items (max " <> show n <> ")")
                wantsMore <- Tui.runStudyTui rqAfter subjToAsg subjects (submitBatch asgToSubj)
                if wantsMore then runBatch else pure ()

          submitBatch asgToSubj subs =
            do
              when verbose $ do
                putStrLn "--- submissions ---"
                mapM_ (printSub asgToSubj) subs
              if not (Cli.optSubmit opts)
                then pure (Tui.SubmitResult "Run with --submit to actually commit results." False)
                else do
                  ts <- getCurrentTime
                  mapM_
                    (\s ->
                      Api.createReview t
                        (Tui.subAssignmentId s)
                        (Tui.subWrongMeaning s)
                        (Tui.subWrongReading s)
                        ts
                    )
                    subs
                  now2     <- getCurrentTime
                  summary2 <- Api.getSummary t
                  as2      <- Api.getAvailableAssignments t now2 n
                  pure Tui.SubmitResult
                    { Tui.srMessage = "Submitted. Reviews available now: "
                                   <> show (Api.reviewsAvailableNow now2 summary2)
                    , Tui.srHasMore = not (null as2)
                    }

      runBatch

printSub :: M.Map Int Api.Subject -> Tui.Submission -> IO ()
printSub asgToSubj s = do
  let name = case M.lookup (Tui.subAssignmentId s) asgToSubj of
               Just subj -> subjLabel subj
               Nothing   -> "assignment #" <> show (Tui.subAssignmentId s)
      correct = Tui.subWrongMeaning s == 0 && Tui.subWrongReading s == 0
      status
        | correct   = "correct"
        | otherwise = "incorrect"
                   <> " (meaning wrong: " <> show (Tui.subWrongMeaning s)
                   <> ", reading wrong: " <> show (Tui.subWrongReading s) <> ")"
  putStrLn (padRight 30 name <> "  " <> status)

subjLabel :: Api.Subject -> String
subjLabel subj =
  let chars = case Api.subjChars subj of
                Just c | not (T.null (T.strip c)) -> T.unpack c
                _ -> ""
      meaning = case Api.subjMeanings subj of
                  (m:_) -> T.unpack m
                  []    -> "?"
  in if null chars then meaning else chars <> " (" <> meaning <> ")"

padLeft :: Int -> String -> String
padLeft n s = replicate (max 0 (n - length s)) ' ' <> s

padRight :: Int -> String -> String
padRight n s = s <> replicate (max 0 (n - length s)) ' '
