module Main (main) where

import qualified Cli
import qualified Api
import qualified Config
import qualified Tui
import Util (strPadLeft, strPadRight)

import Control.Applicative ((<|>))
import System.Environment (lookupEnv)
import System.Exit (die)

import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.List (nub)
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

  let requireToken =
        maybe
          (die "Missing API token. Provide --token, set WANIKANI_API_TOKEN, or put token=... into ~/.config/kroki/config")
          pure
          token

  case Cli.optCommand opts of
    Cli.Init -> Config.initConfig

    Cli.WhoAmI -> do
      t <- requireToken
      user <- Api.getUser t
      putStrLn ("Username: " <> T.unpack (Api.userUsername user))
      putStrLn ("Level:    " <> show (Api.userLevel user))
      putStrLn ("Profile:  " <> T.unpack (Api.userProfileUrl user))

    Cli.Reviews -> do
      t <- requireToken
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
            ( strPadRight 20 (fmtHour hStart) <> "  "
           <> strPadLeft 3 (show newN) <> "  "
           <> strPadLeft 4 (show openN)
            )
        )
        rows

    Cli.Study studyOpts -> do
      t <- requireToken
      let batchSize =
            Cli.studyBatchSize studyOpts
            <|> Config.cfgBatchSize cfg
            <|> Just Config.defaultBatchSize
          rqAfter =
            fromMaybe Config.defaultRequeueAfter
              (Cli.studyRequeueAfter studyOpts <|> Config.cfgRequeueAfter cfg)
          raw = fromMaybe 10 batchSize
          n   = if raw == 0 then maxBound else raw
      now  <- getCurrentTime
      tz   <- getCurrentTimeZone
      user <- Api.getUser t
      summary <- Api.getSummary t

      let runBatch = do
            now2 <- getCurrentTime
            as   <- Api.getAvailableAssignments t now2 n
            if null as
              then putStrLn "No reviews available right now."
              else do
                let subjectIds = map Api.asSubjectId as
                    subjToAsg  = M.fromList [ (Api.asSubjectId a, Api.asId a) | a <- as ]
                subjects <- Api.getSubjectsByIds t subjectIds
                let compIds = nub [ cid | s <- subjects, cid <- Api.subjComponentIds s ]
                compSubjects <- Api.getSubjectsByIds t compIds
                let allSubjMap = M.fromList [ (Api.subjId s, s) | s <- subjects ++ compSubjects ]
                    asgToSubj  = M.fromList
                      [ (asgId, subj)
                      | subj <- subjects
                      , Just asgId <- [M.lookup (Api.subjId subj) subjToAsg]
                      ]
                let audioPlayer = Config.cfgAudioPlayer cfg
                wantsMore <- Tui.runStudyTui rqAfter audioPlayer user summary now tz allSubjMap subjToAsg subjects (submitBatch asgToSubj)
                if wantsMore then runBatch else pure ()

          submitBatch asgToSubj subs = do
            let details = map (fmtSub asgToSubj) subs
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
              , Tui.srDetails = details
              }

      runBatch

fmtSub :: M.Map Int Api.Subject -> Tui.Submission -> String
fmtSub asgToSubj s =
  let name = case M.lookup (Tui.subAssignmentId s) asgToSubj of
               Just subj -> subjLabel subj
               Nothing   -> "assignment #" <> show (Tui.subAssignmentId s)
      correct = Tui.subWrongMeaning s == 0 && Tui.subWrongReading s == 0
      status
        | correct   = "correct"
        | otherwise = "incorrect"
                   <> " (meaning wrong: " <> show (Tui.subWrongMeaning s)
                   <> ", reading wrong: " <> show (Tui.subWrongReading s) <> ")"
  in strPadRight 30 name <> "  " <> status

subjLabel :: Api.Subject -> String
subjLabel subj =
  let chars = case Api.subjChars subj of
                Just c | not (T.null (T.strip c)) -> T.unpack c
                _ -> ""
      meaning = case Api.subjMeanings subj of
                  (m:_) -> T.unpack m
                  []    -> "?"
  in if null chars then meaning else chars <> " (" <> meaning <> ")"

