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

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Char (toLower, isSpace)
import qualified Data.Map.Strict as M

import System.Random (randomRIO)

import qualified Romaji


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

      mapM_ (\(hStart, newN, openN) ->
            putStrLn (padRight 20 (fmtHour hStart) <> "  "
                   <> padLeft 3 (show newN) <> "  "
                   <> padLeft 4 (show openN)))
           rows

    Cli.Study -> do
      let n = fromMaybe 10 batchSize
      now <- getCurrentTime
      as <- Api.getAvailableAssignments t now n
      if null as
        then putStrLn "No reviews available right now."
        else do
          let subjectIds = map Api.asSubjectId as
              subjToAsg  = M.fromList [ (Api.asSubjectId a, Api.asId a) | a <- as ]

          subjects <- Api.getSubjectsByIds t subjectIds
          putStrLn ("Batch: " <> show (length subjects) <> " items (max " <> show n <> ")")

          subs <- runStudySession rqAfter subjToAsg subjects

          -- (optional) print summary, and if you added --submit, commit here
          putStrLn ("Completed (fully correct) items to submit: " <> show (length subs))

          if Cli.optSubmit opts
            then
               if null subs
               then putStrLn "Nothing to submit (no fully-correct items)."
               else do
                  putStrLn ("Ready to submit " <> show (length subs) <> " reviews to WaniKani. Submit now? [y/N]")
                  putStr "> "
                  yn <- getLine
                  case map toLower (trim yn) of
                    "y" -> do
                      ts <- getCurrentTime
                      mapM_
                         (\s -> Api.createReview t
                                 (subAssignmentId s)
                                 (subWrongMeaning s)
                                 (subWrongReading s)
                                 ts)
                         subs
                      putStrLn "Submitted."

                      now2 <- getCurrentTime
                      summary2 <- Api.getSummary t
                      putStrLn ("Reviews available now (after submit): " <> show (Api.reviewsAvailableNow now2 summary2))

                    _ -> putStrLn "Not submitted."
          else
             putStrLn "Tip: run with --submit to commit these results to WaniKani."


padLeft :: Int -> String -> String
padLeft n s = replicate (max 0 (n - length s)) ' ' <> s

padRight :: Int -> String -> String
padRight n s = s <> replicate (max 0 (n - length s)) ' '

--------------------------------------------------------------------------------
-- Study session
--------------------------------------------------------------------------------

data QKind = QMeaning | QReading deriving (Show, Eq, Ord)

data Q = Q
  { qSubject :: Api.Subject
  , qKind    :: QKind
  } deriving (Show)

data WrongAction = OverrideCorrect | BackToQueue
  deriving (Show, Eq)

data Submission = Submission
  { subAssignmentId :: Int
  , subWrongMeaning :: Int
  , subWrongReading :: Int
  } deriving (Show, Eq)

-- Track per subject whether meaning/reading are done correctly + wrong counts.
data Progress = Progress
  { pMeaningOk     :: Bool
  , pReadingNeeded :: Bool
  , pReadingOk     :: Bool
  , pMeaningWrong  :: Int
  , pReadingWrong  :: Int
  } deriving (Show, Eq)

-- Insert the question k positions later (or at end if queue shorter).
requeueAfter :: Int -> Q -> [Q] -> [Q]
requeueAfter k q qs =
  let k' = max 0 k
      (front, back) = splitAt k' qs
  in front ++ [q] ++ back

-- Simple shuffle (Fisher–Yates-ish by repeated random extraction)
-- Needs: import System.Random (randomRIO)
shuffle :: [a] -> IO [a]
shuffle xs = go xs []
  where
    go [] acc = pure acc
    go ys acc = do
      i <- randomRIO (0, length ys - 1)
      let (front, a:back) = splitAt i ys
      go (front ++ back) (a : acc)

-- Study session: returns submissions for fully-correct items.
-- rqAfter: how far to postpone requeued questions
-- subjToAsg: map subject_id -> assignment_id
runStudySession :: Int -> M.Map Int Int -> [Api.Subject] -> IO [Submission]
runStudySession rqAfter subjToAsg subjects = do
  let queue0    = concatMap mkQuestions subjects
  queue <- shuffle queue0

  let progress0 = M.fromList [ (Api.subjId s, initProgress s) | s <- subjects ]

  loop queue progress0 0 0 0
  where
    acceptedReadings s =
      filter (not . T.null . T.strip) (Api.subjReadings s)

    mkQuestions s =
      let rs = acceptedReadings s
      in Q s QMeaning
         : [ Q s QReading
           | Api.subjType s /= Api.Radical
           , not (null rs)
           ]

    initProgress s =
      let needsReading =
            Api.subjType s /= Api.Radical
            && not (null (acceptedReadings s))
      in Progress False needsReading False 0 0

    markOk :: Api.Subject -> QKind -> M.Map Int Progress -> M.Map Int Progress
    markOk subj kind mp =
      let sid = Api.subjId subj
      in M.adjust (upd kind) sid mp
      where
        upd QMeaning p = p { pMeaningOk = True }
        upd QReading p = p { pReadingOk = True }

    incWrong :: Api.Subject -> QKind -> M.Map Int Progress -> M.Map Int Progress
    incWrong subj kind mp =
      let sid = Api.subjId subj
      in M.adjust (upd kind) sid mp
      where
        upd QMeaning p = p { pMeaningWrong = pMeaningWrong p + 1 }
        upd QReading p = p { pReadingWrong = pReadingWrong p + 1 }

    loop :: [Q] -> M.Map Int Progress -> Int -> Int -> Int -> IO [Submission]
    loop [] prog correct wrong overridden = do
      let fullyCorrect =
            [ (sid, p)
            | (sid, p) <- M.toList prog
            , pMeaningOk p
            , (not (pReadingNeeded p) || pReadingOk p)
            , pMeaningWrong p == 0
            , pReadingWrong p == 0
            ]
          totalItems   = M.size prog
          fullyCorrectN = length fullyCorrect

          submissions =
            [ Submission
                { subAssignmentId = asgId
                , subWrongMeaning = pMeaningWrong p
                , subWrongReading = pReadingWrong p
                }
            | (sid, p) <- fullyCorrect
            , Just asgId <- [M.lookup sid subjToAsg]
            ]

      putStrLn ""
      putStrLn ("Done. correct=" <> show correct
             <> " wrong=" <> show wrong
             <> " overridden=" <> show overridden)
      putStrLn ("Fully correct items: " <> show fullyCorrectN <> " / " <> show totalItems)
      pure submissions

    loop (q:qs) prog correct wrong overridden = do
      res <- askOne q
      case res of
        Right True -> do
          let prog' = markOk (qSubject q) (qKind q) prog
          loop qs prog' (correct + 1) wrong overridden
        Right False -> do
          let prog' = incWrong (qSubject q) (qKind q) prog
          loop (requeueAfter rqAfter q qs) prog' correct (wrong + 1) overridden
        Right False -> do
          -- wrong answer, not overridden, not requeued => count as wrong attempt
          let prog' = incWrong (qSubject q) (qKind q) prog
          loop qs prog' correct (wrong + 1) overridden

        Left OverrideCorrect -> do
          -- treated as correct, DO NOT increment wrong
          let prog' = markOk (qSubject q) (qKind q) prog
          loop qs prog' (correct + 1) wrong (overridden + 1)

        Left BackToQueue -> do
          -- wrong attempt + requeue this exact question after rqAfter
          let prog' = incWrong (qSubject q) (qKind q) prog
          loop (requeueAfter rqAfter q qs) prog' correct (wrong + 1) overridden

askOne :: Q -> IO (Either WrongAction Bool)
askOne (Q subj kind) = do
  let promptHead = displayItem subj <> " " <> kindLabel kind <> ">\n "
  putStr promptHead
  ans <- getLine

  let (isOk, expected) =
        case kind of
          QMeaning ->
            let acceptedNorm = map normMeaning (Api.subjMeanings subj)
            in ( normMeaning (T.pack ans) `elem` acceptedNorm
               , map T.unpack (Api.subjMeanings subj)
               )
          QReading ->
            let rs = filter (not . T.null . T.strip) (Api.subjReadings subj)
                acceptedNorm = map normReading rs
            in ( normReading (T.pack ans) `elem` acceptedNorm
               , map T.unpack rs
               )

  if isOk
    then putStrLn "✓" >> pure (Right True)
    else do
      putStrLn ("✗  (accepted: " <> intercalate ", " expected <> ")")
      putStrLn "   [o]=override as correct  [b]=requeue later  [Enter]=requeue"
      putStr "   > "
      choice <- getLine
      let c = map toLower (trim choice)
      if c == "o"
        then pure (Left OverrideCorrect)
        else pure (Left BackToQueue)

kindLabel :: QKind -> String
kindLabel QMeaning = "meaning"
kindLabel QReading = "reading"

displayItem :: Api.Subject -> String
displayItem s =
  case Api.subjChars s of
    Just c | not (T.null (T.strip c)) -> T.unpack (T.strip c)
    _ ->
      let m = case Api.subjMeanings s of
                (x:_) -> T.unpack x
                []    -> "?"
      in show (Api.subjType s) <> ":" <> m <> " (#" <> show (Api.subjId s) <> ")"

-- Meaning normalization: case-insensitive, trim, collapse spaces
normMeaning :: Text -> Text
normMeaning = collapseSpaces . T.toCaseFold . T.strip

-- Reading normalization: accept romaji or kana
normReading :: Text -> Text
normReading t =
  let t' = T.strip t
  in if T.all (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '\'') t'
       then Romaji.romajiToHiragana (T.toCaseFold t')
       else T.toCaseFold t'

collapseSpaces :: Text -> Text
collapseSpaces =
  T.unwords . filter (not . T.null) . T.words

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
