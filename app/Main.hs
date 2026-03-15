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
      putStrLn ("Assignment subject_ids: " <> show (map Api.asSubjectId as))
      if null as
        then putStrLn "No reviews available right now."
        else do
          let subjectIds = map Api.asSubjectId as
          subjects <- Api.getSubjectsByIds t subjectIds

          putStrLn ("Batch: " <> show (length subjects) <> " items (max " <> show n <> ")")
          runStudySession subjects

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

-- Track per subject whether meaning/reading are done correctly.
data Progress = Progress
  { pMeaningOk     :: Bool
  , pReadingNeeded :: Bool
  , pReadingOk     :: Bool
  } deriving (Show, Eq)

-- How far to postpone a requeued question.
requeueAfterK :: Int
requeueAfterK = 7

runStudySession :: [Api.Subject] -> IO ()
runStudySession subjects = do
  let queue    = concatMap mkQuestions subjects
      progress = M.fromList [ (Api.subjId s, initProgress s) | s <- subjects ]
  loop queue progress 0 0 0
  where
    -- only ask reading if there are accepted readings and it's not a radical
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
      in Progress False needsReading False

    acceptedReadings s =
      filter (not . T.null . T.strip) (Api.subjReadings s)

    loop :: [Q] -> M.Map Int Progress -> Int -> Int -> Int -> IO ()
    loop [] prog correct wrong overridden = do
      let fullyCorrect =
            length
              [ ()
              | (_, p) <- M.toList prog
              , pMeaningOk p
              , (not (pReadingNeeded p) || pReadingOk p)
              ]
          totalItems = M.size prog
      putStrLn ""
      putStrLn ("Done. correct=" <> show correct
             <> " wrong=" <> show wrong
             <> " overridden=" <> show overridden)
      putStrLn ("Fully correct items: " <> show fullyCorrect <> " / " <> show totalItems)

    loop (q:qs) prog correct wrong overridden = do
      res <- askOne q
      case res of
        Right True -> do
          let prog' = markOk (qSubject q) (qKind q) prog
          loop qs prog' (correct + 1) wrong overridden

        Right False ->
          loop qs prog correct (wrong + 1) overridden

        Left OverrideCorrect -> do
          let prog' = markOk (qSubject q) (qKind q) prog
          loop qs prog' (correct + 1) wrong (overridden + 1)

        Left BackToQueue ->
          loop (requeueAfter requeueAfterK q qs) prog correct wrong overridden

markOk :: Api.Subject -> QKind -> M.Map Int Progress -> M.Map Int Progress
markOk subj kind mp =
  let sid = Api.subjId subj
  in M.adjust (upd kind) sid mp
  where
    upd QMeaning p = p { pMeaningOk = True }
    upd QReading p = p { pReadingOk = True }

-- Insert the question k positions later (or at end if queue shorter).
requeueAfter :: Int -> Q -> [Q] -> [Q]
requeueAfter k q qs =
  let k' = max 0 k
      (front, back) = splitAt k' qs
  in front ++ [q] ++ back

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
      putStrLn "   [o]=override as correct  [b]=back to queue  [Enter]=keep wrong"
      putStr "   > "
      choice <- getLine
      case map toLower (trim choice) of
        "o" -> pure (Left OverrideCorrect)
        "b" -> pure (Left BackToQueue)
        _   -> pure (Right False)

displayItem :: Api.Subject -> String
displayItem s =
  case Api.subjChars s of
    Just c | not (T.null (T.strip c)) -> T.unpack (T.strip c)
    _ ->
      let m = case Api.subjMeanings s of
                (x:_) -> T.unpack x
                []    -> "?"
      in show (Api.subjType s) <> ":" <> m <> " (#" <> show (Api.subjId s) <> ")"

kindLabel :: QKind -> String
kindLabel QMeaning = "meaning"
kindLabel QReading = "reading"

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
