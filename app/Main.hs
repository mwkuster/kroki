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

data QKind = QMeaning | QReading deriving (Show, Eq)

data Q = Q
  { qSubject :: Api.Subject
  , qKind    :: QKind
  } deriving (Show)

runStudySession :: [Api.Subject] -> IO ()
runStudySession subjects = do
  let queue = concatMap mkQuestions subjects
  loop queue 0 0 0
  where
    mkQuestions s =
      Q s QMeaning
      : [ Q s QReading | not (null (Api.subjReadings s)) ]

    loop :: [Q] -> Int -> Int -> Int -> IO ()
    loop [] correct wrong overridden = do
      putStrLn ""
      putStrLn ("Done. correct=" <> show correct
             <> " wrong=" <> show wrong
             <> " overridden=" <> show overridden)
    loop (q:qs) correct wrong overridden = do
      okOrAction <- askOne q
      case okOrAction of
        Right True -> loop qs (correct + 1) wrong overridden
        Right False -> loop qs correct (wrong + 1) overridden
        Left OverrideCorrect -> loop qs (correct + 1) wrong (overridden + 1)
        Left BackToQueue     -> loop (qs ++ [q]) correct wrong overridden

data WrongAction = OverrideCorrect | BackToQueue
  deriving (Show, Eq)

askOne :: Q -> IO (Either WrongAction Bool)
askOne (Q subj kind) = do
  let promptHead = "Item: " <> displayItem subj <> " " <> kindLabel kind <> ">\n"
  putStr promptHead
  ans <- getLine

  let (isOk, expected) =
        case kind of
          QMeaning ->
            let acceptedNorm = map normMeaning (Api.subjMeanings subj)
            in (normMeaning (T.pack ans) `elem` acceptedNorm, map T.unpack (Api.subjMeanings subj))
          QReading ->
            let acceptedNorm = map normReading (Api.subjReadings subj)
            in (normReading (T.pack ans) `elem` acceptedNorm, map T.unpack (Api.subjReadings subj))

  if isOk
    then do
      putStrLn "✓"
      pure (Right True)
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
    Just c | not (T.null (T.strip c)) -> T.unpack c
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

-- Reading normalization: trim + case-fold (harmless for kana; helps for romaji)
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
