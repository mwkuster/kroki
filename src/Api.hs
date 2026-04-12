{-# LANGUAGE OverloadedStrings #-}

module Api
  ( User(..)
  , UserEnvelope(..)
  , getUser

  , Summary(..)
  , ReviewBucket(..)
  , getSummary
  , reviewsAvailableNow
  , nextReviewBucket
  , reviewsPerHourNext24

  , SrsStage(..)
  , srsStageLabel
  , Assignment(..)
  , nextSrsStage
  , getAvailableAssignments

  , SubjectType(..)
  , Subject(..)
  , getSubjectsByIds
  , createReview
  ) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON(..), (.:), (.:?), Object, withObject)
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as Key
import Data.Aeson (object, (.=))
import Data.List (sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime(..), addUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)

import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Req

--------------------------------------------------------------------------------
-- Shared API options
--------------------------------------------------------------------------------

-- | Standard auth + revision headers, shared by all API calls.
apiOpts :: String -> Option scheme
apiOpts token =
  header "Authorization" ("Bearer " <> BS8.pack token)
  <> header "Wanikani-Revision" "20170710"

--------------------------------------------------------------------------------
-- User
--------------------------------------------------------------------------------

data User = User
  { userUsername   :: Text
  , userLevel      :: Int
  , userProfileUrl :: Text
  } deriving (Show, Eq)

instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User <$> o .: "username" <*> o .: "level" <*> o .: "profile_url"

newtype UserEnvelope = UserEnvelope { ueData :: User } deriving (Show, Eq)

instance FromJSON UserEnvelope where
  parseJSON = withObject "UserEnvelope" $ \o ->
    UserEnvelope <$> o .: "data"

data KrokiError
  = ApiDecodeError Text
  deriving (Show)

instance Exception KrokiError

getUser :: String -> IO User
getUser token = runReq defaultHttpConfig $ do
  resp <- req
    GET
    (https "api.wanikani.com" /: "v2" /: "user")
    NoReqBody
    jsonResponse
    (apiOpts token)
  pure (ueData (responseBody resp :: UserEnvelope))

--------------------------------------------------------------------------------
-- Summary (reviews timeline)
--------------------------------------------------------------------------------

data Summary = Summary
  { summaryReviews :: [ReviewBucket]
  } deriving (Show, Eq)

data ReviewBucket = ReviewBucket
  { rbAvailableAt :: UTCTime
  , rbSubjectIds  :: [Int]
  } deriving (Show, Eq)

newtype SummaryEnvelope = SummaryEnvelope { seData :: Summary } deriving (Show)

instance FromJSON SummaryEnvelope where
  parseJSON = withObject "SummaryEnvelope" $ \o ->
    SummaryEnvelope <$> o .: "data"

instance FromJSON Summary where
  parseJSON = withObject "Summary" $ \o ->
    Summary <$> o .: "reviews"

instance FromJSON ReviewBucket where
  parseJSON = withObject "ReviewBucket" $ \o -> do
    t <- o .: "available_at"
    at <- maybe (fail "invalid available_at") pure (iso8601ParseM t)
    ReviewBucket at <$> o .: "subject_ids"

getSummary :: String -> IO Summary
getSummary token = runReq defaultHttpConfig $ do
  resp <- req
    GET
    (https "api.wanikani.com" /: "v2" /: "summary")
    NoReqBody
    jsonResponse
    (apiOpts token)
  let env = responseBody resp :: SummaryEnvelope
  pure (seData env)

reviewsAvailableNow :: UTCTime -> Summary -> Int
reviewsAvailableNow now s =
  sum [ length (rbSubjectIds b)
      | b <- summaryReviews s
      , rbAvailableAt b <= now
      ]

nextReviewBucket :: UTCTime -> Summary -> Maybe (UTCTime, Int)
nextReviewBucket now s =
  case sortOn rbAvailableAt
        [ b | b <- summaryReviews s
            , rbAvailableAt b > now
            , not (null (rbSubjectIds b))
        ] of
    (b:_) -> Just (rbAvailableAt b, length (rbSubjectIds b))
    []    -> Nothing

openAt :: UTCTime -> Summary -> Int
openAt t s =
  sum [ length (rbSubjectIds b)
      | b <- summaryReviews s
      , rbAvailableAt b <= t
      ]

newInWindow :: UTCTime -> UTCTime -> Summary -> Int
newInWindow start end s =
  sum [ length (rbSubjectIds b)
      | b <- summaryReviews s
      , rbAvailableAt b >  start
      , rbAvailableAt b <= end
      ]

-- Row 0: (now, openNow, openNow). Rows 1..: per hour new + cumulative open from now.
reviewsPerHourNext24 :: UTCTime -> Summary -> [(UTCTime, Int, Int)]
reviewsPerHourNext24 now s =
  let openNow = openAt now s

      mk :: Int -> (UTCTime, Int, Int)
      mk 0 = (now, openNow, openNow)
      mk i =
        let start = addUTCTime (fromIntegral ((i - 1) * 3600)) now
            end   = addUTCTime 3600 start
            newN  = newInWindow start end s
            openN = openNow + sum
                      [ newInWindow (addUTCTime (fromIntegral ((j - 1) * 3600)) now)
                                    (addUTCTime (fromIntegral (j * 3600)) now)
                                    s
                      | j <- [1..i]
                      ]
        in (start, newN, openN)

  in map mk ([0..23] :: [Int])

--------------------------------------------------------------------------------
-- Assignments (to get what's available now)
--------------------------------------------------------------------------------

data SrsStage = Initiate | Apprentice | Guru | Master | Enlightened | Burned
  deriving (Show, Eq)

srsStageLabel :: SrsStage -> String
srsStageLabel Initiate   = "Initiate"
srsStageLabel Apprentice = "Apprentice"
srsStageLabel Guru       = "Guru"
srsStageLabel Master     = "Master"
srsStageLabel Enlightened = "Enlightened"
srsStageLabel Burned     = "Burned"

srsStageFromInt :: Int -> SrsStage
srsStageFromInt 0         = Initiate
srsStageFromInt n | n <= 4 = Apprentice
srsStageFromInt n | n <= 6 = Guru
srsStageFromInt 7         = Master
srsStageFromInt 8         = Enlightened
srsStageFromInt _         = Burned

data Assignment = Assignment
  { asId           :: Int
  , asSubjectId    :: Int
  , asSrsStage     :: SrsStage
  , asSrsStageNum  :: Int
  } deriving (Show, Eq)

-- | Compute the SRS stage category after a review.
-- wrongTotal is the sum of wrong meaning and wrong reading counts.
-- Correct (wrongTotal == 0): advance by 1. Incorrect: penalise by
-- ceil(wrongTotal / 2), minimum 1 stage drop, floor at Apprentice I (1).
nextSrsStage :: Assignment -> Int -> SrsStage
nextSrsStage asg wrongTotal =
  let cur = asSrsStageNum asg
      next | wrongTotal == 0 = min 9 (cur + 1)
           | otherwise       = max 1 (cur - max 1 ((wrongTotal + 1) `div` 2))
  in srsStageFromInt next

newtype AssignmentsEnvelope = AssignmentsEnvelope { aeData :: [AssignmentData] } deriving (Show)

data AssignmentData = AssignmentData
  { adId        :: Int
  , adSubject   :: Int
  , adSrsStage  :: Int
  } deriving (Show)

instance FromJSON AssignmentsEnvelope where
  parseJSON = withObject "AssignmentsEnvelope" $ \o ->
    AssignmentsEnvelope <$> o .: "data"

instance FromJSON AssignmentData where
  parseJSON = withObject "AssignmentData" $ \o -> do
    i     <- o .: "id"
    d     <- o .: "data"
    s     <- d .: "subject_id"
    stage <- d .: "srs_stage"
    pure (AssignmentData i s stage)

toAssignment :: AssignmentData -> Assignment
toAssignment (AssignmentData i s stage) =
  Assignment i s (srsStageFromInt stage) stage

getAvailableAssignments :: String -> UTCTime -> Int -> IO [Assignment]
getAvailableAssignments token now n = runReq defaultHttpConfig $ do
  let nowParam = T.pack (iso8601Show now)

  resp <- req
    GET
    (https "api.wanikani.com" /: "v2" /: "assignments")
    NoReqBody
    jsonResponse
    ( "available_before" =: nowParam
   <> "in_review"        =: True
   <> "hidden"           =: False
   <> apiOpts token )

  let env = responseBody resp :: AssignmentsEnvelope
      as  = map toAssignment (aeData env)
  pure (take n as)

--------------------------------------------------------------------------------
-- Subjects (to show prompts + accepted answers)
--------------------------------------------------------------------------------

data SubjectType = Radical | Kanji | Vocabulary | KanaVocabulary
  deriving (Show, Eq)

data Subject = Subject
  { subjId               :: Int
  , subjType             :: SubjectType
  , subjLevel            :: Int
  , subjChars            :: Maybe Text
  , subjMeanings         :: [Text]       -- accepted meanings
  , subjReadings         :: [Text]       -- accepted readings (kana/romaji depending on type)
  , subjAudioUrls        :: [Text]       -- pronunciation audio URLs (vocab only)
  , subjMeaningMnemonic  :: Maybe Text
  , subjReadingMnemonic  :: Maybe Text
  , subjComponentIds     :: [Int]        -- radicals for kanji; kanji for vocab
  } deriving (Show, Eq)

newtype SubjectsEnvelope = SubjectsEnvelope { suData :: [Subject] } deriving (Show)

instance FromJSON SubjectsEnvelope where
  parseJSON = withObject "SubjectsEnvelope" $ \o ->
    SubjectsEnvelope <$> o .: "data"

newtype PronAudio = PronAudio { paUrl :: Text }

instance FromJSON PronAudio where
  parseJSON = withObject "PronAudio" $ \o -> PronAudio <$> o .: "url"

instance FromJSON Subject where
  parseJSON = withObject "Subject" $ \o -> do
    sid <- o .: "id"
    obj <- o .: "object"
    st  <- parseSubjectType obj
    d   <- o .: "data"

    lvl   <- d .:  "level"
    chars <- d .:? "characters"

    meanings <- d .: "meanings" >>= parseAccepted "meaning"
    readings <- case st of
      Radical -> pure []
      _       -> d .:? "readings" >>= maybe (pure []) (parseAccepted "reading")

    let fetchAudio = maybe [] (map paUrl) <$> (d .:? "pronunciation_audios")
    audioUrls <- case st of
      Vocabulary     -> fetchAudio
      KanaVocabulary -> fetchAudio
      _              -> pure []

    mmnem   <- d .:? "meaning_mnemonic"
    rmnem   <- case st of
      Radical -> pure Nothing
      _       -> d .:? "reading_mnemonic"
    compIds <- case st of
      Radical -> pure []
      _       -> fromMaybe [] <$> (d .:? "component_subject_ids")

    pure Subject
      { subjId              = sid
      , subjType            = st
      , subjLevel           = lvl
      , subjChars           = chars
      , subjMeanings        = meanings
      , subjReadings        = readings
      , subjAudioUrls       = audioUrls
      , subjMeaningMnemonic = mmnem
      , subjReadingMnemonic = rmnem
      , subjComponentIds    = compIds
      }

parseSubjectType :: Text -> Parser SubjectType
parseSubjectType t =
  case t of
    "radical"         -> pure Radical
    "kanji"           -> pure Kanji
    "vocabulary"      -> pure Vocabulary
    "kana_vocabulary" -> pure KanaVocabulary
    _                 -> fail ("Unknown subject type: " <> T.unpack t)

-- Parse accepted answers from a list of objects like:
-- { "meaning": "...", "accepted_answer": true, ... }
parseAccepted :: Text -> [AesonObj] -> Parser [Text]
parseAccepted field xs =
  fmap catMaybes $ mapM (acceptedFrom field) xs

type AesonObj = Data.Aeson.Object

acceptedFrom :: Text -> AesonObj -> Parser (Maybe Text)
acceptedFrom field o = do
  acc <- o .: "accepted_answer"
  if acc
    then Just <$> o .: Key.fromText field
    else pure Nothing


-- Fetch subjects by IDs; chunk to avoid huge URLs.
getSubjectsByIds :: String -> [Int] -> IO [Subject]
getSubjectsByIds token ids = do
  let chunks = chunkN 100 ids
  fmap concat $ mapM (getChunk token) chunks

getChunk :: String -> [Int] -> IO [Subject]
getChunk token idsChunk = runReq defaultHttpConfig $ do
  let idsParam = T.intercalate "," (map (T.pack . show) idsChunk)

  resp <- req
    GET
    (https "api.wanikani.com" /: "v2" /: "subjects")
    NoReqBody
    jsonResponse
    ( "ids" =: idsParam <> apiOpts token )

  let env = responseBody resp :: SubjectsEnvelope
  pure (suData env)

chunkN :: Int -> [a] -> [[a]]
chunkN n0 = go
  where
    n = max 1 n0
    go [] = []
    go xs =
      let (a, b) = splitAt n xs
      in a : go b

createReview :: String -> Int -> Int -> Int -> UTCTime -> IO ()
createReview token assignmentId wrongMeaning wrongReading createdAt =
  runReq defaultHttpConfig $ do
    let body =
          object
            [ "review" .= object
                [ "assignment_id"             .= assignmentId
                , "incorrect_meaning_answers" .= wrongMeaning
                , "incorrect_reading_answers" .= wrongReading
                , "created_at"                .= iso8601Show createdAt
                ]
            ]

    _ <- req
      POST
      (https "api.wanikani.com" /: "v2" /: "reviews")
      (ReqBodyJson body)
      ignoreResponse
      (apiOpts token)

    pure ()
