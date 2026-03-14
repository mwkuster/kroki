{-# LANGUAGE OverloadedStrings #-}

module Api
  ( User(..)
  , getUser
  , Summary(..)
  , ReviewBucket(..)
  , getSummary
  , reviewsAvailableNow
  , nextReviewBucket
  , reviewsPerHourNext24
  ) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON(..), (.:), withObject)
import Data.Time (UTCTime(..), addUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)

import qualified Data.ByteString.Char8 as BS8
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T

import Network.HTTP.Req

-- A small "domain" type used by Main.hs
data User = User
  { userUsername   :: String
  , userLevel      :: Int
  , userProfileUrl :: String
  } deriving (Show, Eq)

-- WaniKani wraps the actual record in a top-level "data" field.
newtype UserEnvelope = UserEnvelope
  { ueData :: UserData
  } deriving (Show)

data UserData = UserData
  { udUsername   :: Text
  , udLevel      :: Int
  , udProfileUrl :: Text
  } deriving (Show)

instance FromJSON UserEnvelope where
  parseJSON = withObject "UserEnvelope" $ \o ->
    UserEnvelope <$> o .: "data"

instance FromJSON UserData where
  parseJSON = withObject "UserData" $ \o ->
    UserData
      <$> o .: "username"
      <*> o .: "level"
      <*> o .: "profile_url"

-- Summary endpoint types
data Summary = Summary
  { summaryReviews :: [ReviewBucket]
  } deriving (Show, Eq)

data ReviewBucket = ReviewBucket
  { rbAvailableAt :: UTCTime
  , rbSubjectIds  :: [Int]
  } deriving (Show, Eq)

newtype SummaryEnvelope = SummaryEnvelope
  { seData :: Summary
  } deriving (Show)

instance FromJSON SummaryEnvelope where
  parseJSON = withObject "SummaryEnvelope" $ \o ->
    SummaryEnvelope <$> o .: "data"

instance FromJSON Summary where
  parseJSON = withObject "Summary" $ \o ->
    Summary <$> o .: "reviews"

instance FromJSON ReviewBucket where
  parseJSON = withObject "ReviewBucket" $ \o -> do
    -- available_at is an ISO8601 timestamp string in the API
    t <- o .: "available_at"
    at <- maybe (fail "invalid available_at") pure (iso8601ParseM t)
    ReviewBucket at <$> o .: "subject_ids"

data KrokiError
  = ApiDecodeError Text
  deriving (Show)

instance Exception KrokiError

-- API calls
getUser :: String -> IO User
getUser token = runReq defaultHttpConfig $ do
  let authHeader = header "Authorization" ("Bearer " <> BS8.pack token)
      revHeader  = header "Wanikani-Revision" "20170710"

  resp <- req
    GET
    (https "api.wanikani.com" /: "v2" /: "user")
    NoReqBody
    jsonResponse
    (authHeader <> revHeader)

  let env = responseBody resp :: UserEnvelope
      u   = ueData env

  pure User
    { userUsername   = T.unpack (udUsername u)
    , userLevel      = udLevel u
    , userProfileUrl = T.unpack (udProfileUrl u)
    }

getSummary :: String -> IO Summary
getSummary token = runReq defaultHttpConfig $ do
  let authHeader = header "Authorization" ("Bearer " <> BS8.pack token)
      revHeader  = header "Wanikani-Revision" "20170710"

  resp <- req
    GET
    (https "api.wanikani.com" /: "v2" /: "summary")
    NoReqBody
    jsonResponse
    (authHeader <> revHeader)

  let env = responseBody resp :: SummaryEnvelope
  pure (seData env)

-- Review helpers
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

-- Projection semantics:
-- Row 0 (timestamp = now): new == open == number currently available.
-- Rows 1..23: for each next hour window, "new" is what becomes available
-- in that hour; "open" is cumulative from now (assuming you do no reviews).
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
