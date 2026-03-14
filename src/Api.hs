{-# LANGUAGE OverloadedStrings #-}

module Api
  ( User(..)
  , getUser
  , Summary(..)
  , ReviewBucket(..)
  , getSummary
  , reviewsAvailableNow
  , nextReviewBucket
  ) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON(..), (.:), withObject)
-- import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.List (sortOn)

-- A small "domain" type used by Main.hs
data User = User
  { userUsername   :: String
  , userLevel      :: Int
  , userProfileUrl :: String
  } deriving (Show, Eq)

-- WaniKani wraps the actual user record in a top-level "data" field.
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

data Summary = Summary
  { summaryReviews :: [ReviewBucket]
  } deriving (Show)

data ReviewBucket = ReviewBucket
  { rbAvailableAt :: UTCTime
  , rbSubjectIds  :: [Int]
  } deriving (Show)

newtype SummaryEnvelope = SummaryEnvelope { seData :: Summary } deriving (Show)

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

reviewsAvailableNow :: UTCTime -> Summary -> Int
reviewsAvailableNow now s =
    sum [ length (rbSubjectIds b) | b <- summaryReviews s, rbAvailableAt b <= now ]

nextReviewBucket :: UTCTime -> Summary -> Maybe (UTCTime, Int)
nextReviewBucket now s =
  case sortOn rbAvailableAt [ b | b <- summaryReviews s, rbAvailableAt b > now, not (null (rbSubjectIds b)) ] of
    (b:_) -> Just (rbAvailableAt b, length (rbSubjectIds b))
    []    -> Nothing
