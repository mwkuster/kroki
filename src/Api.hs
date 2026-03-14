{-# LANGUAGE OverloadedStrings #-}

module Api
  ( User(..)
  , getUser
  ) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON(..), (.:), withObject)
-- import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Req

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
