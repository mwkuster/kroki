{-# LANGUAGE OverloadedStrings #-}

module Config
  ( KrokiConfig(..)
  , loadConfig
  ) where

import Control.Exception (IOException, catch)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import System.Directory (getXdgDirectory, XdgDirectory(XdgConfig))
import System.FilePath ((</>))

data KrokiConfig = KrokiConfig
  { cfgToken :: Maybe String
  , cfgBatchSize :: Maybe Int
  } deriving (Show, Eq)

-- Loads ~/.config/kroki/config (via XDG)
loadConfig :: IO KrokiConfig
loadConfig = do
  base <- getXdgDirectory XdgConfig "kroki"
  let path = base </> "config"
  content <- readFile path `catch` \(_ :: IOException) -> pure ""
  pure $ parseConfig content

parseConfig :: String -> KrokiConfig
parseConfig s =
  KrokiConfig
    { cfgToken = lookupKey "token" (lines s)
    , cfgBatchSize = lookupInt "batch_size" (lines s)
    }

lookupKey :: String -> [String] -> Maybe String
lookupKey key ls =
  case [ drop 1 v | line <- ls
                  , let line' = trim line
                  , not (null line')
                  , head line' /= '#'
                  , (k, rest) <- [break (=='=') line']
                  , trim k == key
                  , let v = trim rest
                  , not (null v)
                  ] of
    (v:_) -> Just v
    []    -> Nothing

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

lookupInt :: String -> [String] -> Maybe Int
lookupInt key ls =
  case lookupKey key ls of
    Just v  -> case reads v of
                 [(n, "")] -> Just n
                 _         -> Nothing
    Nothing -> Nothing
