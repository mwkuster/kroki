{-# LANGUAGE OverloadedStrings #-}

module Romaji
  ( romajiToHiragana
  ) where

import Data.Char (isAlpha, toLower)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | Convert (mostly Hepburn) romaji to hiragana.
-- Intended for WaniKani-style readings; not a full IME.
romajiToHiragana :: Text -> Text
romajiToHiragana = go . T.toLower . T.filter keep
  where
    keep c = isAlpha c || c == '\'' || c == '-' -- allow n' and ignore hyphens
    go t
      | T.null t  = ""
      | otherwise =
          case smallTsu t of
            Just (out, rest) -> out <> go rest
            Nothing ->
              case parseN t of
                Just rest -> "ん" <> go rest
                Nothing  ->
                  case matchKana t of
                    Just (k, rest) -> k <> go rest
                    Nothing ->
                      -- If unknown chunk, drop one char (keeps session moving)
                      go (T.drop 1 t)

-- small っ for doubled consonants: kk, ss, tt, pp, etc.
-- not for vowels or 'n' (handled separately)
smallTsu :: Text -> Maybe (Text, Text)
smallTsu t = do
  c1 <- T.uncons t >>= (Just . fst)
  c2 <- T.drop 1 t `T.uncons` >>= (Just . fst)
  let isConsonant c = c `elem` ("bcdfghjklmnpqrstvwxyz" :: String)
  if c1 == c2 && isConsonant c1 && c1 /= 'n'
     then Just ("っ", T.drop 1 t)
     else Nothing

-- Handle 'n' as ん when:
--  - "n'" explicitly
--  - "nn" (consume one n, leave one for next syllable)
--  - "n" before a non-vowel and not 'y'
parseN :: Text -> Maybe Text
parseN t
  | "n'" `T.isPrefixOf` t = Just (T.drop 2 t)
  | "nn" `T.isPrefixOf` t = Just (T.drop 1 t)
  | "n"  `T.isPrefixOf` t =
      case T.drop 1 t `T.uncons` of
        Nothing      -> Just ""          -- trailing n
        Just (c, _)  ->
          if c `elem` ("aiueoy" :: String)
            then Nothing                -- part of syllable: na/nya/ni...
            else Just (T.drop 1 t)      -- n + consonant => ん
  | otherwise = Nothing

matchKana :: Text -> Maybe (Text, Text)
matchKana t = do
  -- prefer longest match
  (r, k) <- find (\(r,_) -> r `T.isPrefixOf` t) table
  pure (k, T.drop (T.length r) t)

-- Ordered longest-first. Covers the bulk of WK readings.
table :: [(Text, Text)]
table =
  [ ("kya","きゃ"),("kyu","きゅ"),("kyo","きょ")
  , ("gya","ぎゃ"),("gyu","ぎゅ"),("gyo","ぎょ")
  , ("sha","しゃ"),("shu","しゅ"),("sho","しょ")
  , ("ja","じゃ"), ("ju","じゅ"), ("jo","じょ")
  , ("cha","ちゃ"),("chu","ちゅ"),("cho","ちょ")
  , ("nya","にゃ"),("nyu","にゅ"),("nyo","にょ")
  , ("hya","ひゃ"),("hyu","ひゅ"),("hyo","ひょ")
  , ("bya","びゃ"),("byu","びゅ"),("byo","びょ")
  , ("pya","ぴゃ"),("pyu","ぴゅ"),("pyo","ぴょ")
  , ("mya","みゃ"),("myu","みゅ"),("myo","みょ")
  , ("rya","りゃ"),("ryu","りゅ"),("ryo","りょ")

  , ("tsu","つ"),("shi","し"),("chi","ち"),("fu","ふ")
  , ("dzu","づ"),("ji","じ")   -- keep simple; WK often uses 'ji'
  , ("kwi","くぃ"),("kwe","くぇ"),("kwo","くぉ")
  , ("gwi","ぐぃ"),("gwe","ぐぇ"),("gwo","ぐぉ")

  , ("kya","きゃ") -- (harmless redundancy if you edit table later)
  ]
  ++ basicRows

basicRows :: [(Text, Text)]
basicRows =
  [ ("a","あ"),("i","い"),("u","う"),("e","え"),("o","お")

  , ("ka","か"),("ki","き"),("ku","く"),("ke","け"),("ko","こ")
  , ("sa","さ"),("su","す"),("se","せ"),("so","そ") -- shi handled above
  , ("ta","た"),("te","て"),("to","と")             -- chi/tsu handled above
  , ("na","な"),("ni","に"),("nu","ぬ"),("ne","ね"),("no","の")
  , ("ha","は"),("hi","ひ"),("he","へ"),("ho","ほ") -- fu handled above
  , ("ma","ま"),("mi","み"),("mu","む"),("me","め"),("mo","も")
  , ("ya","や"),("yu","ゆ"),("yo","よ")
  , ("ra","ら"),("ri","り"),("ru","る"),("re","れ"),("ro","ろ")
  , ("wa","わ"),("wo","を")
  , ("ga","が"),("gi","ぎ"),("gu","ぐ"),("ge","げ"),("go","ご")
  , ("za","ざ"),("zu","ず"),("ze","ぜ"),("zo","ぞ")
  , ("da","だ"),("de","で"),("do","ど")
  , ("ba","ば"),("bi","び"),("bu","ぶ"),("be","べ"),("bo","ぼ")
  , ("pa","ぱ"),("pi","ぴ"),("pu","ぷ"),("pe","ぺ"),("po","ぽ")

  -- small vowels (occasionally useful)
  , ("xa","ぁ"),("xi","ぃ"),("xu","ぅ"),("xe","ぇ"),("xo","ぉ")
  , ("la","ぁ"),("li","ぃ"),("lu","ぅ"),("le","ぇ"),("lo","ぉ")

  -- small ya/yu/yo
  , ("xya","ゃ"),("xyu","ゅ"),("xyo","ょ")
  , ("lya","ゃ"),("lyu","ゅ"),("lyo","ょ")

  -- small tsu
  , ("xtsu","っ"),("ltsu","っ")
  ]
