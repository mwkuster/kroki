{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import qualified Romaji
import qualified TuiSpec
import qualified Config
import qualified Api

main :: IO ()
main = hspec $ do
  TuiSpec.spec
  configSpec
  apiSpec

  describe "romajiToHiragana" $ do

    describe "basic vowels" $ do
      it "a → あ" $ Romaji.romajiToHiragana "a"  `shouldBe` "あ"
      it "i → い" $ Romaji.romajiToHiragana "i"  `shouldBe` "い"
      it "u → う" $ Romaji.romajiToHiragana "u"  `shouldBe` "う"
      it "e → え" $ Romaji.romajiToHiragana "e"  `shouldBe` "え"
      it "o → お" $ Romaji.romajiToHiragana "o"  `shouldBe` "お"

    describe "basic consonant+vowel" $ do
      it "ka → か" $ Romaji.romajiToHiragana "ka" `shouldBe` "か"
      it "ki → き" $ Romaji.romajiToHiragana "ki" `shouldBe` "き"
      it "sa → さ" $ Romaji.romajiToHiragana "sa" `shouldBe` "さ"
      it "ta → た" $ Romaji.romajiToHiragana "ta" `shouldBe` "た"
      it "na → な" $ Romaji.romajiToHiragana "na" `shouldBe` "な"
      it "ha → は" $ Romaji.romajiToHiragana "ha" `shouldBe` "は"
      it "ma → ま" $ Romaji.romajiToHiragana "ma" `shouldBe` "ま"
      it "ya → や" $ Romaji.romajiToHiragana "ya" `shouldBe` "や"
      it "ra → ら" $ Romaji.romajiToHiragana "ra" `shouldBe` "ら"
      it "wa → わ" $ Romaji.romajiToHiragana "wa" `shouldBe` "わ"

    describe "special spellings" $ do
      it "shi → し" $ Romaji.romajiToHiragana "shi" `shouldBe` "し"
      it "chi → ち" $ Romaji.romajiToHiragana "chi" `shouldBe` "ち"
      it "tsu → つ" $ Romaji.romajiToHiragana "tsu" `shouldBe` "つ"
      it "fu  → ふ" $ Romaji.romajiToHiragana "fu"  `shouldBe` "ふ"
      it "ji  → じ" $ Romaji.romajiToHiragana "ji"  `shouldBe` "じ"

    describe "palatalized sounds" $ do
      it "kya → きゃ" $ Romaji.romajiToHiragana "kya" `shouldBe` "きゃ"
      it "sha → しゃ" $ Romaji.romajiToHiragana "sha" `shouldBe` "しゃ"
      it "shu → しゅ" $ Romaji.romajiToHiragana "shu" `shouldBe` "しゅ"
      it "sho → しょ" $ Romaji.romajiToHiragana "sho" `shouldBe` "しょ"
      it "cha → ちゃ" $ Romaji.romajiToHiragana "cha" `shouldBe` "ちゃ"
      it "ryu → りゅ" $ Romaji.romajiToHiragana "ryu" `shouldBe` "りゅ"

    describe "ん (n)" $ do
      it "n' → ん"              $ Romaji.romajiToHiragana "n'"     `shouldBe` "ん"
      it "nn → ん (not んん)"   $ Romaji.romajiToHiragana "nn"     `shouldBe` "ん"
      it "trailing n → ん"      $ Romaji.romajiToHiragana "n"      `shouldBe` "ん"
      it "n before consonant"   $ Romaji.romajiToHiragana "nka"    `shouldBe` "んか"
      it "nn before vowel (nna)"$ Romaji.romajiToHiragana "nna"    `shouldBe` "んな"
      it "n before vowel stays" $ Romaji.romajiToHiragana "na"     `shouldBe` "な"
      it "kanna → かんな"       $ Romaji.romajiToHiragana "kanna"  `shouldBe` "かんな"
      it "denwa → でんわ"       $ Romaji.romajiToHiragana "denwa"  `shouldBe` "でんわ"
      it "dennwa → でんわ"      $ Romaji.romajiToHiragana "denbwa" `shouldBe` "でんわ"
      it "n'a → んあ"           $ Romaji.romajiToHiragana "n'a"    `shouldBe` "んあ"

    describe "っ (small tsu / doubled consonant)" $ do
      it "kka → っか" $ Romaji.romajiToHiragana "kka"  `shouldBe` "っか"
      it "tte → って" $ Romaji.romajiToHiragana "tte"  `shouldBe` "って"
      it "ssh → っし" $ Romaji.romajiToHiragana "sshi" `shouldBe` "っし"
      it "pp  → っぱ" $ Romaji.romajiToHiragana "ppa"  `shouldBe` "っぱ"

    describe "multi-syllable words" $ do
      it "nihon → にほん"    $ Romaji.romajiToHiragana "nihon"    `shouldBe` "にほん"
      it "sakura → さくら"  $ Romaji.romajiToHiragana "sakura"   `shouldBe` "さくら"
      it "gakkou → がっこう" $ Romaji.romajiToHiragana "gakkou"  `shouldBe` "がっこう"
      it "macchi → まっち"  $ Romaji.romajiToHiragana "macchi"   `shouldBe` "まっち"
      it "chidimaru → ちぢまる"  $ Romaji.romajiToHiragana "chidimaru" `shouldBe` "ちぢまる"

    describe "case insensitivity" $ do
      it "KA → か" $ Romaji.romajiToHiragana "KA"  `shouldBe` "か"
      it "SHI → し" $ Romaji.romajiToHiragana "SHI" `shouldBe` "し"

  describe "romajiToHiraganaLive" $ do

    describe "complete input converts fully" $ do
      it "ka → か"  $ Romaji.romajiToHiraganaLive "ka"  `shouldBe` "か"
      it "shi → し" $ Romaji.romajiToHiraganaLive "shi" `shouldBe` "し"

    describe "pending suffix shown as-is" $ do
      it "k stays pending"  $ Romaji.romajiToHiraganaLive "k"  `shouldBe` "k"
      it "sh stays pending" $ Romaji.romajiToHiraganaLive "sh" `shouldBe` "sh"
      it "n stays pending"  $ Romaji.romajiToHiraganaLive "n"  `shouldBe` "n"

    describe "mixed converted + pending" $ do
      it "kak → か + k pending" $ Romaji.romajiToHiraganaLive "kak"  `shouldBe` "かk"
      it "kas → か + s pending" $ Romaji.romajiToHiraganaLive "kas"  `shouldBe` "かs"
      it "shan → しゃ + n pending" $ Romaji.romajiToHiraganaLive "shan" `shouldBe` "しゃn"
      it "shik → し + k pending"  $ Romaji.romajiToHiraganaLive "shik" `shouldBe` "しk"

    describe "nn handling" $ do
      it "nn alone → ん"   $ Romaji.romajiToHiraganaLive "nn"   `shouldBe` "ん"
      it "nna → んな"      $ Romaji.romajiToHiraganaLive "nna"  `shouldBe` "んな"
      it "kanna → かんな"  $ Romaji.romajiToHiraganaLive "kanna" `shouldBe` "かんな"

    describe "っ (doubled consonant)" $ do
      it "kka → っか" $ Romaji.romajiToHiraganaLive "kka" `shouldBe` "っか"
      it "kk pending" $ Romaji.romajiToHiraganaLive "kk"  `shouldBe` "っk"

--------------------------------------------------------------------------------
-- Config parsing tests
--------------------------------------------------------------------------------

configSpec :: Spec
configSpec = describe "parseConfig" $ do

  it "parses token" $
    Config.cfgToken (Config.parseConfig "token=abc123") `shouldBe` Just "abc123"

  it "parses batch_size" $
    Config.cfgBatchSize (Config.parseConfig "batch_size=5") `shouldBe` Just 5

  it "parses requeue_after" $
    Config.cfgRequeueAfter (Config.parseConfig "requeue_after=3") `shouldBe` Just 3

  it "parses audio_player with spaces in command" $
    Config.cfgAudioPlayer (Config.parseConfig "audio_player=mpv --really-quiet")
      `shouldBe` Just "mpv --really-quiet"

  it "ignores comment lines" $
    Config.cfgToken (Config.parseConfig "# this is a comment\ntoken=xyz") `shouldBe` Just "xyz"

  it "ignores blank lines" $
    Config.cfgToken (Config.parseConfig "\n\ntoken=abc\n\n") `shouldBe` Just "abc"

  it "returns Nothing for missing key" $
    Config.cfgToken (Config.parseConfig "") `shouldBe` Nothing

  it "returns Nothing for empty value" $
    Config.cfgToken (Config.parseConfig "token=") `shouldBe` Nothing

  it "returns Nothing for malformed int" $
    Config.cfgBatchSize (Config.parseConfig "batch_size=not_a_number") `shouldBe` Nothing

  it "trims whitespace around key and value" $
    Config.cfgToken (Config.parseConfig "  token  =  mytoken  ") `shouldBe` Just "mytoken"

  it "uses first occurrence when key appears twice" $
    Config.cfgToken (Config.parseConfig "token=first\ntoken=second") `shouldBe` Just "first"

  it "defaultBatchSize is 10" $
    Config.defaultBatchSize `shouldBe` 10

  it "defaultRequeueAfter is 7" $
    Config.defaultRequeueAfter `shouldBe` 7

--------------------------------------------------------------------------------
-- API JSON parsing tests
--------------------------------------------------------------------------------

apiSpec :: Spec
apiSpec = describe "Api JSON parsing" $ do

  describe "User" $ do
    let validJson :: ByteString
        validJson = "{\"data\":{\"username\":\"bob\",\"level\":5,\"profile_url\":\"https://example.com\"}}"

    it "parses username" $
      fmap Api.userUsername (decode validJson) `shouldBe` Just "bob"

    it "parses level" $
      fmap Api.userLevel (decode validJson) `shouldBe` Just 5

    it "parses profile_url" $
      fmap Api.userProfileUrl (decode validJson) `shouldBe` Just "https://example.com"

    it "fails on missing data field" $
      (decode "{\"username\":\"bob\"}" :: Maybe Api.User) `shouldBe` Nothing

  describe "ReviewBucket" $ do
    let validJson :: ByteString
        validJson = "{\"available_at\":\"2024-01-01T00:00:00.000000Z\",\"subject_ids\":[1,2,3]}"

    it "parses subject_ids" $
      fmap Api.rbSubjectIds (decode validJson) `shouldBe` Just [1, 2, 3]

    it "fails on invalid available_at" $
      (decode "{\"available_at\":\"not-a-date\",\"subject_ids\":[]}" :: Maybe Api.ReviewBucket)
        `shouldBe` Nothing

  describe "Subject (kanji)" $ do
    -- WaniKani omits absent optional fields rather than sending null;
    -- aeson's .:? only yields Nothing for absent keys, not for null values
    -- when the target type is Text.
    let validJson :: ByteString
        validJson = mconcat
          [ "{\"id\":1,\"object\":\"kanji\",\"data\":{"
          , "\"characters\":\"\\u65e5\","
          , "\"meanings\":[{\"meaning\":\"Sun\",\"accepted_answer\":true}"
          , ",{\"meaning\":\"Day\",\"accepted_answer\":true}],"
          , "\"readings\":[{\"reading\":\"\\u306b\\u3061\",\"accepted_answer\":true}"
          , ",{\"reading\":\"\\u3058\\u3064\",\"accepted_answer\":false}],"
          , "\"component_subject_ids\":[]}}"
          ]

    it "parses subject type" $
      fmap Api.subjType (decode validJson) `shouldBe` Just Api.Kanji

    it "parses accepted meanings only" $
      fmap Api.subjMeanings (decode validJson) `shouldBe` Just ["Sun", "Day"]

    it "parses accepted readings only" $
      fmap Api.subjReadings (decode validJson) `shouldBe` Just ["\12395\12385"]

    it "parses characters" $
      fmap Api.subjChars (decode validJson) `shouldBe` Just (Just "\26085")

  describe "Subject (radical)" $ do
    let validJson :: ByteString
        validJson = mconcat
          [ "{\"id\":2,\"object\":\"radical\",\"data\":{"
          , "\"characters\":\"\\u4e00\","
          , "\"meanings\":[{\"meaning\":\"One\",\"accepted_answer\":true}],"
          , "\"component_subject_ids\":[]}}"
          ]

    it "has no readings" $
      fmap Api.subjReadings (decode validJson) `shouldBe` Just []

    it "has no reading mnemonic" $
      fmap Api.subjReadingMnemonic (decode validJson) `shouldBe` Just Nothing

  describe "Subject (unknown type)" $ do
    it "fails to parse unknown object type" $
      (decode "{\"id\":1,\"object\":\"unknown\",\"data\":{}}" :: Maybe Api.Subject)
        `shouldBe` Nothing
