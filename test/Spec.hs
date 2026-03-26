{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import qualified Romaji
import qualified TuiSpec

main :: IO ()
main = hspec $ do
  TuiSpec.spec

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
      it "n' → ん"              $ Romaji.romajiToHiragana "n'"    `shouldBe` "ん"
      it "nn → ん (not んん)"   $ Romaji.romajiToHiragana "nn"    `shouldBe` "ん"
      it "trailing n → ん"      $ Romaji.romajiToHiragana "n"     `shouldBe` "ん"
      it "n before consonant"   $ Romaji.romajiToHiragana "nka"   `shouldBe` "んか"
      it "nn before vowel (nna)"$ Romaji.romajiToHiragana "nna"   `shouldBe` "んな"
      it "n before vowel stays" $ Romaji.romajiToHiragana "na"    `shouldBe` "な"
      it "kanna → かんな"       $ Romaji.romajiToHiragana "kanna" `shouldBe` "かんな"
      it "n'a → んあ"           $ Romaji.romajiToHiragana "n'a"   `shouldBe` "んあ"

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
