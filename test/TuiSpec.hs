{-# LANGUAGE OverloadedStrings #-}

module TuiSpec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as M
import qualified Data.Vector as Vec
import qualified Brick.Widgets.List as L

import qualified Api
import qualified Tui

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- Minimal kanji subject for testing
kanjiSubj :: Api.Subject
kanjiSubj = Api.Subject
  { Api.subjId       = 1
  , Api.subjType     = Api.Kanji
  , Api.subjChars    = Just "日"
  , Api.subjMeanings = ["Sun", "Day"]
  , Api.subjReadings = ["にち", "じつ"]
  }

-- Radical (no reading question)
radicalSubj :: Api.Subject
radicalSubj = Api.Subject
  { Api.subjId       = 2
  , Api.subjType     = Api.Radical
  , Api.subjChars    = Just "一"
  , Api.subjMeanings = ["One"]
  , Api.subjReadings = []
  }

-- Vocab subject
vocabSubj :: Api.Subject
vocabSubj = Api.Subject
  { Api.subjId       = 3
  , Api.subjType     = Api.Vocabulary
  , Api.subjChars    = Just "学校"
  , Api.subjMeanings = ["School"]
  , Api.subjReadings = ["がっこう"]
  }

mkQ :: Api.Subject -> Tui.QKind -> Tui.Q
mkQ s k = Tui.Q { Tui.qSubject = s, Tui.qKind = k }

-- Bare AppState with only progress/subjToAsg populated (for mkSubmissions)
stateWith :: M.Map Int Tui.Progress -> M.Map Int Int -> Tui.AppState
stateWith prog subjToAsg = Tui.AppState
  { Tui.stQueue        = []
  , Tui.stQueueWidget  = L.list Tui.QueueList (Vec.fromList []) 1
  , Tui.stInput        = ""
  , Tui.stProgress     = prog
  , Tui.stSubjToAsg    = subjToAsg
  , Tui.stRequeueAfter = 7
  , Tui.stCorrect      = 0
  , Tui.stWrong        = 0
  , Tui.stOverridden   = 0
  , Tui.stMode         = Tui.Normal
  , Tui.stBanner       = Nothing
  , Tui.stHasMore      = False
  , Tui.stWantsMore    = False
  }

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "normMeaning" $ do
    it "lowercases input"         $ Tui.normMeaning "Sun"        `shouldBe` "sun"
    it "trims whitespace"         $ Tui.normMeaning "  sun  "    `shouldBe` "sun"
    it "collapses inner spaces"   $ Tui.normMeaning "to  go"     `shouldBe` "to go"
    it "case-folds unicode"       $ Tui.normMeaning "GROß"       `shouldBe` "gross"
    it "empty string stays empty" $ Tui.normMeaning ""           `shouldBe` ""

  describe "normReading" $ do
    it "passes through hiragana unchanged" $
      Tui.normReading "にち" `shouldBe` "にち"
    it "converts romaji to hiragana" $
      Tui.normReading "nichi" `shouldBe` "にち"
    it "lowercases before converting" $
      Tui.normReading "NICHI" `shouldBe` "にち"
    it "trims whitespace" $
      Tui.normReading "  にち  " `shouldBe` "にち"

  describe "checkAnswer" $ do

    describe "meaning questions" $ do
      it "accepts exact match" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QMeaning) "Sun") `shouldBe` True
      it "accepts case-insensitive match" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QMeaning) "sun") `shouldBe` True
      it "accepts second accepted meaning" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QMeaning) "day") `shouldBe` True
      it "rejects wrong answer" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QMeaning) "Moon") `shouldBe` False
      it "returns accepted meanings on failure" $
        snd (Tui.checkAnswer (mkQ kanjiSubj Tui.QMeaning) "Moon") `shouldBe` ["Sun", "Day"]

    describe "reading questions" $ do
      it "accepts hiragana directly" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QReading) "にち") `shouldBe` True
      it "accepts romaji (converted to hiragana)" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QReading) "nichi") `shouldBe` True
      it "rejects wrong reading" $
        fst (Tui.checkAnswer (mkQ kanjiSubj Tui.QReading) "ka") `shouldBe` False
      it "accepts vocab reading" $
        fst (Tui.checkAnswer (mkQ vocabSubj Tui.QReading) "gakkou") `shouldBe` True

  describe "requeueAfterK" $ do
    let qs = map (\n -> mkQ kanjiSubj { Api.subjId = n } Tui.QMeaning) [1..5]
        q0 = mkQ kanjiSubj Tui.QMeaning

    it "inserts at position k" $
      map (Api.subjId . Tui.qSubject) (Tui.requeueAfterK 2 q0 qs)
        `shouldBe` [1, 2, 1, 3, 4, 5]
    it "k=0 puts item at front" $
      Api.subjId (Tui.qSubject (head (Tui.requeueAfterK 0 q0 qs)))
        `shouldBe` Api.subjId kanjiSubj
    it "k > length appends at end" $
      last (Tui.requeueAfterK 100 q0 qs) `shouldBe` q0
    it "empty queue returns singleton" $
      Tui.requeueAfterK 3 q0 [] `shouldBe` [q0]

  describe "requeueOnly" $ do
    let q0   = mkQ kanjiSubj Tui.QMeaning
        qs   = map (\n -> mkQ (kanjiSubj { Api.subjId = n }) Tui.QMeaning) [2..4]
        st0  = (stateWith (M.singleton 1 (Tui.initProgress kanjiSubj)) M.empty)
                 { Tui.stQueue        = q0 : qs
                 , Tui.stQueueWidget  = L.list Tui.QueueList (Vec.fromList (q0 : qs)) 1
                 , Tui.stRequeueAfter = 2
                 , Tui.stWrong        = 0
                 }

    it "does not increment wrong count" $ do
      let st' = Tui.requeueOnly q0 st0
      Tui.stWrong st' `shouldBe` 0

    it "does not increment pMeaningWrong" $ do
      let st' = Tui.requeueOnly q0 st0
      Tui.pMeaningWrong (Tui.stProgress st' M.! 1) `shouldBe` 0

    it "removes item from front of queue" $ do
      let st' = Tui.requeueOnly q0 st0
      Api.subjId (Tui.qSubject (head (Tui.stQueue st'))) `shouldBe` 2

    it "reinserts item at requeue position" $ do
      let st' = Tui.requeueOnly q0 st0
      map (Api.subjId . Tui.qSubject) (Tui.stQueue st') `shouldBe` [2, 3, 1, 4]

    it "clears input" $ do
      let st' = Tui.requeueOnly q0 st0 { Tui.stInput = "foo" }
      Tui.stInput st' `shouldBe` ""

  describe "initProgress" $ do
    it "kanji needs reading" $
      Tui.pReadingNeeded (Tui.initProgress kanjiSubj) `shouldBe` True
    it "radical does not need reading" $
      Tui.pReadingNeeded (Tui.initProgress radicalSubj) `shouldBe` False
    it "starts with nothing correct" $ do
      let p = Tui.initProgress kanjiSubj
      Tui.pMeaningOk p `shouldBe` False
      Tui.pReadingOk p `shouldBe` False
    it "starts with zero wrong counts" $ do
      let p = Tui.initProgress kanjiSubj
      Tui.pMeaningWrong p `shouldBe` 0
      Tui.pReadingWrong p `shouldBe` 0

  describe "markOk" $ do
    let prog0 = M.singleton 1 (Tui.initProgress kanjiSubj)

    it "marks meaning ok" $ do
      let result = Tui.markOk kanjiSubj Tui.QMeaning prog0
      Tui.pMeaningOk (result M.! 1) `shouldBe` True

    it "marks reading ok" $ do
      let result = Tui.markOk kanjiSubj Tui.QReading prog0
      Tui.pReadingOk (result M.! 1) `shouldBe` True

    it "does not affect other subjects" $
      M.lookup 99 (Tui.markOk kanjiSubj Tui.QMeaning prog0) `shouldBe` Nothing

  describe "incWrong" $ do
    let prog0 = M.singleton 1 (Tui.initProgress kanjiSubj)

    it "increments meaning wrong count" $
      Tui.pMeaningWrong (Tui.incWrong kanjiSubj Tui.QMeaning prog0 M.! 1) `shouldBe` 1
    it "increments reading wrong count" $
      Tui.pReadingWrong (Tui.incWrong kanjiSubj Tui.QReading prog0 M.! 1) `shouldBe` 1
    it "accumulates multiple wrongs" $ do
      let result = ( Tui.incWrong kanjiSubj Tui.QMeaning
                   . Tui.incWrong kanjiSubj Tui.QMeaning
                   $ prog0
                   ) M.! 1
      Tui.pMeaningWrong result `shouldBe` 2

  describe "mkSubmissions" $ do
    let subjToAsg = M.fromList [(1, 101), (3, 303)]

    it "produces one submission per subject with an assignment" $ do
      let prog = M.fromList
            [ (1, (Tui.initProgress kanjiSubj) { Tui.pMeaningWrong = 0, Tui.pReadingWrong = 1 })
            , (3, (Tui.initProgress vocabSubj)  { Tui.pMeaningWrong = 2, Tui.pReadingWrong = 0 })
            ]
          subs = Tui.mkSubmissions (stateWith prog subjToAsg)
      length subs `shouldBe` 2

    it "carries wrong counts into submission" $ do
      let prog = M.singleton 1
            (Tui.initProgress kanjiSubj) { Tui.pMeaningWrong = 3, Tui.pReadingWrong = 1 }
          [sub] = Tui.mkSubmissions (stateWith prog subjToAsg)
      Tui.subWrongMeaning sub `shouldBe` 3
      Tui.subWrongReading sub `shouldBe` 1

    it "uses the correct assignment id" $ do
      let prog  = M.singleton 1 (Tui.initProgress kanjiSubj)
          [sub] = Tui.mkSubmissions (stateWith prog subjToAsg)
      Tui.subAssignmentId sub `shouldBe` 101

    it "excludes subjects without an assignment mapping" $ do
      let prog = M.singleton 99 (Tui.initProgress (kanjiSubj { Api.subjId = 99 }))
          subs = Tui.mkSubmissions (stateWith prog subjToAsg)
      subs `shouldBe` []
