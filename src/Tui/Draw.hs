{-# LANGUAGE OverloadedStrings #-}

module Tui.Draw
  ( drawUi
  , theMap
  ) where

import qualified Api
import Tui.State
import Util (strPadLeft, strPadRight)

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V

import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Time (utcToLocalTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (L.listAttr,         V.defAttr)
  , (L.listSelectedAttr, V.defAttr `V.withStyle` V.reverseVideo)
  , (attrName "header",  fg V.cyan)
  , (attrName "ok",      fg V.green)
  , (attrName "bad",     fg V.red)
  , (attrName "hint",    V.defAttr `V.withStyle` V.dim)
  ]

drawUi :: AppState -> [Widget Name]
drawUi st =
  [ C.center $
      hBox
        [ hLimit 28 $ drawQueue st
        , B.vBorder
        , padLeft (Pad 1) $ hLimit 80 $ drawMain st
        ]
  ]

drawQueue :: AppState -> Widget Name
drawQueue st =
  B.borderWithLabel (str "Queue") $
    vBox
      [ L.renderList drawQueueItem True (stQueueWidget st)
      , padTop (Pad 1) $
          withAttr (attrName "hint") $
            str ("remaining: " <> show (length (stQueue st)))
      ]

drawQueueItem :: Bool -> Q -> Widget Name
drawQueueItem _ q =
  let s = qSubject q
  in str (displayItem s <> " [" <> kindLabel (qKind q) <> "]")

drawMain :: AppState -> Widget Name
drawMain st
  | stOverlay st == AllInfo =
      case currentQuestion st of
        Just q  -> drawAllInfo q st
        Nothing -> emptyWidget
  | stOverlay st == UserInfo       = drawUserInfo st
  | stOverlay st == ReviewSchedule = drawReviewSchedule st
  | otherwise =
  case currentQuestion st of
    Nothing ->
      B.borderWithLabel (str "Done") $
        viewport DoneViewport Vertical $
          padAll 1 $
            let confirmWidgets =
                  case stMode st of
                    ConfirmSubmit -> [padTop (Pad 1) (drawConfirmSubmit st)]
                    _             -> []
                detailWidgets =
                  case stSubmitDetails st of
                    [] -> []
                    ds -> padTop (Pad 1) (withAttr (attrName "hint") (str "--- submitted ---"))
                        : map (withAttr (attrName "hint") . txtWrap . T.pack) ds
                bannerWidgets =
                  case stBanner st of
                    Just msg -> [padTop (Pad 1) (txt msg)]
                    Nothing  -> []
                errorWidgets =
                  case stError st of
                    Just msg -> [padTop (Pad 1) (withAttr (attrName "bad") (txtWrap msg))]
                    Nothing  -> []
                hintLine =
                  case stMode st of
                    ConfirmSubmit ->
                      hintBox ["y/Enter=confirm", "n/Esc=cancel"]
                    Submitting ->
                      hintBox ["please wait…"]
                    _ | Just _ <- stBanner st ->
                          hintBox $ ["Esc=quit", "Ctrl-u=user", "Ctrl-v=reviews"] ++
                            [ "Ctrl-n=next batch" | stHasMore st ] ++
                            [ "↑↓/j/k=scroll" | not (null (stSubmitDetails st)) ]
                    _ -> hintBox ["Ctrl-s=submit to WaniKani", "Esc=quit", "Ctrl-u=user", "Ctrl-v=reviews"]
            in vBox
                 ( [ withAttr (attrName "ok") $ str "Session finished."
                   , str ("correct:     " <> show (stCorrect st))
                   , str ("wrong:       " <> show (stWrong st))
                   , str ("overridden:  " <> show (stOverridden st))
                   , str ("submissions: " <> show (length (mkSubmissions st)))
                   ]
                ++ confirmWidgets
                ++ detailWidgets
                ++ bannerWidgets
                ++ errorWidgets
                ++ [ padTop (Pad 1) hintLine ]
                 )

    Just q ->
      B.borderWithLabel (str ("Current" <> srsIndicator q st)) $
        padAll 1 $
          vBox $
            [ withAttr (attrName "header") $
                txt (T.pack (displayItem (qSubject q) <> " — " <> kindLabel (qKind q)))
            , padTop (Pad 1) $
                B.borderWithLabel (str "Input") $
                  padAll 1 $
                    txt (displayInput (qKind q) (stInput st))
            , padTop (Pad 1) $
                drawMode st q
            ]
            ++ ( case stError st of
                   Just msg ->
                     [ padTop (Pad 1)
                         (withAttr (attrName "bad") (txtWrap msg))
                     ]
                   Nothing -> []
               )
            ++
            [ padTop (Pad 1) $
                withAttr (attrName "hint") $
                  normalHintWidget q st
            ]

drawMode :: AppState -> Q -> Widget Name
drawMode st q =
  case stMode st of
    Normal ->
      emptyWidget

    WrongAnswer input expected ->
      let shownInput = case qKind q of
            QReading -> normReading input
            QMeaning -> input
      in vBox
        [ withAttr (attrName "bad") $
            txt ("✗ you entered: " <> shownInput)
        , withAttr (attrName "ok") $
            txt ("✓ accepted:    " <> T.pack (intercalate ", " expected))
        ]

    Feedback msg ->
      withAttr (attrName "ok") $ txt msg

    _ -> emptyWidget

drawConfirmSubmit :: AppState -> Widget Name
drawConfirmSubmit st =
  let subs = mkSubmissions st
      total = length subs
      withMistakes = length [ () | s <- subs, subWrongMeaning s > 0 || subWrongReading s > 0 ]
  in vBox
      [ withAttr (attrName "header") $
          str ("Submit " <> show total <> " reviews to WaniKani? [y/N]")
      , padTop (Pad 1) $
          str ("Items with mistakes: " <> show withMistakes)
      ]

drawAllInfo :: Q -> AppState -> Widget Name
drawAllInfo q st =
  B.borderWithLabel (txt label) $
    viewport InfoViewport Vertical $
      padAll 1 $
        vBox $
             assignSection
          ++ compSection
          ++ amalgSection
          ++ [ str ("Meanings:  " <> T.unpack (T.intercalate ", " (Api.subjMeanings subj))) ]
          ++ readSection
          ++ mnSection "Meaning mnemonic" (Api.subjMeaningMnemonic subj)
          ++ mnSection "Reading mnemonic" (Api.subjReadingMnemonic subj)
          ++ [ padTop (Pad 1) $
                 hintBox ["Ctrl-a/Esc/Enter=close", "↑↓/j/k=scroll"] ]
  where
    subj  = qSubject q
    label = fromMaybe "?" (Api.subjChars subj)
         <> " · " <> subjTypeLabel (Api.subjType subj)

    assignSection =
      let stageStr = case M.lookup (Api.subjId subj) (stSubjToAsg st) of
                       Just asg -> Api.srsStageLabel (Api.asSrsStage asg)
                       Nothing  -> "?"
      in [ str ("Level:     " <> show (Api.subjLevel subj))
         , str ("SRS stage: " <> stageStr)
         , str ""
         ]

    showKanjiReadings c =
      Api.subjType c == Api.Kanji
      && (Api.subjType subj == Api.Vocabulary || Api.subjType subj == Api.KanaVocabulary)
      && not (null (Api.subjReadings c))

    renderComponent c =
      let chars   = T.unpack (fromMaybe "?" (Api.subjChars c))
          meanings = T.unpack (T.intercalate ", " (Api.subjMeanings c))
          headerW  = str ("  " <> chars <> "  " <> meanings)
      in if showKanjiReadings c
           then [ headerW
                , withAttr (attrName "hint") $
                    str ("       readings: "
                      <> T.unpack (T.intercalate ", " (Api.subjReadings c)))
                ]
           else [ headerW ]

    compSection =
      let comps = mapMaybe (\cid -> M.lookup cid (stAllSubjects st))
                                      (Api.subjComponentIds subj)
      in case comps of
           [] -> []
           cs -> str "Components:" : concatMap renderComponent cs ++ [str ""]

    renderAmalgamation v =
      let chars   = T.unpack (fromMaybe "?" (Api.subjChars v))
          meaning = case Api.subjMeanings v of
                      (m:_) -> T.unpack m
                      []    -> "?"
          rd      = case Api.subjReadings v of
                      (r:_) -> " (" <> T.unpack r <> ")"
                      []    -> ""
      in str ("  " <> chars <> rd <> "  " <> meaning)

    amalgSection =
      case Api.subjType subj of
        Api.Kanji ->
          let vocabs = mapMaybe (\aid -> M.lookup aid (stAllSubjects st))
                                          (Api.subjAmalgamationIds subj)
          in case vocabs of
               [] -> []
               vs -> str "Vocabulary:" : map renderAmalgamation vs ++ [str ""]
        _ -> []

    readSection =
      case Api.subjReadings subj of
        [] -> []
        rs -> [ str ("Readings:  " <> T.unpack (T.intercalate ", " rs)) ]

    mnSection _     Nothing  = []
    mnSection title (Just t) =
      [ str ""
      , withAttr (attrName "hint") (str (title <> ":"))
      , txtWrap (stripWkTags t)
      ]

drawUserInfo :: AppState -> Widget Name
drawUserInfo st =
  let u = stUser st
  in B.borderWithLabel (str "User") $
       viewport UserViewport Vertical $
         padAll 1 $
           vBox
             [ txt ("Username: " <> Api.userUsername u)
             , str ("Level:    " <> show (Api.userLevel u))
             , txt ("Profile:  " <> Api.userProfileUrl u)
             , padTop (Pad 1) $ hintBox ["Ctrl-u/Esc=close"]
             ]

drawReviewSchedule :: AppState -> Widget Name
drawReviewSchedule st =
  let rows    = Api.reviewsPerHourNext24 (stNow st) (stSummary st)
      nowAvail = Api.reviewsAvailableNow (stNow st) (stSummary st)
      fmtHour utc =
        formatTime defaultTimeLocale "%F %H:00" (utcToLocalTime (stTZ st) utc)
  in B.borderWithLabel (str "Review Schedule") $
       viewport ReviewViewport Vertical $
         padAll 1 $
           vBox $
             [ withAttr (attrName "header") $
                 str ("Available now: " <> show nowAvail)
             , padTop (Pad 1) $
                 withAttr (attrName "hint") $ str "Hour (local)            New   Open"
             ] ++
             map (\(hStart, newN, openN) ->
               str ( strPadRight 24 (fmtHour hStart)
                  <> strPadLeft 3 (show newN) <> "  "
                  <> strPadLeft 4 (show openN) )
             ) rows ++
             [ padTop (Pad 1) $ hintBox ["Ctrl-v/Esc=close", "↑↓/j/k=scroll"] ]

subjTypeLabel :: Api.SubjectType -> Text
subjTypeLabel Api.Radical        = "Radical"
subjTypeLabel Api.Kanji          = "Kanji"
subjTypeLabel Api.Vocabulary     = "Vocabulary"
subjTypeLabel Api.KanaVocabulary = "Kana Vocabulary"

-- Strip WaniKani HTML-like tags (<radical>…</radical> etc.), keeping inner text.
stripWkTags :: Text -> Text
stripWkTags t = T.pack (go (T.unpack t))
  where
    go []        = []
    go ('<':cs)  = go (drop 1 (dropWhile (/= '>') cs))
    go (c  :cs)  = c : go cs

-- | Render a list of hint strings as auto-wrapping text.
hintBox :: [Text] -> Widget Name
hintBox hints =
  withAttr (attrName "hint") $
    txtWrap (T.intercalate "  " hints)

srsIndicator :: Q -> AppState -> String
srsIndicator q st =
  case M.lookup (Api.subjId (qSubject q)) (stSubjToAsg st) of
    Just asg -> " · " <> Api.srsStageLabel (Api.asSrsStage asg)
    Nothing  -> ""

normalHintWidget :: Q -> AppState -> Widget Name
normalHintWidget q st =
  case stMode st of
    WrongAnswer _ _ ->
      hintBox $
        [ "Ctrl-o=override correct", "Ctrl-r=requeue (no penalty)", "Enter=requeue (wrong)"
        , "Ctrl-a=all info", "Ctrl-u=user", "Ctrl-v=reviews"
        ] ++ [ "Ctrl-p=play audio" | hasAudio q st ]
    _ ->
      hintBox $
        [ "Enter=submit", "Ctrl-o=override", "Ctrl-r=requeue"
        , "Ctrl-a=all info", "Ctrl-u=user", "Ctrl-v=reviews", "Esc=quit"
        ] ++ [ "Ctrl-p=play audio" | hasAudio q st ]
