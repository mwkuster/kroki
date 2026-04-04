{-# LANGUAGE OverloadedStrings #-}

module Tui
  ( runStudyTui
  , SubmitResult(..)
  , Submission(..)
  , QKind(..)
  , Q(..)
  , Progress(..)
  , AppState(..)
  , Mode(..)
  , Overlay(..)
  , Name(..)
  , checkAnswer
  , normMeaning
  , normReading
  , mkSubmissions
  , requeueAfterK
  , requeueOnly
  , initProgress
  , markOk
  , incWrong
  ) where

import qualified Api
import qualified Romaji
import Util (strPadLeft, strPadRight)

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, isJust, fromMaybe)
import qualified Data.Vector as Vec
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Time (UTCTime, utcToLocalTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone)
import System.Process (spawnProcess)
import System.Random (randomRIO)

--------------------------------------------------------------------------------
-- Public data
--------------------------------------------------------------------------------

data QKind = QMeaning | QReading
  deriving (Show, Eq, Ord)

data Q = Q
  { qSubject :: Api.Subject
  , qKind    :: QKind
  } deriving (Show, Eq)

data Submission = Submission
  { subAssignmentId :: Int
  , subWrongMeaning :: Int
  , subWrongReading :: Int
  } deriving (Show, Eq)

data SubmitResult = SubmitResult
  { srMessage :: String
  , srHasMore :: Bool
  , srDetails :: [String]   -- per-submission lines for TUI display
  } deriving (Show)

data Progress = Progress
  { pMeaningOk     :: Bool
  , pReadingNeeded :: Bool
  , pReadingOk     :: Bool
  , pMeaningWrong  :: Int
  , pReadingWrong  :: Int
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- TUI state
--------------------------------------------------------------------------------

data Name = QueueList | InfoViewport | UserViewport | ReviewViewport | DoneViewport
  deriving (Ord, Eq, Show)

data Overlay = NoOverlay | AllInfo | UserInfo | ReviewSchedule
  deriving (Show, Eq)

data Mode
  = Normal
  | WrongAnswer Text [String]  -- user's input, accepted answers
  | Feedback Text
  | ConfirmSubmit
  | Finished
  deriving (Show, Eq)

data AppState = AppState
  { stQueue        :: [Q]
  , stQueueWidget  :: L.List Name Q
  , stInput        :: Text
  , stProgress     :: M.Map Int Progress
  , stSubjToAsg    :: M.Map Int Int
  , stRequeueAfter :: Int
  , stCorrect      :: Int
  , stWrong        :: Int
  , stOverridden   :: Int
  , stMode         :: Mode
  , stBanner       :: Maybe Text
  , stHasMore      :: Bool
  , stWantsMore    :: Bool
  , stAudioPlayer   :: Maybe String          -- command to play audio (e.g. "mpv --really-quiet")
  , stSubmitDetails :: [String]              -- per-submission lines shown after submit
  , stOverlay       :: Overlay               -- active info overlay
  , stAllSubjects   :: M.Map Int Api.Subject -- full subject map incl. components
  , stUser          :: Api.User
  , stSummary       :: Api.Summary
  , stNow           :: UTCTime
  , stTZ            :: TimeZone
  }

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

runStudyTui :: Int -> Maybe String -> Api.User -> Api.Summary -> UTCTime -> TimeZone -> M.Map Int Api.Subject -> M.Map Int Int -> [Api.Subject] -> ([Submission] -> IO SubmitResult) -> IO Bool
runStudyTui rqAfter audioPlayer user summary now tz allSubjects subjToAsg subjects submitFn = do
  let queue0 = concatMap mkQuestions subjects
      prog0  = M.fromList [ (Api.subjId s, initProgress s) | s <- subjects ]

  queue <- shuffle queue0

  let st0 = AppState
        { stQueue        = queue
        , stQueueWidget  = mkQueueWidget queue
        , stInput        = T.empty
        , stProgress     = prog0
        , stSubjToAsg    = subjToAsg
        , stRequeueAfter = rqAfter
        , stCorrect      = 0
        , stWrong        = 0
        , stOverridden   = 0
        , stMode          = Normal
        , stBanner        = Nothing
        , stHasMore       = False
        , stWantsMore     = False
        , stAudioPlayer   = audioPlayer
        , stSubmitDetails = []
        , stOverlay       = NoOverlay
        , stAllSubjects   = allSubjects
        , stUser          = user
        , stSummary       = summary
        , stNow           = now
        , stTZ            = tz
        }

  let buildVty = VCP.mkVty V.defaultConfig
  initialVty <- buildVty
  finalState <- customMain initialVty buildVty Nothing (app submitFn) st0
  pure (stWantsMore finalState)

--------------------------------------------------------------------------------
-- App
--------------------------------------------------------------------------------

app :: ([Submission] -> IO SubmitResult) -> App AppState e Name
app submitFn = App
  { appDraw         = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent submitFn
  , appStartEvent   = pure ()
  , appAttrMap      = const theMap
  }

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (L.listAttr,         V.defAttr)
  , (L.listSelectedAttr, V.defAttr `V.withStyle` V.reverseVideo)
  , (attrName "header",  fg V.cyan)
  , (attrName "ok",      fg V.green)
  , (attrName "bad",     fg V.red)
  , (attrName "hint",    V.defAttr `V.withStyle` V.dim)
  ]

--------------------------------------------------------------------------------
-- Drawing
--------------------------------------------------------------------------------

drawUi :: AppState -> [Widget Name]
drawUi st =
  [ C.center $
      hBox
        [ hLimit 36 $ drawQueue st
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
                        : map (withAttr (attrName "hint") . str) ds
                bannerWidgets =
                  case stBanner st of
                    Just msg -> [padTop (Pad 1) (txt msg)]
                    Nothing  -> []
                hintLine =
                  case stMode st of
                    ConfirmSubmit ->
                      hintBox ["y/Enter=confirm", "n/Esc=cancel"]
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
                ++ [ padTop (Pad 1) hintLine ]
                 )

    Just q ->
      B.borderWithLabel (str "Current") $
        padAll 1 $
          vBox
            [ withAttr (attrName "header") $
                txt (T.pack (displayItem (qSubject q) <> " — " <> kindLabel (qKind q)))
            , padTop (Pad 1) $
                B.borderWithLabel (str "Input") $
                  padAll 1 $
                    txt (displayInput (qKind q) (stInput st))
            , padTop (Pad 1) $
                drawMode st q
            , padTop (Pad 1) $
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
             compSection
          ++ [ str ("Meanings:  " <> T.unpack (T.intercalate ", " (Api.subjMeanings subj))) ]
          ++ readSection
          ++ mnSection "Meaning mnemonic" (Api.subjMeaningMnemonic subj)
          ++ mnSection "Reading mnemonic" (Api.subjReadingMnemonic subj)
          ++ [ padTop (Pad 1) $
                 hintBox ["Ctrl-a/Esc=close", "↑↓/j/k=scroll"] ]
  where
    subj  = qSubject q
    label = fromMaybe "?" (Api.subjChars subj)
         <> " · " <> subjTypeLabel (Api.subjType subj)

    compSection =
      let comps = mapMaybe (\cid -> M.lookup cid (stAllSubjects st))
                                      (Api.subjComponentIds subj)
      in case comps of
           [] -> []
           cs -> str "Components:"
               : map (\c -> str ("  " <> T.unpack (fromMaybe "?" (Api.subjChars c))
                              <> "  " <> T.unpack (T.intercalate ", " (Api.subjMeanings c)))) cs
              ++ [str ""]

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

--------------------------------------------------------------------------------
-- Event handling
--------------------------------------------------------------------------------

handleEvent :: ([Submission] -> IO SubmitResult) -> BrickEvent Name e -> EventM Name AppState ()
handleEvent submitFn (VtyEvent ev) = do
  st <- get
  if stOverlay st /= NoOverlay
    then handleOverlay ev
    else case stMode st of
      WrongAnswer _ _ -> handleWrongAnswer ev
      ConfirmSubmit   -> handleConfirm submitFn ev
      Finished        -> handleFinished ev
      _               -> handleNormal ev
handleEvent _ _ = pure ()

handleOverlay :: V.Event -> EventM Name AppState ()
handleOverlay ev =
  case ev of
    V.EvKey (V.KChar 'a') [V.MCtrl] -> close
    V.EvKey (V.KChar 'u') [V.MCtrl] -> close
    V.EvKey (V.KChar 'v') [V.MCtrl] -> close
    V.EvKey V.KEsc []                -> close
    V.EvKey V.KUp []                 -> scroll (-1)
    V.EvKey V.KDown []               -> scroll 1
    V.EvKey (V.KChar 'k') []         -> scroll (-1)
    V.EvKey (V.KChar 'j') []         -> scroll 1
    _                                -> pure ()
  where
    close = modify $ \st -> st { stOverlay = NoOverlay }
    scroll n = do
      st <- get
      let vp = case stOverlay st of
                 AllInfo        -> viewportScroll InfoViewport
                 UserInfo       -> viewportScroll UserViewport
                 ReviewSchedule -> viewportScroll ReviewViewport
                 NoOverlay      -> error "scroll called with NoOverlay"
      vScrollBy vp n

handleWrongAnswer :: V.Event -> EventM Name AppState ()
handleWrongAnswer ev =
  case ev of
    V.EvKey (V.KChar 'o') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  -> put (advanceOverride q st { stMode = Normal })

    V.EvKey (V.KChar 'r') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  -> put (requeueOnly q st { stMode = Normal })

    V.EvKey (V.KChar 'p') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Just q | hasAudio q st -> liftIO $ playAudio (stAudioPlayer st) (qSubject q)
        _ -> pure ()

    V.EvKey (V.KChar 'a') [V.MCtrl] ->
      modify $ \st -> st { stOverlay = AllInfo }
    V.EvKey (V.KChar 'u') [V.MCtrl] ->
      modify $ \st -> st { stOverlay = UserInfo }
    V.EvKey (V.KChar 'v') [V.MCtrl] ->
      modify $ \st -> st { stOverlay = ReviewSchedule }

    V.EvKey V.KEnter [] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  -> put (requeueWrong q st { stMode = Normal })

    V.EvKey V.KEsc [] -> do
      st <- get
      put st { stMode = Normal }

    _ -> pure ()

handleConfirm :: ([Submission] -> IO SubmitResult) -> V.Event -> EventM Name AppState ()
handleConfirm submitFn ev =
  case ev of
    V.EvKey (V.KChar 'y') [] -> doSubmit
    V.EvKey V.KEnter []      -> doSubmit
    V.EvKey (V.KChar 'n') [] -> do
      st <- get
      put st { stMode = Finished }

    V.EvKey V.KEsc [] -> do
      st <- get
      put st { stMode = Finished }

    _ -> pure ()
  where
    doSubmit = do
      st <- get
      result <- liftIO (submitFn (mkSubmissions st))
      put st
        { stMode          = Finished
        , stBanner        = Just (T.pack (srMessage result))
        , stHasMore       = srHasMore result
        , stSubmitDetails = srDetails result
        }

handleFinished :: V.Event -> EventM Name AppState ()
handleFinished ev =
  case ev of
    V.EvKey (V.KChar 'q') [V.MCtrl] -> halt
    V.EvKey V.KEsc []               -> halt
    V.EvKey (V.KChar 'u') [V.MCtrl] ->
      modify $ \st -> st { stOverlay = UserInfo }
    V.EvKey (V.KChar 'v') [V.MCtrl] ->
      modify $ \st -> st { stOverlay = ReviewSchedule }
    V.EvKey (V.KChar 's') [V.MCtrl] -> do
      st <- get
      case stBanner st of
        Nothing -> put st { stMode = ConfirmSubmit }
        Just _  -> pure ()
    V.EvKey (V.KChar 'n') [V.MCtrl] -> do
      st <- get
      if stHasMore st
        then put st { stWantsMore = True } >> halt
        else pure ()
    V.EvKey V.KUp []         -> vScrollBy (viewportScroll DoneViewport) (-1)
    V.EvKey V.KDown []       -> vScrollBy (viewportScroll DoneViewport) 1
    V.EvKey (V.KChar 'k') [] -> vScrollBy (viewportScroll DoneViewport) (-1)
    V.EvKey (V.KChar 'j') [] -> vScrollBy (viewportScroll DoneViewport) 1
    _                               -> pure ()

handleNormal :: V.Event -> EventM Name AppState ()
handleNormal ev =
  case ev of
    V.EvKey (V.KChar 'q') [V.MCtrl] ->
      halt

    V.EvKey (V.KChar 'o') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  -> put (advanceOverride q st)

    V.EvKey (V.KChar 'r') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  -> put (requeueOnly q st)

    V.EvKey (V.KChar 'p') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Just q | hasAudio q st -> liftIO $ playAudio (stAudioPlayer st) (qSubject q)
        _ -> pure ()

    V.EvKey (V.KChar 'a') [V.MCtrl] ->
      modify $ \st -> st { stOverlay = AllInfo }
    V.EvKey (V.KChar 'u') [V.MCtrl] ->
      modify $ \st -> st { stOverlay = UserInfo }
    V.EvKey (V.KChar 'v') [V.MCtrl] ->
      modify $ \st -> st { stOverlay = ReviewSchedule }

    V.EvKey V.KEsc [] ->
      halt

    V.EvKey V.KEnter [] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  ->
          let ans = T.strip (stInput st)
          in if T.null ans then pure () else put (submitAnswer q ans st)

    V.EvKey V.KBS [] -> do
      st <- get
      put st
        { stInput = if T.null (stInput st) then T.empty else T.init (stInput st)
        , stMode  = Normal
        }

    V.EvKey V.KDel [] -> do
      st <- get
      put st
        { stInput = if T.null (stInput st) then T.empty else T.init (stInput st)
        , stMode  = Normal
        }

    V.EvKey (V.KChar c) [] -> do
      st <- get
      put st
        { stInput = stInput st <> T.singleton c
        , stMode  = Normal
        }

    _ -> pure ()

--------------------------------------------------------------------------------
-- Session logic
--------------------------------------------------------------------------------

currentQuestion :: AppState -> Maybe Q
currentQuestion st =
  case stQueue st of
    []    -> Nothing
    (q:_) -> Just q

submitAnswer :: Q -> Text -> AppState -> AppState
submitAnswer q answer st =
  let (ok, expected) = checkAnswer q answer
  in if ok
       then advanceCorrect q st
       else st
          { stInput = T.empty
          , stMode  = WrongAnswer answer expected
          }

advanceCorrect :: Q -> AppState -> AppState
advanceCorrect q st =
  let prog'  = markOk (qSubject q) (qKind q) (stProgress st)
      queue' = drop 1 (stQueue st)
  in st
     { stQueue       = queue'
     , stQueueWidget = mkQueueWidget queue'
     , stProgress    = prog'
     , stCorrect     = stCorrect st + 1
     , stInput       = T.empty
     , stMode        = if null queue' then Finished else Feedback "✓"
     }

advanceOverride :: Q -> AppState -> AppState
advanceOverride q st =
  let prog'  = markOk (qSubject q) (qKind q) (stProgress st)
      queue' = drop 1 (stQueue st)
  in st
     { stQueue       = queue'
     , stQueueWidget = mkQueueWidget queue'
     , stProgress    = prog'
     , stCorrect     = stCorrect st + 1
     , stOverridden  = stOverridden st + 1
     , stInput       = T.empty
     , stMode        = if null queue' then Finished else Feedback "override"
     }

requeueWrong :: Q -> AppState -> AppState
requeueWrong q st =
  let prog'  = incWrong (qSubject q) (qKind q) (stProgress st)
      queue' = requeueAfterK (stRequeueAfter st) q (drop 1 (stQueue st))
  in st
     { stQueue       = queue'
     , stQueueWidget = mkQueueWidget queue'
     , stProgress    = prog'
     , stWrong       = stWrong st + 1
     , stInput       = T.empty
     , stMode        = Feedback "requeued"
     }

-- | Requeue without recording a wrong answer (no penalty to wrong counts).
requeueOnly :: Q -> AppState -> AppState
requeueOnly q st =
  let queue' = requeueAfterK (stRequeueAfter st) q (drop 1 (stQueue st))
  in st
     { stQueue       = queue'
     , stQueueWidget = mkQueueWidget queue'
     , stInput       = T.empty
     , stMode        = Feedback "requeued"
     }

requeueAfterK :: Int -> Q -> [Q] -> [Q]
requeueAfterK k q qs =
  let k' = max 0 k
      (front, back) = splitAt k' qs
  in front ++ [q] ++ back

mkQueueWidget :: [Q] -> L.List Name Q
mkQueueWidget qs =
  L.list QueueList (Vec.fromList qs) 1

--------------------------------------------------------------------------------
-- Progress / submissions
--------------------------------------------------------------------------------

markOk :: Api.Subject -> QKind -> M.Map Int Progress -> M.Map Int Progress
markOk subj kind mp =
  let sid = Api.subjId subj
  in M.adjust upd sid mp
  where
    upd p =
      case kind of
        QMeaning -> p { pMeaningOk = True }
        QReading -> p { pReadingOk = True }

incWrong :: Api.Subject -> QKind -> M.Map Int Progress -> M.Map Int Progress
incWrong subj kind mp =
  let sid = Api.subjId subj
  in M.adjust upd sid mp
  where
    upd p =
      case kind of
        QMeaning -> p { pMeaningWrong = pMeaningWrong p + 1 }
        QReading -> p { pReadingWrong = pReadingWrong p + 1 }

mkSubmissions :: AppState -> [Submission]
mkSubmissions st =
  [ Submission
      { subAssignmentId = asgId
      , subWrongMeaning = pMeaningWrong p
      , subWrongReading = pReadingWrong p
      }
  | (sid, p) <- M.toList (stProgress st)
  , Just asgId <- [M.lookup sid (stSubjToAsg st)]
  ]

--------------------------------------------------------------------------------
-- Setup helpers
--------------------------------------------------------------------------------

mkQuestions :: Api.Subject -> [Q]
mkQuestions s =
  let rs = acceptedReadings s
  in Q s QMeaning
     : [ Q s QReading
       | Api.subjType s /= Api.Radical
       , not (null rs)
       ]

initProgress :: Api.Subject -> Progress
initProgress s =
  let needsReading =
        Api.subjType s /= Api.Radical
        && not (null (acceptedReadings s))
  in Progress False needsReading False 0 0

acceptedReadings :: Api.Subject -> [Text]
acceptedReadings s =
  filter (not . T.null . T.strip) (Api.subjReadings s)

shuffle :: [a] -> IO [a]
shuffle xs = go xs []
  where
    go [] acc = pure acc
    go ys acc = do
      i <- randomRIO (0, length ys - 1)
      case splitAt i ys of
        (front, a:back) -> go (front ++ back) (a : acc)
        _               -> pure acc

--------------------------------------------------------------------------------
-- Answer checking / display
--------------------------------------------------------------------------------

checkAnswer :: Q -> Text -> (Bool, [String])
checkAnswer (Q subj kind) ans =
  case kind of
    QMeaning ->
      let acceptedNorm = map normMeaning (Api.subjMeanings subj)
      in ( normMeaning ans `elem` acceptedNorm
         , map T.unpack (Api.subjMeanings subj)
         )
    QReading ->
      let rs = acceptedReadings subj
          acceptedNorm = map normReading rs
      in ( normReading ans `elem` acceptedNorm
         , map T.unpack rs
         )


displayItem :: Api.Subject -> String
displayItem s =
  let tag =
        case Api.subjType s of
          Api.Kanji          -> " (Kanji)"
          Api.Radical        -> " (Radical)"
          Api.Vocabulary     -> " (Vocab)"
          Api.KanaVocabulary -> " (Vocab)"
      core =
        case Api.subjChars s of
          Just c | let cs = T.strip c, not (T.null cs) -> T.unpack cs
          _ ->
            let m = case Api.subjMeanings s of
                      (x:_) -> T.unpack x
                      []    -> "?"
            in m <> " (#" <> show (Api.subjId s) <> ")"
  in core <> tag

kindLabel :: QKind -> String
kindLabel QMeaning = "meaning"
kindLabel QReading = "reading"

displayInput :: QKind -> Text -> Text
displayInput QReading t = Romaji.romajiToHiraganaLive t
displayInput QMeaning t = t

hasAudio :: Q -> AppState -> Bool
hasAudio q st =
  not (null (Api.subjAudioUrls (qSubject q))) && isJust (stAudioPlayer st)

-- | Render a list of hint strings as auto-wrapping text.
hintBox :: [Text] -> Widget Name
hintBox hints =
  withAttr (attrName "hint") $
    txtWrap (T.intercalate "  " hints)

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

-- | Fire-and-forget audio playback via configured external player.
playAudio :: Maybe String -> Api.Subject -> IO ()
playAudio Nothing _ = pure ()
playAudio (Just cmd) subj =
  case Api.subjAudioUrls subj of
    [] -> pure ()
    urls -> do
      i <- randomRIO (0, length urls - 1)
      let url         = urls !! i
          (exe, args) = case words cmd of
                          []     -> ("mpv", [])
                          (w:ws) -> (w, ws)
      void $ spawnProcess exe (args ++ [T.unpack url])

normMeaning :: Text -> Text
normMeaning = collapseSpaces . britishToAmerican . T.toCaseFold . T.strip

-- | Convert British English spellings to American English, word by word.
-- Applied after case-folding so all lookups are lowercase.
britishToAmerican :: Text -> Text
britishToAmerican = T.unwords . map convertWord . T.words
  where
    convertWord w = M.findWithDefault (applySuffixRules w) w wordTable

    -- Word-pair table for cases that don't follow simple suffix rules.
    -- All keys must be lowercase (applied after toCaseFold).
    wordTable :: M.Map Text Text
    wordTable = M.fromList $
         re "centre"   "center"
      ++ re "theatre"  "theater"
      ++ re "fibre"    "fiber"
      ++ re "litre"    "liter"
      ++ re "metre"    "meter"
      ++ re "spectre"  "specter"
      ++ re "sabre"    "saber"
      ++ re "calibre"  "caliber"
      ++ re "lustre"   "luster"
      ++ re "sombre"   "somber"
      ++ [ ("defence",  "defense"),  ("defences",  "defenses")
         , ("offence",  "offense"),  ("offences",  "offenses")
         , ("pretence", "pretense"), ("pretences", "pretenses")
         , ("licence",  "license"),  ("licences",  "licenses")
         , ("practise", "practice")
         ]
      where
        re b a = [(b, a), (b <> "s", a <> "s")]

    applySuffixRules w
      | "iour"    `T.isSuffixOf` w                  = T.dropEnd 4 w <> "ior"
      | "our"     `T.isSuffixOf` w
      , w `notElem` ourBlacklist                     = T.dropEnd 3 w <> "or"
      | "yse"     `T.isSuffixOf` w                  = T.dropEnd 3 w <> "yze"
      | "isation" `T.isSuffixOf` w                  = T.dropEnd 7 w <> "ization"
      | "ise"     `T.isSuffixOf` w
      , w `notElem` iseBlacklist                     = T.dropEnd 3 w <> "ize"
      | "ogue"    `T.isSuffixOf` w
      , w `notElem` ogueBlacklist                    = T.dropEnd 4 w <> "og"
      | otherwise                                    = w

    ourBlacklist =
      [ "four", "pour", "hour", "your", "sour", "dour", "tour", "flour"
      , "amour", "contour", "detour", "velour", "troubadour", "paramour" ]

    iseBlacklist =
      [ "rise", "wise", "guise", "surprise", "revise", "advise", "devise"
      , "enterprise", "exercise", "franchise", "improvise", "promise"
      , "supervise", "advertise", "comprise", "disguise", "arise"
      , "otherwise", "likewise", "clockwise", "lengthwise"
      , "prise", "demise", "surmise", "premise", "treatise"
      , "precise", "concise"
      , "noise", "poise", "turquoise", "tortoise", "porpoise" ]

    ogueBlacklist =
      [ "rogue", "vogue", "pirogue", "brogue" ]

normReading :: Text -> Text
normReading t =
  let t' = T.strip t
  in if T.all (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '\'') t'
       then Romaji.romajiToHiragana (T.toCaseFold t')
       else T.toCaseFold t'

collapseSpaces :: Text -> Text
collapseSpaces =
  T.unwords . filter (not . T.null) . T.words
