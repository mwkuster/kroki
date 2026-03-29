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

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Maybe (mapMaybe)
import qualified Data.Vector as Vec
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
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

data Name = QueueList | InfoViewport
  deriving (Ord, Eq, Show)

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
  , stAudioPlayer   :: Maybe String         -- command to play audio (e.g. "mpv --really-quiet")
  , stSubmitDetails :: [String]             -- per-submission lines shown after submit
  , stShowAllInfo   :: Bool                 -- Ctrl-a overlay
  , stAllSubjects   :: M.Map Int Api.Subject -- full subject map incl. components
  }

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

runStudyTui :: Int -> Maybe String -> M.Map Int Api.Subject -> M.Map Int Int -> [Api.Subject] -> ([Submission] -> IO SubmitResult) -> IO Bool
runStudyTui rqAfter audioPlayer allSubjects subjToAsg subjects submitFn = do
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
        , stShowAllInfo   = False
        , stAllSubjects   = allSubjects
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
  [ (L.listAttr,         V.white `on` V.black)
  , (L.listSelectedAttr, V.black `on` V.yellow)
  , (attrName "header",  fg V.cyan)
  , (attrName "ok",      fg V.green)
  , (attrName "bad",     fg V.red)
  , (attrName "hint",    fg V.white)
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
  | stShowAllInfo st =
      case currentQuestion st of
        Just q  -> drawAllInfo q st
        Nothing -> emptyWidget
  | otherwise =
  case currentQuestion st of
    Nothing ->
      B.borderWithLabel (str "Done") $
        padAll 1 $
          let confirmWidgets =
                case stMode st of
                  ConfirmSubmit ->
                    let subs = mkSubmissions st
                        total = length subs
                        withMistakes =
                          length [ () | s <- subs, subWrongMeaning s > 0 || subWrongReading s > 0 ]
                    in [ padTop (Pad 1) $
                           withAttr (attrName "header") $
                             str ("Submit " <> show total <> " reviews to WaniKani? [y/N]")
                       , str ("Items with mistakes: " <> show withMistakes)
                       ]
                  _ -> []
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
                        hintBox $ ["Esc=quit"] ++
                          [ "Ctrl-n=next batch" | stHasMore st ]
                  _ -> hintBox ["Ctrl-s=submit to WaniKani", "Esc=quit"]
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
        , padTop (Pad 1) $
            hintBox $
              [ "Ctrl-o=override correct", "Ctrl-r=requeue (no penalty)"
              , "Ctrl-a=all info", "Enter=requeue (wrong)"
              ] ++ [ "Ctrl-p=play audio" | hasAudio q st ]
        ]

    Feedback msg ->
      withAttr (attrName "ok") $ txt msg

    ConfirmSubmit ->
      let subs = mkSubmissions st
          total = length subs
          withMistakes =
            length
              [ ()
              | s <- subs
              , subWrongMeaning s > 0 || subWrongReading s > 0
              ]
      in vBox
          [ withAttr (attrName "header") $
              str ("Submit " <> show total <> " reviews to WaniKani? [y/N]")
          , padTop (Pad 1) $
              str ("Items with mistakes: " <> show withMistakes)
          ]

    Finished ->
      vBox $
        [ withAttr (attrName "ok") $ str "Finished." ] ++
        case stBanner st of
          Just msg -> [padTop (Pad 1) (txt msg)]
          Nothing  -> []

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
    label = maybe "?" id (Api.subjChars subj)
         <> " · " <> subjTypeLabel (Api.subjType subj)

    compSection =
      let comps = Data.Maybe.mapMaybe (\cid -> M.lookup cid (stAllSubjects st))
                                      (Api.subjComponentIds subj)
      in case comps of
           [] -> []
           cs -> str "Components:"
               : map (\c -> str ("  " <> T.unpack (maybe "?" id (Api.subjChars c))
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
  if stShowAllInfo st
    then handleAllInfo ev
    else case stMode st of
      WrongAnswer _ _ -> handleWrongAnswer ev
      ConfirmSubmit   -> handleConfirm submitFn ev
      Finished        -> handleFinished ev
      _               -> handleNormal ev
handleEvent _ _ = pure ()

handleAllInfo :: V.Event -> EventM Name AppState ()
handleAllInfo ev =
  case ev of
    V.EvKey (V.KChar 'a') [V.MCtrl] -> modify $ \st -> st { stShowAllInfo = False }
    V.EvKey V.KEsc []                -> modify $ \st -> st { stShowAllInfo = False }
    V.EvKey V.KUp []                 -> vScrollBy (viewportScroll InfoViewport) (-1)
    V.EvKey V.KDown []               -> vScrollBy (viewportScroll InfoViewport) 1
    V.EvKey (V.KChar 'k') []         -> vScrollBy (viewportScroll InfoViewport) (-1)
    V.EvKey (V.KChar 'j') []         -> vScrollBy (viewportScroll InfoViewport) 1
    _                                -> pure ()

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
      modify $ \st -> st { stShowAllInfo = True }

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
      put st { stMode = Normal }

    V.EvKey V.KEsc [] -> do
      st <- get
      put st { stMode = Normal }

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
    _                               -> pure ()

handleNormal :: V.Event -> EventM Name AppState ()
handleNormal ev =
  case ev of
    V.EvKey (V.KChar 'q') [V.MCtrl] ->
      halt

    V.EvKey (V.KChar 's') [V.MCtrl] -> do
      st <- get
      put st { stMode = ConfirmSubmit }

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
      modify $ \st -> st { stShowAllInfo = True }

    V.EvKey V.KEsc [] ->
      halt

    V.EvKey V.KEnter [] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  ->
          put (submitAnswer q (T.strip (stInput st)) st)

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
          Just c | not (T.null (T.strip c)) -> T.unpack (T.strip c)
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
  not (null (Api.subjAudioUrls (qSubject q))) && stAudioPlayer st /= Nothing

-- | Render a list of hint strings as auto-wrapping text.
hintBox :: [Text] -> Widget Name
hintBox hints =
  withAttr (attrName "hint") $
    txtWrap (T.intercalate "  " hints)

normalHintWidget :: Q -> AppState -> Widget Name
normalHintWidget q st =
  hintBox $
    [ "Enter=submit", "Ctrl-o=override", "Ctrl-r=requeue"
    , "Ctrl-a=all info", "Esc=quit", "Ctrl-s=submit batch"
    ] ++ [ "Ctrl-p=play audio" | hasAudio q st ]

-- | Fire-and-forget audio playback via configured external player.
playAudio :: Maybe String -> Api.Subject -> IO ()
playAudio Nothing _ = pure ()
playAudio (Just cmd) subj =
  case Api.subjAudioUrls subj of
    [] -> pure ()
    urls -> do
      i <- randomRIO (0, length urls - 1)
      let url   = urls !! i
          parts = case words cmd of { [] -> ["mpv"]; ws -> ws }
          exe   = head parts
          args  = tail parts
      void $ spawnProcess exe (args ++ [T.unpack url])

normMeaning :: Text -> Text
normMeaning = collapseSpaces . T.toCaseFold . T.strip

normReading :: Text -> Text
normReading t =
  let t' = T.strip t
  in if T.all (\c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '\'') t'
       then Romaji.romajiToHiragana (T.toCaseFold t')
       else T.toCaseFold t'

collapseSpaces :: Text -> Text
collapseSpaces =
  T.unwords . filter (not . T.null) . T.words
