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
import Brick.Util (fg, on)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map.Strict as M
import qualified Data.Vector as Vec
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)
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

data Name = QueueList
  deriving (Ord, Eq, Show)

data Mode
  = Normal
  | WrongAnswer [String]
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
  }

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

runStudyTui :: Int -> M.Map Int Int -> [Api.Subject] -> ([Submission] -> IO SubmitResult) -> IO Bool
runStudyTui rqAfter subjToAsg subjects submitFn = do
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
        , stMode         = Normal
        , stBanner       = Nothing
        , stHasMore      = False
        , stWantsMore    = False
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
  , (attrName "hint",    fg V.brightBlack)
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
drawMain st =
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
              bannerWidgets =
                case stBanner st of
                  Just msg -> [padTop (Pad 1) (txt msg)]
                  Nothing  -> []
              hintLine =
                case stMode st of
                  ConfirmSubmit -> str "y/Enter=confirm  n/Esc=cancel"
                  _ | Just _ <- stBanner st ->
                        if stHasMore st
                          then str "Ctrl-n=next batch  Esc=quit"
                          else str "Esc=quit"
                  _ -> str "Ctrl-s=submit to WaniKani  Esc=quit"
          in vBox
               ( [ withAttr (attrName "ok") $ str "Session finished."
                 , str ("correct:     " <> show (stCorrect st))
                 , str ("wrong:       " <> show (stWrong st))
                 , str ("overridden:  " <> show (stOverridden st))
                 , str ("submissions: " <> show (length (mkSubmissions st)))
                 ]
              ++ confirmWidgets
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
                  str "Enter=submit answer  Ctrl-o=override  Ctrl-b=requeue  Ctrl-s=submit batch  Esc=quit"
            ]

drawMode :: AppState -> Q -> Widget Name
drawMode st q =
  case stMode st of
    Normal ->
      emptyWidget

    WrongAnswer expected ->
      vBox
        [ withAttr (attrName "bad") $
            txt ("✗ accepted: " <> T.pack (intercalate ", " expected))
        , padTop (Pad 1) $
            withAttr (attrName "hint") $
              str "Ctrl-o=override as correct  Ctrl-b=requeue later  Enter=requeue"
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

--------------------------------------------------------------------------------
-- Event handling
--------------------------------------------------------------------------------

handleEvent :: ([Submission] -> IO SubmitResult) -> BrickEvent Name e -> EventM Name AppState ()
handleEvent submitFn (VtyEvent ev) = do
  st <- get
  case stMode st of
    WrongAnswer expected -> handleWrongAnswer expected ev
    ConfirmSubmit        -> handleConfirm submitFn ev
    Finished             -> handleFinished ev
    _                    -> handleNormal ev
handleEvent _ _ = pure ()

handleWrongAnswer :: [String] -> V.Event -> EventM Name AppState ()
handleWrongAnswer _ ev =
  case ev of
    V.EvKey (V.KChar 'o') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  -> put (advanceOverride q st { stMode = Normal })

    V.EvKey (V.KChar 'b') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  -> put (requeueWrong q st { stMode = Normal })

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
        { stMode    = Finished
        , stBanner  = Just (T.pack (srMessage result))
        , stHasMore = srHasMore result
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

    V.EvKey (V.KChar 'b') [V.MCtrl] -> do
      st <- get
      case currentQuestion st of
        Nothing -> pure ()
        Just q  -> put (requeueWrong q st)

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
          , stMode  = WrongAnswer expected
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
      let (front, a:back) = splitAt i ys
      go (front ++ back) (a : acc)

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

acceptedPreview :: Q -> Text
acceptedPreview (Q subj kind) =
  case kind of
    QMeaning -> T.intercalate ", " (Api.subjMeanings subj)
    QReading -> T.intercalate ", " (acceptedReadings subj)

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
