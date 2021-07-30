{-# LANGUAGE OverloadedStrings #-}

module Main where

import App

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center

import Data.List (elemIndex, delete)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import qualified Graphics.Vty as V

-- Setup
type Name = Cursor

data Cursor = HeadlineCursor
            | ButtonCursor
            | TextCursor Text
            deriving (Show, Eq, Ord)

data State = State { options :: [Text]
                   , cursor :: Cursor
                   , prevCursor :: Maybe Cursor
                   , selectedOptions :: [Text]
                   }

data Tick = Tick

app :: App State Tick Name
app = App { appDraw         = return . displayTui -- Single layer
          , appChooseCursor = showFirstCursor -- Only one cursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const attributes
          }

startCursor :: Cursor
startCursor = HeadlineCursor

noneSelected :: [Text] -> State
noneSelected ts = State ts startCursor Nothing []

allSelected :: [Text] -> State
allSelected ts = State ts startCursor Nothing ts

initialState :: State
initialState = noneSelected listOptions

-- Display
main :: IO ()
main = do
  s <- defaultMain app initialState
  print $ selectedOptions s

displayOptions :: State -> Widget Name
displayOptions s = border $ vLimit 3 $ displayText s (options s)
  where
    displayText :: State -> [Text] -> Widget Name
    displayText _ [] = emptyWidget
    displayText state (x:xs)
      | null xs   = optionName state x
      | otherwise = optionName state x <+> vBorder <+> displayText state xs
    optionName state t = padOptionName $ if t `elem` selectedOptions state
                                         then withAttr selectedAttr $ cursorText state (TextCursor t) t
                                         else cursorText state (TextCursor t) t
    padOptionName = padLeftRight 3 . padTopBottom 1

displayTui :: State -> Widget Name
displayTui s = withBorderStyle unicode $ borderWithLabel (cursorText s HeadlineCursor "")
  $ hCenter (displayOptions s) <=>  hCenter (enterButton s)

enterButton :: State -> Widget Name
enterButton s = border $ cursorText s ButtonCursor "Run"

cursorText :: State -> Cursor -> Text -> Widget Name
cursorText s c t = if c == cursor s
                   then showCursor c (Location (0,0)) (txt t)
                   else txt t

-- Update state
selectNextOption :: State -> State
selectNextOption state = case cursor state of
                           TextCursor t -> state { cursor = nextOption t state}
                           _            -> state
  where
    nextOption t s = maybe (cursor s)
                            (\i -> TextCursor $ options s !! (if i < length (options s) - 1 then i+1 else i))
                            (elemIndex t (options s))

selectPrevOption :: State -> State
selectPrevOption state = case cursor state of
                       TextCursor t -> state { cursor = prevSelection t state }
                       _            -> state
  where
    prevSelection t s = maybe (cursor s)
                              (\i -> TextCursor $ options s !! (if i >= 1 then i - 1 else i))
                              (elemIndex t (options s))

handleSelect :: State -> EventM Name (Next State)
handleSelect s = case cursor s of
                   TextCursor t -> continue $ s { selectedOptions = updatedOptions t }
                   ButtonCursor -> halt s -- Pass the final state on to the rest of the program
                   _            -> continue s
  where
    updatedOptions t = if t `elem` selectedOptions s
                       then delete t (selectedOptions s)
                       else t : selectedOptions s

data Movement = UpRow
              | DownRow

runCommand :: State -> State
runCommand = id

selectTextCursor :: State -> Cursor -> Cursor
selectTextCursor s defaultCursor =
  if not (null (options s))
  then fromMaybe (TextCursor (head (options s))) (prevCursor s)
  else defaultCursor

changeRow :: State -> Movement -> State
changeRow s DownRow =
  case cursor s of
    HeadlineCursor -> s { cursor = selectTextCursor s ButtonCursor }
    TextCursor _ -> s { cursor = ButtonCursor
                      , prevCursor = Just (cursor s)
                      }
    ButtonCursor -> s { cursor = HeadlineCursor }

changeRow s UpRow =
  case cursor s of
    HeadlineCursor -> s { cursor = ButtonCursor }
    TextCursor _ -> s { cursor = HeadlineCursor
                      , prevCursor = Just (cursor s)
                      }
    ButtonCursor -> s { cursor = selectTextCursor s HeadlineCursor }

handleEvent :: State -> BrickEvent Name Tick -> EventM Name (Next State)
handleEvent s (VtyEvent (V.EvKey V.KRight []))      = continue $ selectNextOption s
handleEvent s (VtyEvent (V.EvKey V.KLeft []))       = continue $ selectPrevOption s
handleEvent s (VtyEvent (V.EvKey V.KUp []))         = continue $ changeRow s UpRow
handleEvent s (VtyEvent (V.EvKey V.KDown []))       = continue $ changeRow s DownRow
handleEvent s (VtyEvent (V.EvKey V.KEnter []))      = handleSelect s
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey V.KEsc []))        = halt s
handleEvent s _                                     = continue s

-- Attributes
attributes :: AttrMap
attributes = attrMap V.defAttr
  [ (selectedAttr, V.currentAttr `V.withStyle` V.bold `V.withStyle` V.standout `V.withStyle` V.underline)
  ]

selectedAttr :: AttrName
selectedAttr = "selectedAttr"
