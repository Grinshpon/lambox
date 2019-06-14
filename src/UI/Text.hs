{-# LANGUAGE RecordWildCards #-}

module UI.Text
  ( testRender --to be taken out
  , onEvent
  , onEvent'
  , onEventGlobal
  , waitFor
  , newPanel
  , deletePanel
  , initTui
  , update
  , Box(..)
  , Panel(..)
  , Borders(..)
  , Event(..)
  , Curses
  ) where --remember to export relevant ncurses stuff as well (like events, curses, glyphs, etc)

import UI.NCurses
import qualified UI.NCurses.Panel as P

import UI.Internal.Types

--TODO:
-- Boxes/Panels
-- |- borders (dash '-' '|' or hash '#' or dot '*' ******** and thickness ======= or plus '+' (or other char)
-- |- gaps                                         *      *               ||   ||
-- |- dimension                                    ********               =======
-- |- ordering
-- |- position
-- Widgets
-- |- scroll
-- |- text input
-- |- check/radial boxes
-- |- tabs
-- Updaters

-- | A test render
testRender :: IO ()
testRender = runCurses $ do
  setEcho False
  w <- defaultWindow
  updateWindow w $ do
    moveCursor 1 10
    drawString "Hello world!"
    moveCursor 3 10
    dim <- windowSize
    drawString $ show dim
    moveCursor 5 10
    drawString "(press q to quit)"
    moveCursor 0 0
  nw <- newWindow 12 12 1 30
  pan <- P.newPanel nw
  updateWindow nw $ do
    moveCursor 1 1
    drawString "Panel here"
    drawBox (Just glyphLineV) (Just glyphLineH)
  render
  waitFor w (== EventCharacter 'd')
  P.movePanel pan 10 30
  P.refreshPanels
  render
  testRenderLoop w
  P.deletePanel pan
  closeWindow nw
  closeWindow w

testRenderWindow :: Window -> Curses ()
testRenderWindow w = do
  P.refreshPanels
  updateWindow w $ do
    moveCursor 3 10
    dim <- windowSize
    drawString $ show dim
    moveCursor 0 0
  render

testRenderLoop :: Window -> Curses ()
testRenderLoop w = do
  testRenderWindow w
  onEvent w (\ev -> ev /= EventCharacter 'q' && ev /= EventCharacter 'Q') (testRenderLoop w)

-- | Wait for condition to be met before continuing
waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor window p = go where
  go = do
    event <- getEvent window Nothing
    case event of
      Nothing -> go
      Just ev -> if p ev then pure () else go

-- | Similar to onEvent, but does not \'consume\' event
onEvent' :: Maybe Event -> (Event -> Bool) -> Curses a -> Curses ()
onEvent' (Just event) p action = if p event then action >> pure () else pure ()
onEvent' Nothing _ _           = pure ()

-- | Perform action if event passed meets the event condition, else do nothing
onEvent :: Window {- replace with Panel -} -> (Event -> Bool) -> Curses a -> Curses ()
onEvent window p action = getEvent window Nothing >>= \event -> onEvent' event p action

-- | Similar to onEvent but happens on any event within the default window
onEventGlobal :: (Event -> Bool) -> Curses a -> Curses ()
onEventGlobal p action = do
  def <- defaultWindow
  getEvent def Nothing >>= \event -> onEvent' event p action

-- | Create a panel given a Box type. Because the tui panel uses NCurses'
-- Panel and Window type, it cannot be garbage collected and must be
-- deleted manually with `deletePanel`
newPanel :: Show a => Box a -> Curses Panel
newPanel Box{..} = do
  win <- newWindow height width y x
  pan <- P.newPanel win
  updateWindow win $ case borders of
    Line -> drawBox (Just glyphLineV) (Just glyphLineH)
    Hash -> drawBorder (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple)
    _ -> drawBox (Just glyphLineV) (Just glyphLineH) -- TODO: Complete
  P.refreshPanels
  pure $ Panel win pan

-- updatePanel :: Panel -> Curses a -> Curses ()

-- | Delete panel
deletePanel :: Panel -> Curses ()
deletePanel (Panel win pan) = P.deletePanel pan >> closeWindow win

-- | Literally just a synonym for render
update :: Curses ()
update = render

initTui :: Curses a -> IO a
initTui f = runCurses (setEcho False >> f)
