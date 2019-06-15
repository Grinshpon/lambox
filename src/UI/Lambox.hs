{-# LANGUAGE RecordWildCards #-}

-- TODO Reorganize everything

module UI.Lambox
  ( module UI.Lambox
  , Config(..)
  --, Box(..)
  , Borders(..)
  , Title(..)
  , AlignV(..)
  , AlignH(..)
  , Event(..)
  , Curses
  , CursorMode(..)
  , setCursorMode
  ) where --remember to export relevant ncurses stuff as well (like events, curses, glyphs, etc)

import UI.NCurses
import UI.NCurses.Panel

import UI.Internal.Types

--TODO:
-- Boxes/Panels
-- |- borders (dash '-' '|' or hash '#' or dot '*' ******** or plus '+' (or other char)
-- |- gaps                                         *      * 
-- |- dimension                                    ********
-- |- ordering
-- |- position
-- Widgets
-- |- scroll
-- |- text input
-- |- check/radial boxes
-- |- tabs
-- Updaters

{-
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
  pan <- newPanel nw
  updateWindow nw $ do
    moveCursor 1 1
    drawString "Panel here"
    drawBox (Just glyphLineV) (Just glyphLineH)
  render
  waitFor w (== EventCharacter 'd')
  movePanel pan 10 30
  refreshPanels
  render
  testRenderLoop w
  deletePanel pan
  closeWindow nw
  closeWindow w

testRenderWindow :: Window -> Curses ()
testRenderWindow w = do
  refreshPanels
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
-}

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
onEvent :: Window {- replace with Box -} -> (Event -> Bool) -> Curses a -> Curses ()
onEvent window p action = getEvent window Nothing >>= \event -> onEvent' event p action

-- | Similar to onEvent but happens on any event within the default window
onEventGlobal :: (Event -> Bool) -> Curses a -> Curses ()
onEventGlobal p action = do
  def <- defaultWindow
  getEvent def Nothing >>= \event -> onEvent' event p action

-- | Create a panel given a Box type. Because the tui panel uses NCurses'
-- Panel and Window type, it cannot be garbage collected and must be
-- deleted manually with `deletePanel`
newBox :: Config -> Curses Box
newBox Config{..} = do
  win <- newWindow configHeight configWidth configY configX
  pan <- newPanel win
  updateWindow win $ do
    case configBorders of
      Line -> drawBox (Just glyphLineV) (Just glyphLineH)
      Hash -> drawBorder (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple)
      _ -> drawBox (Just glyphLineV) (Just glyphLineH) -- TODO: Complete
    case configTitle of
      Nothing -> pure ()
      Just (Title title vAlign hAlign) -> do
        let vert = case vAlign of
              AlignLeft -> 1
              AlignCenter -> (configWidth `quot` 2) - ((toInteger $ length title) `quot` 2)
              AlignRight -> (configWidth-1) - (toInteger $ length title)
            horz = case hAlign of
              AlignTop -> 0
              AlignBot -> configHeight-1
        moveCursor horz vert >> drawString title
  refreshPanels
  pure $ Box win pan

-- updateBox :: Box -> Curses a -> Curses ()

-- | Delete panel
deleteBox :: Box -> Curses ()
deleteBox (Box win pan) = deletePanel pan >> closeWindow win

-- | Literally just a synonym for render
update :: Curses ()
update = refreshPanels >> render

-- | Start the program
lambox :: Curses a -> IO a
lambox f = runCurses (setEcho False >> f)

-- TODO :: default configs like full(screen), up half, down third, etc, using direction and ratio
-- config :: Direction -> Ratio -> Config

-- | Take a box and a pair of local coordinates and print a string within it
writeStr :: Box -> Integer -> Integer -> String -> Curses ()
writeStr (Box win pan) x y str = updateWindow win $ do
  moveCursor y x
  drawString str

-- | Like writeStr but with any showable type
writeShow :: Show a => Box -> Integer -> Integer -> a -> Curses ()
writeShow box x y = ((writeStr box x y) . show)
