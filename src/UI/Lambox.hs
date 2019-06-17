{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- TODO Reorganize everything

module UI.Lambox
  ( module UI.Lambox
  , Config(..)
  --, Box(..)
  , Borders(..)
  , BoxAttribute(..)
  , AlignV(..)
  , AlignH(..)
  , Direction(..)
  , Axis(..)
  , Event(..)
  , Curses
  , CursorMode(..)
  , setCursorMode
  ) where --remember to export relevant ncurses stuff as well (like events, curses, glyphs, etc)

import Data.List (sort)

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
newBox conf@Config{..} = do
  win <- newWindow configHeight configWidth configY configX
  pan <- newPanel win
  updateWindow win $ do
    configureAttrs $ sort configAttrs
  refreshPanels
  pure $ Box conf win pan
  where
    configureAttrs :: [BoxAttribute] -> Update () --ASSUMES LIST IS SORTED
    configureAttrs [] = pure ()
    configureAttrs (a:as) = configAttribute a >> configureAttrs as
    
    configAttribute :: BoxAttribute -> Update ()
    configAttribute = \case
      (Borders border) -> case border of
        Line -> drawBox (Just glyphLineV) (Just glyphLineH)
        Hash -> drawBorder (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple)
        _ -> drawBox (Just glyphLineV) (Just glyphLineH) -- TODO: Complete
      (Title title hAlign vAlign) -> do
        let vert = case vAlign of
              AlignLeft -> 1
              AlignCenter -> (configWidth `quot` 2) - ((toInteger $ length title) `quot` 2)
              AlignRight -> (configWidth-1) - (toInteger $ length title)
            horz = case hAlign of
              AlignTop -> 0
              AlignBot -> configHeight-1
        moveCursor horz vert >> drawString title
      _ -> pure ()




-- updateBox :: Box -> Curses a -> Curses ()


-- | Delete panel
deleteBox :: Box -> Curses ()
deleteBox (Box _ win pan) = deletePanel pan >> closeWindow win

{-
deleteBoxes :: [Box] -> Curses ()
deleteBoxes = foldMap deleteBox -}

-- | Literally just a synonym for render
update :: Curses ()
update = refreshPanels >> render

-- | Start the program
lambox :: Curses a -> IO a
lambox f = runCurses (setEcho False >> setCursorMode CursorInvisible >> f)

-- TODO :: default configs like full(screen), up half, down third, etc, using direction and ratio
-- config :: Direction -> Ratio -> Config

-- | Take a box and a pair of local coordinates and print a string within it
writeStr :: Box -> Integer -> Integer -> String -> Curses ()
writeStr (Box conf win pan) x y str = updateWindow win $ do
  moveCursor y x
  drawString str

-- | Like writeStr but with any showable type
writeShow :: Show a => Box -> Integer -> Integer -> a -> Curses ()
writeShow box x y = ((writeStr box x y) . show)

-- | Take a box and split it into two boxes, returning the new
-- box and altering the passed box as a side effect (CAUTION!)
-- The direction determines where the new box is in relation
-- to the passed box, and the fraction is the ratio of the
-- length or width of the new box to the old.
-- Because this function basically reintroduces more manual
-- memory management, it is not something that should be used.
unsafeSplitBox :: RealFrac a => Box -> Direction -> a -> [BoxAttribute] -> Curses (Box, Box) -- return Curses (Box, Box) (oldbox, newbox) with updated config settings
unsafeSplitBox (Box Config{..} win pan) dir ratio attrs = do
  case dir of
    _ -> do -- DirUp
      let nHeight2 = ratioIF configHeight ratio
          nHeight1 = configHeight - nHeight2
          nuY2 = configY
          nuY1 = configY + nHeight2
      updateWindow win $ do
        resizeWindow nHeight1 configWidth -- have to redraw borders and title, to fix that bottom border bug
        moveWindow nuY1 configX
      let nConfig = Config configX nuY2 configWidth nHeight2 attrs
      box2 <- newBox nConfig
      let nbox1 = Box (Config configX nuY1 configWidth nHeight1 configAttrs) win pan
      pure (nbox1,box2)

-- | Take a config for an area then given the axis and a decimal,
-- split the area into two boxes. The decimal is that ratio between
-- the respective dimensions of the first box and second box.
splitBox :: RealFrac a => Config -> Axis -> a -> Curses (Box,Box)
splitBox Config{..} axis ratio = splitBox' configX configY configWidth configHeight configAttrs configAttrs axis ratio


-- | Take x and y coordinates, dimensions, axis, ratio, and
-- two attribute lists to split the area into two boxes, configuring
-- each box to the respective attributes (top/left takes first one)
splitBox' :: RealFrac a => Integer -> Integer -> Integer -> Integer -> [BoxAttribute] -> [BoxAttribute] -> Axis -> a -> Curses (Box,Box)
splitBox' x y width height attrs1 attrs2 axis ratio = do
  (conf1, conf2) <- case axis of
    Horizontal -> do
      let width1 = ratioIF width ratio
          width2 = width - width1
          x1 = x
          x2 = x + width1
      pure
        ( Config x1 y width1 height attrs1
        , Config x2 y width2 height attrs2
        )
    Vertical -> do
      let height1 = ratioIF height ratio
          height2 = height - height1
          y1 = y
          y2 = y + height1
      pure
        ( Config x y1 width height1 attrs1
        , Config x y2 width height2 attrs2
        )
  box1 <- newBox conf1
  box2 <- newBox conf2
  pure (box1,box2)


      {-
      protobox <- newBox (Config x y width height attrs1)
      unsafeSplitBox protobox DirDown ratio attrs2
      -- newBox (Config x y width height attrs1) >>= \protobox -> unsafeSplitBox protobox DirDown ratio attrs2
      -}
      {-
      let height1 = ratioIF height ratio
          height2 = height - height1
          y1 = y
          y2 = y + height1
      box1 <- newBox $ Config x y1 width height1 attrs1
      box2 <- newBox $ Config x y2 width height2 attrs2
      pure (box1,box2)
      -}

{-
(y,x,height,width) <- updateWindow win $ do --use config instead
    (y,x) <- windowPosition
    (height,width) <- windowSize
    pure (y,x,height,width)
-}

{-
setBorders :: Box -> Borders -> Curses Box
setBorders (Box Congig{..} win pan) cBorders = updateWindow win $ do
  case cBorders of
      Line -> drawBox (Just glyphLineV) (Just glyphLineH)
      Hash -> drawBorder (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple) (Just glyphStipple)
      _ -> drawBox (Just glyphLineV) (Just glyphLineH) -- TODO: Complete

setTitle :: Box -> Maybe Title -> Curses Box
setTitle box@(Box Config{..} win pan) cTitle = do
  setBorders box configBorders
  updateWindow win $ do
  case cTitle of
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
  where
    configBorders = -- ...get borders from configAttrs
-}
