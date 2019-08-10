{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}

-- TODO Reorganize everything

-- | Main module, also re-exports any types from UI.Lambox.Internal.Types that are needed.
--
-- Much of this library acts within the @ncurses@ 'Curses' monad, which in itself is a wrapper around 'IO'.
-- The purpose of this library is to provide a higher level wrapper around @ncurses@ to remove much of the
-- boilerplate required to set up a tui, and to make building a tui a bit simpler. This library is meant to
-- be used in conjuction with @ncurses@, as importing UI.NCurses is required to use the 'Event' type, for
-- instance.
module UI.Lambox
  ( -- * LamBox
    lambox
  , update
    -- * Box
  , newBox
  , splitBox
  , splitBox'
  , splitFromBox
  , deleteBox
  , deleteBoxes
    -- * Accessing Boxes
  , getPosition
  , getDimension
  , getAttributes
  , getBoxContents
    -- * Modifying Boxes
  , withBox
  , setBoxAttributes
  , setBoxConfig
  , setBorders
  , setBorders'
  , setTitle
  , setTitle'
  , setPosition
  , setPosition'
  , setDimension
  , setDimension'
  , writeStr
  , writeStr'
  , writeText
  , writeText'
  , writeShow
  , writeShow'
  , updateBox
    -- * Event Handling
  , onEvent
  , onEventBox
  , onEventGlobal
  , waitForBox
  , waitForGlobal
  , waitForWin
  , onEventWin
  , true
  -- ** Event Handling with Timeout
  , onEventBox'
  , onEventGlobal'
  , onEventWin'
  -- * Types
  , Config(..)
  , BoxAttributes(..)
  , Borders(..)
  , AlignV(..)
  , AlignH(..)
  , Title(..)
  , Direction(..)
  , Axis(..)
  ) where

import Data.Foldable (traverse_)
import Control.Applicative (liftA2)
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T

import UI.NCurses
import UI.NCurses.Panel

import UI.Lambox.Internal.Types
import UI.Lambox.Internal.Util

--TODO:
-- Boxes/Panels
-- - borders (dash '-' '|' or hash '#' or dot '*' ******** or plus '+' (or other char)
-- - gaps
-- - dimension
-- - ordering
-- - position
-- Widgets
-- - scroll
-- - text input
-- - check/radial boxes
-- - tabs
-- Updaters


-- | Start the program.
lambox :: Curses a -> IO a
lambox f = runCurses (setEcho False *> setCursorMode CursorInvisible *> f)

-- | Refreshes the Screen. This is a wrapper calling @ncurses@ 'refreshPanels' and 'render'
update :: Curses ()
update = refreshPanels *> render

-- | Create a panel given a Box type. Because the tui panel uses NCurses'
-- Panel and Window type, it cannot be garbage collected and must be
-- deleted manually with `deletePanel`
newBox :: Config -> Curses Box
newBox conf@Config{..} = do
  win <- newWindow configHeight configWidth configY configX
  pan <- newPanel win
  let box = Box conf win pan
  updateBox box
  refreshPanels
  pure box

-- | Take a config for an area then given the axis and a decimal,
-- split the area into two boxes. The decimal is that ratio between
-- the respective dimensions of the first box and second box.
splitBox :: RealFrac a => Config -> Axis -> a -> Curses (Box,Box)
splitBox Config{..} axis ratio = splitBox' configX configY configWidth configHeight configAttrs configAttrs contents contents axis ratio

-- | Take x and y coordinates, dimensions, axis, ratio, and
-- two attribute lists to split the area into two boxes, configuring
-- each box to the respective attributes (top/left takes first one)
splitBox' :: RealFrac a => Integer -> Integer -> Integer -> Integer -> BoxAttributes -> BoxAttributes -> Text -> Text -> Axis -> a -> Curses (Box,Box)
splitBox' x y width height attrs1 attrs2 txt1 txt2 axis ratio = do
  (conf1, conf2) <- case axis of
    Horizontal -> do
      let width1 = ratioIF width ratio
          width2 = width - width1
          x1 = x
          x2 = x + width1
      pure
        ( Config x1 y width1 height attrs1 txt1
        , Config x2 y width2 height attrs2 txt2
        )
    Vertical -> do
      let height1 = ratioIF height ratio
          height2 = height - height1
          y1 = y
          y2 = y + height1
      pure
        ( Config x y1 width height1 attrs1 txt1
        , Config x y2 width height2 attrs2 txt2
        )
  box1 <- newBox conf1
  box2 <- newBox conf2
  pure (box1,box2)

-- | Take a box and split it into two boxes, returning the new
-- box and altering the passed box as a side effect (CAUTION!)
--
-- The direction determines where the new box is in relation
-- to the passed box, and the fraction is the ratio of the
-- length or width of the new box to the old.
splitFromBox :: RealFrac a => Box -> Direction -> a -> BoxAttributes -> Curses (Box, Box) -- return Curses (Box, Box) (oldbox, newbox) with updated config settings
splitFromBox (Box Config{..} win pan) dir ratio attrs = do
  case dir of
    _ -> do -- DirUp
      let nHeight2 = ratioIF configHeight ratio
          nHeight1 = configHeight - nHeight2
          nuY2 = configY
          nuY1 = configY + nHeight2
      updateWindow win $ do
        resizeWindow nHeight1 configWidth -- have to redraw borders and title, to fix that bottom border bug
        moveWindow nuY1 configX
      let nConfig = Config configX nuY2 configWidth nHeight2 attrs ""
      box2 <- newBox nConfig
      let nbox1 = Box (Config configX nuY1 configWidth nHeight1 configAttrs "") win pan
      pure (nbox1,box2)

-- | Delete box. Because @ncurses@ 'Window's and 'Panel's are not garbage collected, calling this at the end of a Box's lifecycle is required.
deleteBox :: Box -> Curses ()
deleteBox (Box _ win pan) = deletePanel pan *> closeWindow win

-- | Delete multiple boxes from a foldable set
deleteBoxes :: Foldable f => f Box -> Curses ()
deleteBoxes = traverse_ deleteBox

-- | Get the position (x,y) of a box
getPosition :: Box -> (Integer, Integer)
getPosition (Box Config{..} _ _) = (configX, configY)

-- | Get the dimensions (width,height) of a box
getDimension :: Box -> (Integer, Integer)
getDimension (Box Config{..} _ _) = (configWidth, configHeight)

-- | Get the attributes of a box
getAttributes :: Box -> BoxAttributes
getAttributes (Box Config{..} _ _) = configAttrs

-- | Get the contents of a box
getBoxContents :: Box -> Text
getBoxContents (Box Config{..} _ _) = contents


-- withBox :: Box -> UpdateBox Box -> Curses Box -- UpdateBox should be a reader and the setAttr stuff should be put within it
--
-- set_ box >>= set_ >>= set_
--
-- withBox box $ do
--   setTitle ...
--   setBorders ...

-- | Set multiple attributes for a box, returning the box with updated config
withBox :: Foldable t => Box -> t (Box -> Curses Box) -> Curses Box
withBox box setAttrs
    | not $ null setAttrs = (foldl1 (>=>) setAttrs) box
    | otherwise           = pure box

-- TODO :: default configs like full(screen), up half, down third, etc, using direction and ratio
-- config :: Direction -> Ratio -> Config

-- | Set the attributes of the box, returning the box with updated config
setBoxAttributes :: BoxAttributes -> Box -> Curses Box
setBoxAttributes newAttrs box@(Box config _win _pan) =
  setBoxConfig (config { configAttrs = newAttrs }) box

-- | Set the new config of the box, returning the box with updated config
setBoxConfig :: Config -> Box -> Curses Box
setBoxConfig newConf (Box _ win pan) =
  liftA2 (*>) updateBox pure $ Box (newConf) win pan

-- | Set the borders of the box, returning the box with updated config
setBorders :: Borders -> Box -> Curses Box
setBorders newBorders box@(Box cfg _ _) =
  setBoxAttributes (configAttrs cfg) { attrBorders = newBorders } box

-- | Set the borders of the box, returning the box with updated config
setBorders' :: Box -> Borders -> Curses Box
setBorders' = flip setBorders

-- | Set the title of the box, returning the box with updated config
setTitle :: Maybe Title -> Box -> Curses Box
setTitle newTitle box@(Box cfg _ _) =
  setBoxAttributes (configAttrs cfg) { attrTitle = newTitle } box

-- | Set the title of the box, returning the box with updated config
setTitle' :: Box -> Maybe Title -> Curses Box
setTitle' = flip setTitle

-- | Set the position of the box, returning the box with updated config
setPosition :: ()
  => Integer -- ^ Horizontal position (x)
  -> Integer -- ^ Vertical position (y)
  -> Box
  -> Curses Box
setPosition x y box@(Box cfg _ _) =
  setBoxConfig (cfg { configX = x, configY = y }) box

-- | Set the position of the box, returning the box with updated config
setPosition' :: Box -> Integer -> Integer -> Curses Box
setPosition' box x y = setPosition x y box

-- | Set the dimension of the box, returning the box with updated config
setDimension :: ()
  => Integer -- ^ Width
  -> Integer -- ^ Height
  -> Box
  -> Curses Box
setDimension width height box@(Box cfg _ _) =
  setBoxConfig (cfg { configWidth = width, configHeight = height }) box

-- | Set the dimension of the box, returning the box with updated config
setDimension' :: Box -> Integer -> Integer -> Curses Box
setDimension' box w h = setDimension w h box

-- | Take a box and a pair of local coordinates and print a string within it
writeStr :: Integer -> Integer -> String -> Box -> Curses Box
writeStr x y str (Box conf win pan) = do
  let nBox = Box (conf { contents = T.pack str }) win pan
  updateWindow win $
    moveCursor y x
    *> drawString str
  pure nBox

-- | Take a box and a pair of local coordinates and print a string within it
writeStr' :: Box -> Integer -> Integer -> String -> Curses Box
writeStr' box x y str = writeStr x y str box

-- | Take a box and a pair of local coordinates and print some text within it
writeText :: Integer -> Integer -> Text -> Box -> Curses Box
writeText x y txt (Box conf win pan) = do
  let nBox = Box (conf { contents = txt }) win pan
  updateWindow win $
    moveCursor y x
    *> drawText txt
  pure nBox

-- | Take a box and a pair of local coordinates and print some text within it
writeText' :: Box -> Integer -> Integer -> Text -> Curses Box
writeText' box x y txt = writeText x y txt box

-- | Like writeStr but with any showable type
writeShow :: Show a => Integer -> Integer -> a -> Box -> Curses Box
writeShow x y a box = writeShow' box x y a

-- | Like writeStr but with any showable type
writeShow' :: Show a => Box -> Integer -> Integer -> a -> Curses Box
writeShow' box x y = ((writeStr' box x y) . show)

defaultBorder :: Maybe Glyph -> Update ()
defaultBorder g =
  drawBorder g g g g g g g g

-- | (NOTE: for internal use) Update the box to reflect its new config
updateBox :: Box -> Curses ()
updateBox (Box Config{..} win pan) = do
  movePanel pan configY configX
  updateWindow win $ do
    clear
    --moveWindow configY configX
    resizeWindow configHeight configWidth
    case attrBorders configAttrs of
      Line            -> drawBox (Just glyphLineV) (Just glyphLineH)
      Hash            -> defaultBorder (Just $ Glyph '#' [])
      Dot             -> defaultBorder (Just $ Glyph '*' [])
      Plus            -> defaultBorder (Just $ Glyph '+' [])
      None            -> defaultBorder (Just $ Glyph ' ' [])
      Char c          -> defaultBorder (Just $ Glyph c   [])
      Symbol g        -> defaultBorder (Just g)
      Custom border   ->
        drawBorder
          (Just $ left border)
          (Just $ right border)
          (Just $ top border)
          (Just $ bottom border)
          (Just $ topLeft border)
          (Just $ topRight border)
          (Just $ botLeft border)
          (Just $ botRight border)
    case attrTitle configAttrs of
      Nothing -> pure ()
      Just (Title title hAlign vAlign) -> do
        let vert = case vAlign of
              AlignLeft -> 1
              AlignCenter -> (configWidth `quot` 2) - ((toInteger $ length title) `quot` 2)
              AlignRight -> (configWidth-1) - (toInteger $ length title)
            horz = case hAlign of
              AlignTop -> 0
              AlignBot -> configHeight-1
        moveCursor horz vert *> drawString title
    case contents of
      ""  -> pure ()
      txt -> pure () -- moveCursor 1 1 *> drawText txt -- flesh out drawing so contents can wrap, and have more powerful features than just being text

-- | Perform action if an event is passed and it meets the event condition, else do nothing
onEvent :: Maybe Event -> (Event -> Bool) -> Action a -> Curses ()
onEvent (Just event) p action = if p event then action event *> pure () else pure ()
onEvent Nothing _ _           = pure ()

-- | Perform action if event passed within specified Box meets the event condition, else do nothing
onEventBox :: Box -> (Event -> Bool) -> Action a -> Curses ()
onEventBox = onEventBox' 0

-- | Similar to 'onEventBox' but happens on any event within the default window
onEventGlobal :: (Event -> Bool) -> Action a -> Curses ()
onEventGlobal = onEventGlobal' 0

-- | Wait for an event condition in the specified Box to be met before continuing
waitForBox :: Box -> (Event -> Bool) -> Curses ()
waitForBox (Box _ w _) = waitForWin w

-- | Similar to 'waitForBox' but happens on any event within the default window
waitForGlobal :: (Event -> Bool) -> Curses ()
waitForGlobal p = defaultWindow >>= flip waitForWin p

-- | (NOTE: For internal use mainly) Wait for an event condition in the specified Window to be met before continuing
waitForWin :: Window -> (Event -> Bool) -> Curses ()
waitForWin w p = getEvent w Nothing >>= \case
  Just event -> if p event then pure () else waitForWin w p
  Nothing    -> waitForWin w p

-- | (NOTE: For internal use mainly) Perform action if event passed within window meets the event condition, else do nothing
onEventWin :: Window -> (Event -> Bool) -> Action a -> Curses ()
onEventWin = onEventWin' 0

-- | Perform action if event passed within specified Box meets the event condition, else do nothing
onEventBox' :: Integer -> Box -> (Event -> Bool) -> Action a -> Curses ()
onEventBox' timeout (Box _ win _) p action = getEvent win (Just timeout) >>= \event -> onEvent event p action

-- | Similar to `onEventBox'` but happens on any event within the default window
onEventGlobal' :: Integer -> (Event -> Bool) -> Action a -> Curses ()
onEventGlobal' timeout p action = defaultWindow >>= flip getEvent (Just timeout) >>= \event -> onEvent event p action

-- | (NOTE: For internal use mainly) Perform action if event passed within window meets the event condition, else do nothing
onEventWin' :: Integer -> Window -> (Event -> Bool) -> Action a -> Curses ()
onEventWin' timeout window p action = getEvent window (Just timeout) >>= \event -> onEvent event p action
