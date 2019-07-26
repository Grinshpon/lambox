{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE LambdaCase        #-}

-- TODO Reorganize everything

module UI.Lambox
  ( module UI.Lambox
  , Config(..)
  --, Box(..)
  , BoxAttributes(..)
  , Borders(..)
  , AlignV(..)
  , AlignH(..)
  , Title(..)
  , Direction(..)
  , Axis(..)
--  , Event(..) -- from ncurses
--  , Curses -- from ncurses
--  , CursorMode(..) --maybe don't re-export ncurses stuff?
--  , setCursorMode -- from ncurses
  ) where --remember to export relevant ncurses stuff as well (like events, curses, glyphs, etc) (???)

-- import Data.List (sort)
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
-- |- borders (dash '-' '|' or hash '#' or dot '*' ******** or plus '+' (or other char)
-- |- gaps
-- |- dimension
-- |- ordering
-- |- position
-- Widgets
-- |- scroll
-- |- text input
-- |- check/radial boxes
-- |- tabs
-- Updaters

-- | Wait for condition to be met before continuing
waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = onEvent w p (const $ pure ())

-- | Similar to onEvent, but does is passed the event
onEvent' :: Maybe Event -> (Event -> Bool) -> Action a -> Curses ()
onEvent' (Just event) p action = if p event then action event *> pure () else pure ()
onEvent' Nothing _ _           = pure ()

-- | Perform action if event passed within window meets the event condition, else do nothing
onEvent :: Window -> (Event -> Bool) -> Action a -> Curses ()
onEvent window p action = getEvent window Nothing >>= \event -> onEvent' event p action

-- | Perform action if event passed within specified box meets the event condition, else do nothing
onEventBox :: Box -> (Event -> Bool) -> Action a -> Curses ()
onEventBox (Box _ win _) p action = getEvent win Nothing >>= \event -> onEvent' event p action

-- | Similar to onEvent but happens on any event within the default window
onEventGlobal :: (Event -> Bool) -> Action a -> Curses ()
onEventGlobal p action = defaultWindow >>= flip getEvent Nothing >>= \event -> onEvent' event p action

-- | If you want to use one of the onEvent's regardless of the event predicate,
-- simply pass in `true`.
true :: a -> Bool
true = const True

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

-- | Delete panel
deleteBox :: Box -> Curses ()
deleteBox (Box _ win pan) = deletePanel pan *> closeWindow win

-- | Delete multiple boxes from a foldable set
deleteBoxes :: Foldable f => f Box -> Curses ()
deleteBoxes = traverse_ deleteBox

-- | Literally just a synonym for render
update :: Curses ()
update = refreshPanels *> render

-- | Start the program
lambox :: Curses a -> IO a
lambox f = runCurses (setEcho False *> setCursorMode CursorInvisible *> f)

-- TODO :: default configs like full(screen), up half, down third, etc, using direction and ratio
-- config :: Direction -> Ratio -> Config

-- | Take a box and a pair of local coordinates and print a string within it
writeStr :: Integer -> Integer -> String -> Box -> Curses Box
writeStr x y str (Box conf win pan) = do
  let newBox = Box (conf { contents = T.pack str }) win pan
  updateWindow win $
    moveCursor y x
    *> drawString str
  pure newBox

-- | Take a box and a pair of local coordinates and print some text within it
writeText :: Integer -> Integer -> Text -> Box -> Curses Box
writeText x y txt (Box conf win pan) = do
  let newBox = Box (conf { contents = txt }) win pan
  updateWindow win $
    moveCursor y x
    *> drawText txt
  pure newBox

-- | Like writeStr but with any showable type
writeShow :: Show a => Integer -> Integer -> a -> Box -> Curses Box
writeShow x y a box = ((writeStr' box x y) . show) a

-- | Take a box and a pair of local coordinates and print a string within it
writeStr' :: Box -> Integer -> Integer -> String -> Curses Box
writeStr' (Box conf win pan) x y str = do
  let newBox = Box (conf { contents = T.pack str }) win pan
  updateWindow win $
    moveCursor y x
    *> drawString str
  pure newBox

-- | Take a box and a pair of local coordinates and print some text within it
writeText' :: Box -> Integer -> Integer -> Text -> Curses Box
writeText' (Box conf win pan) x y txt = do
  let newBox = Box (conf { contents = txt }) win pan
  updateWindow win $
    moveCursor y x
    *> drawText txt
  pure newBox

-- | Like writeStr but with any showable type
writeShow' :: Show a => Box -> Integer -> Integer -> a -> Curses Box
writeShow' box x y = ((writeStr' box x y) . show)


-- | Take a box and split it into two boxes, returning the new
-- box and altering the passed box as a side effect (CAUTION!)
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

-- withBox :: Box -> UpdateBox Box -> Curses Box -- UpdateBox should be a reader and the setAttr stuff should be put within it
--
-- set_ box >>= set_ >>= set_
--
-- withBox box $ do
--   setTitle ...
--   setBorders ...

withBox :: Foldable t => Box -> t (Box -> Curses Box) -> Curses Box
withBox box setAttrs
    | not $ null setAttrs = (foldl1 (>=>) setAttrs) box
    | otherwise           = pure box

-- | Set the attributes of the box, returning the box with updated config
setBoxAttributes :: BoxAttributes -> Box -> Curses Box
setBoxAttributes newAttrs (Box config win pan) = do
  let newBox = Box (config { configAttrs = newAttrs }) win pan
  liftA2 (*>) updateBox pure newBox

-- | Set the borders of the box, returning the box with updated config
setBorders :: Borders -> Box -> Curses Box
setBorders newBorders box@(Box cfg _ _) =
  setBoxAttributes (configAttrs cfg) { attrBorders = newBorders } box

-- | Set the title of the box, returning the box with updated config
setTitle :: Maybe Title -> Box -> Curses Box
setTitle newTitle box@(Box cfg _ _) =
  setBoxAttributes (configAttrs cfg) { attrTitle = newTitle } box

-- | Set the borders of the box, returning the box with updated config
setBorders' :: Box -> Borders -> Curses Box
setBorders' box@(Box cfg _ _) newBorders =
  setBoxAttributes (configAttrs cfg) { attrBorders = newBorders } box

-- | Set the title of the box, returning the box with updated config
setTitle' :: Box -> Maybe Title -> Curses Box
setTitle' box@(Box cfg _ _) newTitle =
  setBoxAttributes (configAttrs cfg) { attrTitle = newTitle } box


-- | (NOTE: for internal use) Update the box to reflect its new config
updateBox :: Box -> Curses ()
updateBox (Box Config{..} win pan) = updateWindow win $ do
  case attrBorders configAttrs of
    Line -> drawBox (Just glyphLineV) (Just glyphLineH)
    Hash -> drawBorder
      (Just $ Glyph '#' [])
      (Just $ Glyph '#' [])
      (Just $ Glyph '#' [])
      (Just $ Glyph '#' [])
      (Just $ Glyph '#' [])
      (Just $ Glyph '#' [])
      (Just $ Glyph '#' [])
      (Just $ Glyph '#' [])
    Dot -> drawBorder
      (Just $ Glyph '*' [])
      (Just $ Glyph '*' [])
      (Just $ Glyph '*' [])
      (Just $ Glyph '*' [])
      (Just $ Glyph '*' [])
      (Just $ Glyph '*' [])
      (Just $ Glyph '*' [])
      (Just $ Glyph '*' [])
    Plus -> drawBorder
      (Just $ Glyph '+' [])
      (Just $ Glyph '+' [])
      (Just $ Glyph '+' [])
      (Just $ Glyph '+' [])
      (Just $ Glyph '+' [])
      (Just $ Glyph '+' [])
      (Just $ Glyph '+' [])
      (Just $ Glyph '+' [])
    None -> drawBorder
      (Just $ Glyph ' ' [])
      (Just $ Glyph ' ' [])
      (Just $ Glyph ' ' [])
      (Just $ Glyph ' ' [])
      (Just $ Glyph ' ' [])
      (Just $ Glyph ' ' [])
      (Just $ Glyph ' ' [])
      (Just $ Glyph ' ' [])
    (Char c) -> drawBorder
      (Just $ Glyph c [])
      (Just $ Glyph c [])
      (Just $ Glyph c [])
      (Just $ Glyph c [])
      (Just $ Glyph c [])
      (Just $ Glyph c [])
      (Just $ Glyph c [])
      (Just $ Glyph c [])
    (Symbol g) -> drawBorder
      (Just g)
      (Just g)
      (Just g)
      (Just g)
      (Just g)
      (Just g)
      (Just g)
      (Just g)
    (Custom border) -> drawBorder
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
      txt -> moveCursor 1 1 *> drawText txt -- flesh out drawing so contents can wrap, and have more powerful features than just being text

