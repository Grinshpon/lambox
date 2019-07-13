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
  , Event(..) -- from ncurses
  , Curses -- from ncurses
  , CursorMode(..) --maybe don't re-export ncurses stuff?
  , setCursorMode
  ) where --remember to export relevant ncurses stuff as well (like events, curses, glyphs, etc) (???)

import Data.List (sort)

import UI.NCurses
import UI.NCurses.Panel

import UI.Lambox.Internal.Types
import UI.Lambox.Internal.Util

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
  updateWindow win $ do -- Will be replaced with configBox, setTitle/setBorders
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




-- setBox :: Box -> Curses a -> Curses ()
-- configBox :: Box -> Config -> Curses Box

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

{- -- HAVE TO COMBINE TO setAttrs BECAUSE BORDERS AND TITLES ARE BOTH BoxAttribute TYPE
-}
setBoxAttributes :: Box -> [BoxAttribute] -> Curses Box
setBoxAttributes box = \case
  []                            -> pure box
  (Borders nuBorder):nuAttrs    -> setBorders box nuBorder >>= flip setBoxAttributes nuAttrs
  (title@(Title _ _ _)):nuAttrs -> setTitle box (Just title) >>= flip setBoxAttributes nuAttrs
  -- use configAttr from newBox

setBorders :: Box -> Borders -> Curses Box
setBorders (Box Config{..} win pan) mBorders = do
  updateWindow win $ do
    case mBorders of
      None -> drawBox Nothing Nothing
      Line -> drawBox (Just glyphLineV) (Just glyphLineH)
      Hash -> drawBorder
        (Just glyphStipple)
        (Just glyphStipple)
        (Just glyphStipple)
        (Just glyphStipple)
        (Just glyphStipple)
        (Just glyphStipple)
        (Just glyphStipple)
        (Just glyphStipple)
      _ -> drawBox (Just glyphLineV) (Just glyphLineH) -- TODO: Complete
  pure (Box (Config configX configY configWidth configHeight newAttrs) win pan)
  where
    replace :: [BoxAttribute] -> [BoxAttribute] -> [BoxAttribute]
    replace s [] = s
    replace s ((Borders _):xs) = s <> xs
    replace s (x:xs) = replace (s <> [x]) xs
    newAttrs = case mBorders of
      None -> replace [] $ sort configAttrs
      cBorders -> (Borders cBorders):(replace [] $ sort configAttrs)

setTitle :: Box -> Maybe BoxAttribute -> Curses Box
setTitle box@(Box Config{..} win pan) mTitle = do
  setBorders box configBorders
  updateWindow win $ do
    case mTitle of
      Nothing -> pure ()
      Just (Title title hAlign vAlign) -> do
        let vert = case vAlign of
              AlignLeft -> 1
              AlignCenter -> (configWidth `quot` 2) - ((toInteger $ length title) `quot` 2)
              AlignRight -> (configWidth-1) - (toInteger $ length title)
            horz = case hAlign of
              AlignTop -> 0
              AlignBot -> configHeight-1
        moveCursor horz vert >> drawString title
  pure (Box (Config configX configY configWidth configHeight newAttrs) win pan)
  where
    findBorder :: [BoxAttribute] -> Borders
    findBorder [] = None
    findBorder (x:xs) = case x of
      (Borders borders) -> borders
      _ -> findBorder xs
    configBorders = findBorder $ sort configAttrs
    replace :: [BoxAttribute] -> [BoxAttribute] -> [BoxAttribute]
    replace s [] = s
    replace s ((Title _ _ _):xs) = s <> xs
    replace s (x:xs) = replace (s <> [x]) xs
    newAttrs = case mTitle of
      Nothing -> replace [] configAttrs
      (Just title) -> (replace [] configAttrs) <> [title]

