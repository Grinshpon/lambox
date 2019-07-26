module UI.Lambox.Internal.Types where

import UI.Lambox.Internal.Util

import UI.NCurses
import UI.NCurses.Panel

-- probably remove all Show instanes, they're not really needed

data Box = Box Config Window Panel {- Contents -}

data Border = Border --WIP
  { topLeft  :: Glyph
  , topRight :: Glyph
  , botLeft  :: Glyph
  , botRight :: Glyph
  , left     :: Glyph
  , right    :: Glyph
  , top      :: Glyph
  , bottom   :: Glyph
  } deriving (Eq)

data Borders
  = Line
  | Hash
  | Dot
  | Plus
  | None
  | Char Char
  | Symbol Glyph
  | Custom Border
  deriving (Eq)

data Title = Title String AlignH AlignV deriving (Eq)

data AlignV
  = AlignLeft
  | AlignCenter
  | AlignRight
  deriving (Eq)

data AlignH
  = AlignTop
  | AlignBot
  deriving (Eq)

data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Eq)

data Axis
  = Vertical
  | Horizontal
  deriving (Eq)

data BoxAttributes = BoxAttributes
  { attrBorders :: Borders
  , attrTitle   :: Maybe Title
  -- todo: background color, border color, text color, global text styling
  } deriving (Eq)

data Config = Config --WIP
  { configX       :: Integer
  , configY       :: Integer
  , configWidth   :: Integer
  , configHeight  :: Integer
  , configAttrs   :: BoxAttributes
  } deriving (Eq)

type Action a = Event -> Curses a
