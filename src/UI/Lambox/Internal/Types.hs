{-# language OverloadedStrings #-}

-- | Types that are used in UI.Lambox. This module should not be exported, as the necessary types are re-exported through UI.Lambox.
module UI.Lambox.Internal.Types
  ( -- * Box
    Box(..)
    -- * Configuration
  , Config(..)
  , Content(..)
    -- * Attributes
  , BoxAttributes(..)
  , Title(..)
  , Borders(..)
  , Border(..)
    -- * Alignment
  , AlignV(..)
  , AlignH(..)
  , Direction(..)
  , Axis(..)
    -- * Action
  , Action
  ) where

import UI.NCurses
import UI.NCurses.Panel

import Data.Text

-- | A 'Box' is an @ncurses@ 'Window' and 'Panel' along with a 'Config' providing information and context.
--
-- The primary motivation behind the Box type is to create an structure that stores information about it in a
-- way that is easy to reach and modify\/update. Any changes to a Box will return a new one with those changes
-- saved in its config.
data Box = Box Config Window Panel {- Contents -}

-- | Box Config, used to set and store information about the Box's position, size, and content.
data Config = Config --WIP
  { configX       :: Integer
  , configY       :: Integer
  , configWidth   :: Integer
  , configHeight  :: Integer
  , configAttrs   :: BoxAttributes
  , contents      :: [Content]
  } deriving (Eq)

-- | Inner 'Box' contents
data Content
  = Text !Integer !Integer Text -- other stuff TODO
  deriving(Eq)

-- | Stores Box attributes such as borders and title.
data BoxAttributes = BoxAttributes
  { attrBorders :: Borders
  , attrTitle   :: Maybe Title
  -- todo: background color, border color, text color, global text styling
  } deriving (Eq)

-- | Set the Box title, given a string and alignment
data Title = Title String AlignH AlignV deriving (Eq)

-- | Determine what kind of borders should appear around a Box. Can also use any character or any other Glyph from UI.NCurses
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

-- | Describe a custom border
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

-- | Vertical Alignment
data AlignV
  = AlignLeft
  | AlignCenter
  | AlignRight
  deriving (Eq)

-- | Horizontal Alignment
data AlignH
  = AlignTop
  | AlignBot
  deriving (Eq)

-- | Which direction the new Box should split in the function 'splitFromBox'
data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Eq)

-- | Along which axis should the boxes split in the 'splitBox' function. 'Vertical' will split the boxes in a top\/bottom fashion, 'Horizontal' splits them left\/right.
data Axis
  = Vertical
  | Horizontal
  deriving (Eq)

-- | An 'Action' is something that takes an event and runs in the 'Curses' monad
type Action a = Event -> Curses a
