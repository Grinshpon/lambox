module UI.Lambox.Internal.Types where

import UI.Lambox.Internal.Util

import UI.NCurses
import UI.NCurses.Panel

-- probably remove all Show instanes, they're not really needed

data Box = Box Config Window Panel {- Contents -}

data Border = Border --WIP
  { topLeft  :: Char
  , topRight :: Char
  , botLeft  :: Char
  , botRight :: Char
  , left     :: Char
  , right    :: Char
  , top      :: Char
  , bottom   :: Char
  } deriving (Eq,Ord)

data Borders
  = Line
  | Hash
  | Dot
  | Plus
  | None
  | Char Char
  | Custom Border
  deriving (Eq,Ord)

data AlignV
  = AlignLeft
  | AlignCenter
  | AlignRight
  deriving (Eq,Ord)

data AlignH
  = AlignTop
  | AlignBot
  deriving (Eq,Ord)

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


-- | Ord instance is there because there is an order to drawing attributes
-- so that they do not draw over each other.
data BoxAttribute
  = Borders Borders
  | Title String AlignH AlignV
  deriving (Eq,Ord) -- todo: background color, border color, text color, global text styling

-- data Title = Title String AlignV AlignH deriving (Eq)

data Config = Config --WIP
  { configX       :: Integer
  , configY       :: Integer
  , configWidth   :: Integer
  , configHeight  :: Integer
  , configAttrs   :: [BoxAttribute]
  } deriving (Eq)
