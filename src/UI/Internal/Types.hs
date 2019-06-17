module UI.Internal.Types where

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

-- Does this belong in types?
ratioIF :: RealFrac a => Integer -> a -> Integer
ratioIF x y = x `quot` (floor $ 1/y)

