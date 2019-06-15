module UI.Internal.Types where

import UI.NCurses
import UI.NCurses.Panel

-- probably remove all Show instanes, they're not really needed

data Box = Box Window Panel {- Contents -}

data Border = Border --WIP
  { topLeft  :: Char
  , topRight :: Char
  , botLeft  :: Char
  , botRight :: Char
  , left     :: Char
  , right    :: Char
  , top      :: Char
  , bottom   :: Char
  } deriving (Eq)

data Borders
  = Line
  | Hash
  | Dot
  | Plus
  | None
  | Char Char
  | Custom Border
  deriving (Eq)

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

data Title = Title String AlignV AlignH deriving (Eq)

data Config = Config --WIP
  { configX       :: Integer
  , configY       :: Integer
  , configWidth   :: Integer
  , configHeight  :: Integer
  , configBorders :: Borders
  , configTitle   :: Maybe Title
  --, contents  :: a
  } deriving (Eq)

-- Does this belong in types?
ratioIF :: RealFrac a => Integer -> a -> Integer
ratioIF x y = x `quot` (floor $ 1/y)

