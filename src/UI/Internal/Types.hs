module UI.Internal.Types where

import UI.NCurses
import UI.NCurses.Panel

data Box = Box Window Panel

data Border = Border
  { topLeft  :: Char
  , topRight :: Char
  , botLeft  :: Char
  , botRight :: Char
  , left     :: Char
  , right    :: Char
  , top      :: Char
  , bottom   :: Char
  } deriving (Eq, Show)

data Borders
  = Line
  | Hash
  | Dot
  | Plus
  | Char Char
  | Custom Border
  deriving (Eq, Show)

data AlignV
  = AlignLeft
  | AlignCenter
  | AlignRight
  deriving (Eq, Show)

data AlignH
  = AlignTop
  | AlignBottom
  deriving (Eq, Show)

data Title = Title String AlignV AlignH deriving (Eq, Show)

data Config = Config --WIP
  { configX       :: Integer
  , configY       :: Integer
  , configWidth   :: Integer
  , configHeight  :: Integer
  , configBorders :: Borders
  , configTitle   :: Maybe Title
  --, contents  :: a
  } deriving (Eq, Show)



