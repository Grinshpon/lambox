module UI.Internal.Types where

import UI.NCurses
import qualified UI.NCurses.Panel as P

data Panel = Panel Window P.Panel

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

data Borders = Line | Hash | Dot | Plus | Char Char | Custom Border deriving (Eq, Show)

data (Show a) => Box a = Box --WIP
  { x         :: Integer
  , y         :: Integer
  , width     :: Integer
  , height    :: Integer
  , borders   :: Borders
  , contents  :: a
  } deriving (Eq, Show)



