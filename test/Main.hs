{-# LANGUAGE RecordWildCards #-}

module Main where

import UI.Text

testBox :: Box ()
testBox = Box 10 10 10 10 Line ()

main :: IO ()
main = initTui $ do
  p <- newPanel testBox
  update
  loop
  deletePanel p
  where
    loop :: Curses ()
    loop = do
      onEventGlobal (/= EventCharacter 'q') loop
