{-# LANGUAGE OverloadedStrings #-}
module Main where

import UI.NCurses (Event(..))
import UI.Lambox

import Control.Monad

main :: IO ()
main = lambox $ do
  (box1,box2) <- do
    (b1,b2) <- splitBox config Vertical 0.5
    nb1 <- withBox b1
      [ setTitle (Just title)
      , writeStr 2 2 "Hello World!"
      ]
    nb2 <- writeStr' b2 2 2 "Press 'q' to quit!"
    pure (nb1, nb2)
  update
  go
  deleteBoxes [box1, box2]
  where
    go = onEventGlobal (/= EventCharacter 'q') (\_ -> update *> go)
    title = Title "LamBox" AlignTop AlignRight
    config = Config 2 2 22 10 (BoxAttributes Line Nothing) ""
