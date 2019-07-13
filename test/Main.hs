module Main where

import UI.Lambox

main :: IO ()
main = lambox $ do
  (box1,box2) <- do
    (b1,b2) <- splitBox config Vertical 0.5
    nb1 <- setBoxAttributes b1 [title]
    pure (nb1, b2)
  writeStr box1 2 2 "Hello World!"
  writeStr box2 2 2 "Press 'q' to quit!"
  update
  go
  deleteBox box1
  deleteBox box2
  where
    go = onEventGlobal (/= EventCharacter 'q') (\_ -> update >> go)
    title = Title "LamBox" AlignTop AlignRight
    config = Config 2 2 22 10 [Borders Line]
