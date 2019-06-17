module Main where

import UI.Lambox

main :: IO ()
main = lambox $ do
  (box1,box2) <- splitBox testBox Vertical 0.5
  writeStr box1 2 2 "Hello World!"
  writeStr box2 2 2 "Press 'q' to quit!"
  update
  go
  deleteBox box1
  deleteBox box2
  where
    go = onEventGlobal (/= EventCharacter 'q') (update >> go)
    testTitle = Title "LamBox" AlignTop AlignRight
    testBox = Config 2 2 22 10 [Borders Line,testTitle]
