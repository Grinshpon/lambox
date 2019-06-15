module Main where

import UI.Lambox

main :: IO ()
main = lambox $ do
  bigBox <- newBox testBox
  writeStr bigBox 2 2 "Press 'q' to quit!"
  (box1,box2) <- splitBox bigBox DirUp 0.5 [Borders Line]
  writeStr box2 2 2 "Hello World!"
  update
  go
  deleteBox box1
  deleteBox box2
  where
    go = onEventGlobal (/= EventCharacter 'q') (update >> go)
    testTitle = Title "LamBox" AlignTop AlignRight
    testBox = Config 2 2 22 10 [Borders Line,testTitle]
