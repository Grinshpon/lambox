module Main where

import UI.Lambox

main :: IO ()
main = lambox $ do
  setCursorMode CursorInvisible
  box1 <- newBox testBox
  writeStr box1 2 2 "Press 'q' to quit!"
  box2 <- splitBox box1 DirUp 0.5 Line
  writeStr box2 2 2 "Hello World!"
  update
  loop
  deleteBox box1
  deleteBox box2
  where
    loop = onEventGlobal (/= EventCharacter 'q') (update >> loop)
    testTitle = Title "LamBox" AlignRight AlignTop
    testBox = Config 2 2 22 10 Line $ Just testTitle
