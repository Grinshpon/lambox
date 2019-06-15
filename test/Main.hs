module Main where

import UI.Lambox

main :: IO ()
main = lambox $ do
  setCursorMode CursorInvisible
  p <- newBox testBox
  writeStr p 2 2 "Press 'q' to quit!"
  update
  loop
  deleteBox p
  where
    loop = onEventGlobal (/= EventCharacter 'q') (update >> loop)
    testTitle = Title "LamBox" AlignRight AlignTop
    testBox = Config 5 5 22 5 Line $ Just testTitle
