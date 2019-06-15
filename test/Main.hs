module Main where

import UI.Lambox

main :: IO ()
main = lambox $ do
  p <- newBox testBox
  update
  loop
  deleteBox p
  where
    loop = onEventGlobal (/= EventCharacter 'q') loop
    testTitle = Title "LamBox" AlignLeft AlignTop
    testBox = Config 5 5 20 20 Line $ Just testTitle
