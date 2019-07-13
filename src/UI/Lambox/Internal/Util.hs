module UI.Lambox.Internal.Util where

ratioIF :: RealFrac a => Integer -> a -> Integer
ratioIF x y = x `quot` (floor $ 1/y)
