module UI.Lambox.Internal.Util where

ratioIF :: RealFrac a => Integer -> a -> Integer
ratioIF x y = x `quot` (floor $ 1/y)

-- | If you want to use one of the onEvent's regardless of the event predicate,
-- just pass in `true`, which is simply defined as:
--
-- > true = const True
true :: a -> Bool
true = const True
