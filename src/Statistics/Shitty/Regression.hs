module Statistics.Shitty.Regression where

square :: Num a => a -> a
square x = x * x

rSquared
  :: [Double] -- ^ data set y1..yn
  -> [Double] -- ^ model f1..fn
  -> Double   -- ^ r squared
rSquared ys fs
 = let yBar  = sum ys / fromIntegral (length ys)
       ssTot = sum (fmap (square . subtract yBar) ys)
       ssRes = sum (fmap square (zipWith (-) ys fs))
   in  1 - (ssRes / ssTot)
