module Statistics.Shitty.Regression where

import Data.List


type Point = (Double, Double)

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

deriv :: [Point] -> [Point]
deriv points
 = let gs = zipWith gradient points (drop 1 points)
   in  zip (fmap fst points) gs

gradient :: Point -> Point -> Double
gradient (x1, y1) (x2, y2)
  = (y2 - y1) / (x2 - x1)

variance :: [Double] -> Double
variance xs@(_:_:_)
 = let (n,_,m2) = foldl' go (0,0,0) xs
   in   m2 / (n - 1)
 where
  go (n,m,m2) x
   = let n'    = n + 1
         delta = x - m
         m'    = m + delta / n'
         m2'   = m2 + delta * (x - m')
     in  (n',m',m2')
variance _ = 0 / 0

sd :: [Double] -> Double
sd = sqrt . variance

polyOrder
  :: Double
  -> [Point]
  -> Maybe Int
polyOrder epsilon points@(_:_:_:_)
 | isConstant points
 = Just 0
 | otherwise
 = fmap succ $ polyOrder epsilon $ deriv points
 where
  isConstant derivs
   = sd (fmap snd derivs) < epsilon
polyOrder _ _
 = Nothing
