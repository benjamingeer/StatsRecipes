{-

A one-sample t test.

Cola makers test new recipes for loss of sweetness during
storage. Trained tasters rate the sweetness before and after
storage. File cola.csv contains the sweetness losses (sweetness before
storage minus sweetness after storage) found by 10 tasters for one new
cola recipe. Are these data good evidence that the cola lost
sweetness? (Moore, David S. The Basic Practice of Statistics. 4th
ed. New York: W. H. Freeman, 2007, pp. 440-441, example 18.3.)

-}

module Main where

import qualified Data.List as L
import Control.Monad (when)
import System.Environment
import qualified Data.Packed.Vector as Vector
import qualified Numeric.GSL.Statistics as Stat
import Numeric.GSL.Distribution.Continuous
import Text.Printf (printf)

{-

Given an array of data and the expected mean in the null hypothesis,
returns the sample mean, the t statistic and the probability of the
sample mean if the null hypothesis is true.

-}
oneSampleTTest :: [Double] -> Double -> (Double, Double, Double)
oneSampleTTest sample mu0 =
  let n = fromIntegral (length sample)
      v = Vector.fromList sample
      xBar = Stat.mean v
      s = Stat.stddev_m xBar v
      se = s / sqrt n
      t = (xBar - mu0) / se
      p = density_1p TDist Upper (n - 1.0) t
  in (xBar, t, p)

main = do
  args <- getArgs
  when (null args) $ error "Expected data file"
  sample <- readData(head args)
  
  let (xBar, t, p) = oneSampleTTest sample 0

  printf "Sample mean: %.4f\n" xBar
  printf "One-stample t statistic: %.4f\n" t
  printf "P: %.4f\n" p

readData :: String -> IO [Double]
readData dataFile = do
  dataStr <- readFile dataFile
  return $ map read (lines dataStr)
