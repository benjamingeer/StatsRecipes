{-

A one-sample t confidence interval.

Biologists studying the healing of skin wounds measured the rate at
which new cells closed a razor cut made in the skin of an anesthetized
newt. The file newts.csv contains data from 18 newts, measured in
micrometers per hour. The population distribution is Normal. Estimate
the mean rate μ for all newts of this species by giving a 95%
confidence interval. (Moore, David S. The Basic Practice of
Statistics. 4th ed. New York: W. H. Freeman, 2007, pp. 437-438,
example 18.2.)

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

Given an array of data and a confidence level, returns the sample
mean, the t statistic, and the lower and upper limits of the t
confidence interval.

-}
oneSampleTInterval :: [Double] -> Double -> (Double, Double, Double, Double)
oneSampleTInterval sample c =
  let n = fromIntegral (length sample)
      v = Vector.fromList sample
      xBar = Stat.mean v
      s = Stat.stddev_m xBar v
      t = density_1p TDist UppInv (n - 1.0) ((1.0 - c) / 2.0)
      se = s / sqrt n
      absErr = t * se
      lowerLim = xBar - absErr
      upperLim = xBar + absErr
  in (xBar, t, lowerLim, upperLim)

main = do
  args <- getArgs
  when (null args) $ error "Expected data file"
  sample <- readData(head args)
  
  let (xBar, t, lowerLim, upperLim) = oneSampleTInterval sample 0.95
  printf "Sample mean: %.4f\n" xBar
  printf "t: %.4f\n" t
  printf "95%% confidence interval: %.4f to %.4f\n" lowerLim upperLim

readData :: String -> IO [Double]
readData dataFile = do
  dataStr <- readFile dataFile
  return $ map read (lines dataStr)
