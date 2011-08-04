{-

A one-sample t confidence interval.

Biologists studying the healing of skin wounds measured the rate at
which new cells closed a razor cut made in the skin of an anesthetized
newt. The file newts.csv contains data from 18 newts, measured in
micrometers per hour. The population distribution is Normal. Estimate
the mean rate μ for all newts of this species by giving a 95%
confidence interval.

-}

module Main where

import qualified Data.List as L
import Control.Monad (when)
import System.Environment
import qualified Data.Packed.Vector as Vector
import qualified Numeric.GSL.Statistics as Stat
import Numeric.GSL.Distribution.Continuous
import Text.Printf (printf)

main = do
  args <- getArgs
  when (null args) $ error "Expected data file"
  xs <- readData(head args)
  
  let n = fromIntegral (length xs)
  let v = Vector.fromList xs
  let xBar = Stat.mean v
  printf "Sample mean: %.4f\n" xBar
  
  let s = Stat.stddev_m xBar v
  printf "Sample standard deviation: %.4f\n" s
  
  let t = density_1p TDist UppInv (n - 1.0) ((1.0 - 0.95) / 2.0)
  printf "t: %.4f\n" t
  
  let se = s / sqrt n
  let absErr = t * se
  let lowerLim = xBar - absErr
  let upperLim = xBar + absErr
  printf "95%% confidence interval: %.4f to %.4f\n" lowerLim upperLim

readData :: String -> IO [Double]
readData dataFile = do
  dataStr <- readFile dataFile
  return $ map read (lines dataStr)
