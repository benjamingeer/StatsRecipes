{-

Prints out some summary statistics about an array of data.

-}

module Main where

import qualified Data.List as L
import Control.Monad (when)
import System.Environment
import qualified Data.Packed.Vector as Vector
import qualified Numeric.GSL.Statistics as Stat
import Text.Printf (printf)

data Desc = Desc { dMin :: Double,
                   d1q :: Double,
                   dMedian :: Double,
                   dMean :: Double,
                   d3q :: Double,
                   dMax :: Double,
                   dStdDev :: Double }

desc ls =
  let sorted = L.sort ls
      v = Vector.fromList sorted
      mean = Stat.mean v
  in Desc { dMin = head sorted,
            d1q = Stat.quantile 0.25 v,
            dMedian = Stat.median v,
            dMean = mean,
            d3q = Stat.quantile 0.75 v,
            dMax = last sorted,
            dStdDev = Stat.stddev_m mean v }

main = do
  args <- getArgs
  when (null args) $ error "Expected data file"
  xs <- readData(head args)
  
  let d = desc xs
  printf "Minimum: %.2f\n" (dMin d)
  printf "First quartile: %.2f\n" (d1q d)
  printf "Median: %.2f\n" (dMedian d)
  printf "Mean: %.2f\n" (dMean d)
  printf "Third quartile: %.2f\n" (d3q d)
  printf "Maximum: %.2f\n" (dMax d)
  printf "Standard deviation: %.3f\n" (dStdDev d)

readData :: String -> IO [Double]
readData dataFile = do
  dataStr <- readFile dataFile
  return $ map read (lines dataStr)
