{-

A one-sample t test.

Cola makers test new recipes for loss of sweetness during
storage. Trained tasters rate the sweetness before and after
storage. File cola.csv contains the sweetness losses (sweetness before
storage minus sweetness after storage) found by 10 tasters for one new
cola recipe. Are these data good evidence that the cola lost
sweetness?

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
  
  let se = s / sqrt n
  let tStat = xBar / se
  printf "One-stample t statistic: %.4f\n" tStat
  
  let p = density_1p TDist Upper (n - 1.0) tStat
  printf "P: %.4f\n" p

readData :: String -> IO [Double]
readData dataFile = do
  dataStr <- readFile dataFile
  return $ map read (lines dataStr)
