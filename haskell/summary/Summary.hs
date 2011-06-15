{-# LANGUAGE DeriveDataTypeable #-}

{-

Prints out some summary statistics about an array of data.

-}

module Main where

import qualified Data.List as L
import Control.Monad (when)
import System.Environment
import Text.CSV.ByteString (CSV, parseCSV)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lex.Double as BD
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
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
            dMean = Stat.mean v,
            d3q = Stat.quantile 0.75 v,
            dMax = last sorted,
            dStdDev = Stat.stddev_m mean v }

main = do
  args <- getArgs
  when (null args) $ error "Expected data file"
  let dataFile = head args
  csvStr <- B.readFile dataFile
  let xs = case parseCSV csvStr of
        Nothing -> error $ "Couldn't parse CSV file " ++ dataFile
        Just csv -> csvToDoubles csv
  printSummary(desc xs)


-- Parse the numbers in the data file
csvToDoubles :: CSV -> [Double]
csvToDoubles csv =
  map fieldToDouble (concat csv)
    where fieldToDouble field =
            case BD.readDouble field of
              Nothing -> error $ "Couldn't parse field " ++
                         T.unpack (decodeUtf8 field)
              Just (value, _) -> value

-- Print the summary statistics
printSummary :: Desc -> IO ()
printSummary d = do
  printf "Minimum: %.2f\n" (dMin d)
  printf "First quartile: %.2f\n" (d1q d)
  printf "Median: %.2f\n" (dMedian d)
  printf "Mean: %.2f\n" (dMean d)
  printf "Third quartile: %.2f\n" (d3q d)
  printf "Maximum: %.2f\n" (dMax d)
  printf "Standard deviation: %.3f\n" (dStdDev d)
