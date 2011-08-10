{-

Statistics for regression inference.

Problem: Child development researchers explored the relationship
between the crying of infants four to ten days old and their later IQ
test scores. A snap of a rubber band on the sole of the foot caused
the infants to cry. The researchers recorded the crying and measured
its intensity by the number of peaks in the most active 20
seconds. They later measured the children's IQ at age three years
using the Stanford-Binet IQ test. Do children with higher crying
counts tend to have higher IQ? (Moore, David S. The Basic Practice of
Statistics. 4th ed. New York: W. H. Freeman, 2007, pp. 581-592,
exercises 24.1-24.4.)

-}

module Main where

import Control.Monad (when)
import System.Environment
import Text.CSV.ByteString (CSV, parseCSV)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lex.Double as BD
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Packed.Vector as V
import qualified Numeric.GSL.Fitting.Linear as Linear
import qualified Numeric.GSL.Statistics as Stats
import Numeric.GSL.Distribution.Continuous
import Text.Printf (printf)

main = do
  args <- getArgs
  when (length args /= 1) $ error "Expected data file"
  let [dataFile] = args

  csvStr <- B.readFile dataFile
  let (xs, ys) = case parseCSV csvStr of
        Nothing -> error $ "Couldn't parse CSV file " ++ dataFile
        Just csv -> csvToDoubles csv
  
  let (a, b, r, seB, p, lowerLim, upperLim) = linearRegression xs ys 0.95
  
  printf "Slope: %.4f\n" b
  printf "Intercept: %.4f\n" a
  printf "r: %.4f\n" r
  printf "Standard error of the slope: %.4f\n" seB
  printf "P (two-sided): %.4f\n" p
  printf "P (one-sided): %.4f\n" (p / 2.0)
  printf "95%% confidence interval for the slope: from %.4f to %.4f\n"
    lowerLim upperLim  

csvToDoubles :: CSV -> ([Double], [Double])
csvToDoubles csv =
  foldr convRow ([], []) csv where
    convRow row (xAcc, yAcc) = case row of
      x:y:[] -> (fieldToDouble x : xAcc,
                 fieldToDouble y : yAcc)
      _ -> error "Expected two columns per row"
      where fieldToDouble field =
              case BD.readDouble field of
                Nothing -> error $ "Couldn't parse field \"" ++
                           T.unpack (decodeUtf8 field) ++ "\""
                Just (value, _) -> value

linearRegression :: [Double] -> [Double] -> Double ->
                    (Double, Double, Double, Double, Double, Double, Double)
linearRegression xs ys c =
  let (vx, vy) = (V.fromList xs, V.fromList ys)
      (a, b, _, _, _, ySumSq) = Linear.linear vx vy
      r = Stats.correlation vx vy
      n = V.dim vx
      s = sqrt ((1.0 / fromIntegral (n - 2)) * ySumSq)
      xSumSq = sqrt (Stats.tot_sumsq vx)
      seB = s / xSumSq
      t = b / seB
      df = fromIntegral (n - 2)
      p = 2.0 * density_1p TDist Upper df (abs t)
      tConfidence = density_1p TDist UppInv df ((1.0 - c) / 2.0)
      absErr = tConfidence * seB
      lowerLim = b - absErr
      upperLim = b + absErr
  in (a, b, r, seB, p, lowerLim, upperLim)
