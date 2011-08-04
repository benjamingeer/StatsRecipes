{-

A two-sample Welch's t test, along with a confidence interval.

Problem: How quickly do synthetic fabrics such as polyester decay in
landfills? A researcher buried polyester strips in the soil for
different lengths of time, then dug up the strips and measured the
force required to break them. Part of the study buried 10 strips of
polyester fabric in well-drained soil in the summer. Five of the
strips, chosen at random, were dug up after 2 weeks; the other 5 were
dug up after 16 weeks. The breaking strengths in pounds for the two
samples are given in the files fabric1.csv and fabric2.csv. We suspect
that decay increases over time. Do the data give good evidence that
mean breaking strength is less after 16 weeks than after 2 weeks? Give
a one-sided P-value for the null hypothesis, and a 90% confidence
interval for the difference between the sample means. (Moore, David
S. The Basic Practice of Statistics. 4th ed. New York: W. H. Freeman,
2007, pp. 463 and 466-468, examples 19.2, 19.3 and 19.4.)

This procedure doesn't seem to be implemented in any library, so we
implement it here using the formulas given on
<http://en.wikipedia.org/wiki/Welch%27s_t_test>. This gives the same
results as the t.test function in R.

-}

module Main where

import Control.Monad (when)
import System.Environment
import qualified Data.Packed.Vector as Vector
import qualified Numeric.GSL.Statistics as Stat
import Numeric.GSL.Distribution.Continuous
import Text.Printf (printf)

data Alternative = Less | Greater | TwoSided

main = do
  args <- getArgs
  when (length args /= 2) $ error "Expected two data files"
  let [file1, file2] = args
  sample1 <- readData file1
  sample2 <- readData file2
  
  let (t, df, p, lower_lim, upper_lim) = welchTest sample1
                                         sample2 0 Greater 0.90
                                         
  printf "t: %.4f\n" t
  printf "Degrees of freedom: %.4f\n" df
  printf "P: %.4f\n" p
  printf "90%% confidence interval: %.4f to %.4f\n" lower_lim upper_lim


{-

Performs Welch's two-sample t-test on two lists and calculates a
confidence interval, given the expected difference between the
population means (μ1 - μ2), the alternative hypothesis and the
confidence level. Returns a tuple containing the t-value, the degrees
of freedom, the probability, and the upper and lower limits of the
confidence interval.

-}
welchTest :: [Double] -> [Double] -> Double -> Alternative -> Double ->
             (Double, Double, Double, Double, Double)
welchTest a1 a2 deltaMu alternative c =
  let v1 = Vector.fromList a1
      v2 = Vector.fromList a2
      n1 = fromIntegral (Vector.dim v1)
      n2 = fromIntegral (Vector.dim v2)
      x1Bar = Stat.mean v1
      x2Bar = Stat.mean v2
      var1 = Stat.variance_m x1Bar v1
      var2 = Stat.variance_m x2Bar v2

      testT = ((x1Bar - x2Bar) - deltaMu) / sqrt ((var1 / n1) + (var2 / n2))

      df = ((var1 / n1) + (var2 / n2))^2 / ((var1^2 / (n1^2 * (n1 - 1))) +
                                            (var2^2 / (n2^2 * (n2 - 1))))

      p = case alternative of
        Less -> density_1p TDist Lower df testT
        Greater -> density_1p TDist Upper df testT
        TwoSided -> 2.0 * density_1p TDist Upper df (abs testT)

      intervalT = density_1p TDist UppInv df ((1.0 - c) / 2.0)
      absErr = intervalT * sqrt ((var1 / n1) + (var2 / n2))
      lowerLim = (x1Bar - x2Bar) - absErr
      upperLim = (x1Bar - x2Bar) + absErr

      in (testT, df, p, lowerLim, upperLim)

readData :: String -> IO [Double]
readData dataFile = do
  dataStr <- readFile dataFile
  return $ map read (lines dataStr)
