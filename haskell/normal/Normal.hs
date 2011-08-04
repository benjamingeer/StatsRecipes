{-

Calculations using Normal distributions.

-}

module Main where

import Numeric.GSL.Distribution.Common
import qualified Numeric.GSL.Statistics as Stat
import Numeric.GSL.Distribution.Continuous
import Text.Printf

{-

Problem 1: If women's height has the N(64, 2.7) distribution, what is
the probability that a randomly chosen woman has height between 68 and
70 inches? (Moore, David S. The Basic Practice of Statistics. 4th
ed. New York: W. H. Freeman, 2007, pp. 258-259, example 10.9.)

-}

p1 =
  let mean = 64.0
      stdDev = 2.7
  in (density_1p Gaussian Lower stdDev (70 - mean)) -
     (density_1p Gaussian Lower stdDev (68 - mean))

{-

Problem 2: Investigators looked at a random sample of 97 articles
reporting on placebo-controlled randomized trials in the top five
general medical journals. Only 7 of the 97 discussed the success of
blinding, and in 5 of these the blinding was imperfect. What
proportion of all such studies discuss the success of blinding? Give
a 95% confidence interval. (Moore, David S. The Basic Practice of
Statistics. 4th ed. New York: W. H. Freeman, 2007, p. 500,
example 20.5.)

-}

p2 = plusFourInterval 97 7 0.95

{-

Calculates a plus-four confidence interval for a proportion. Returns a
tuple containing the upper and lower limits of the interval.

-}
plusFourInterval :: Int -> Int -> Double -> (Double, Double)
plusFourInterval n successes c =
    let pTilde = fromIntegral (successes + 2) / fromIntegral (n + 4)
        z = density_1p Gaussian UppInv 1 ((1.0 - c) / 2.0)
        absErr = z * sqrt ((pTilde * (1.0 - pTilde)) / fromIntegral (n + 4))
    in (pTilde - absErr, pTilde + absErr)

main = do
  printf "Problem 1: P = %.4f\n" p1
  
  let (lowerLim, upperLim) = p2
  printf "Problem 2: 95%% confidence interval from %.4f to %.4f\n" lowerLim
    upperLim
