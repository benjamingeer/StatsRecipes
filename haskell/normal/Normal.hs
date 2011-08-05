{-

Calculations using Normal distributions.

-}

module Main where

import Numeric.GSL.Distribution.Common
import qualified Numeric.GSL.Statistics as Stat
import Numeric.GSL.Distribution.Continuous
import Text.Printf

data Alternative = Less | Greater | TwoSided

{-

Problem 1: If women's height has the N(64, 2.7) distribution, what is
the probability that a randomly chosen woman has height between 68 and
70 inches? (Moore, David S. The Basic Practice of Statistics. 4th
ed. New York: W. H. Freeman, 2007, pp. 258-259, example 10.9.)

-}

problem1 =
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

problem2 = plusFourOneSampleInterval 7 97 0.95

{-

Calculates a plus-four confidence interval for a proportion. Returns a
tuple containing the upper and lower limits of the interval.

-}
plusFourOneSampleInterval :: Int -> Int -> Double -> (Double, Double)
plusFourOneSampleInterval successes n c =
    let pTilde = fromIntegral (successes + 2) / fromIntegral (n + 4)
        z = density_1p Gaussian UppInv 1 ((1.0 - c) / 2.0)
        absErr = z * sqrt ((pTilde * (1.0 - pTilde)) / fromIntegral (n + 4))
    in (pTilde - absErr, pTilde + absErr)
       
{-

Problem 3: A random sample found 13,173 boys among 25,468 firstborn
children. The sample proportion of boys was therefore 0.5172. Is this
sample evidence that boys are more common than girls in the entire
population? (Moore, David S. The Basic Practice of Statistics. 4th
ed. New York: W. H. Freeman, 2007, p. 505, example 20.7.

-}

problem3 = sigTestProp 13173 25468 0.5 Greater
      
{-

Performs a significance test for a proportion, given the number of
successes, the size of the sample, the expected proportion in the null
hypothesis, and the alternative hypothesis. Returns the probability of
the sample proportion if the null hypothesis is true.

-}
sigTestProp :: Int -> Int -> Double -> Alternative -> Double
sigTestProp successes n p0 alternative =
    let pHat = (fromIntegral successes) / (fromIntegral n)
        z = (pHat - p0) / sqrt((p0 * (1.0 - p0)) / (fromIntegral n))
    in case alternative of
      Less -> density_1p Gaussian Lower 1 z
      Greater -> density_1p Gaussian Upper 1 z
      TwoSided -> 2.0 * density_1p Gaussian Upper 1 (abs z)

{-

Problem 4: Some shrubs can resprout from their roots after their tops
are destroyed. Fire is a serious threat to shrubs in dry climates, as
it can injure the roots as well as destroy the tops. One study of
resprouting took place in a dry area of Mexico. The investigators
randomly assigned shrubs to treatment and control groups. They clipped
the tops of all the shrubs. They then applied a propane torch to the
stumps of the treatment group to simulate a fire. A shrub is a success
if it resprouts. Here are the data for the shrub Xerospirea
hartwegiana:

             Population    Sample   Number of      Sample
Population   description    size    successes    proportion
------------------------------------------------------------------
     1         control       12        12        12/12=1.000
     2        treatment      12        8          8/12=0.667

How much does burning reduce the proportion of shrubs of this species
that resprout? Give a 90% confidence interval for the difference of
population proportions, p1 - p2. (Moore, David S. The Basic Practice
of Statistics. 4th ed. New York: W. H. Freeman, 2007, p. 518, example
21.3.)

-}

problem4 = plusFourTwoSampleInterval 12 12 8 12 0.90

{-

Calculates a plus-four confidence interval for the differences
between proportions in two samples. Returns a tuple containing the
upper and lower limits of the interval.

-}
plusFourTwoSampleInterval :: Int -> Int -> Int -> Int -> Double ->
                             (Double, Double)
plusFourTwoSampleInterval successes1 n1 successes2 n2 c =
  let pTilde1 = fromIntegral (successes1 + 1) / fromIntegral (n1 + 2)
      pTilde2 = fromIntegral (successes2 + 1) / fromIntegral (n2 + 2)
      se = sqrt (((pTilde1 * (1.0 - pTilde1)) / fromIntegral (n1 + 2)) +
                 ((pTilde2 * (1.0 - pTilde2)) / fromIntegral (n2 + 2)))
      z = density_1p Gaussian UppInv 1 ((1.0 - c) / 2.0)
      diff = pTilde1 - pTilde2
      absErr = z * se
  in (diff - absErr, diff + absErr)


main = do
  printf "Problem 1: P = %.4f\n" problem1
  
  let (lowerLim, upperLim) = problem2
  printf "Problem 2: 95%% confidence interval from %.4f to %.4f\n" lowerLim
    upperLim

  printf "Problem 3: P = %.10f\n" problem3

  let (lowerLim, upperLim) = problem4
  printf "Problem 4: 90%% confidence interval from %.4f to %.4f\n" lowerLim
    upperLim
