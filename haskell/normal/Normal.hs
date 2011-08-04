{-

If women's height has the N(64, 2.7) distribution, what is the
probability that a randomly chosen woman has height between 68 and 70
inches? (Moore, David S. The Basic Practice of Statistics. 4th ed. New
York: W. H. Freeman, 2007, pp. 258-259, example 10.9.)

-}

module Main where

import Numeric.GSL.Distribution.Common
import Numeric.GSL.Distribution.Continuous
import Text.Printf

p =
  let mean = 64.0
      stdDev = 2.7
  in (density_1p Gaussian Lower stdDev (70 - mean)) -
     (density_1p Gaussian Lower stdDev (68 - mean))

main = do
  printf "Probability: %.4f\n" p
