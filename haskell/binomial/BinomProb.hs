{-

Demonstrates binomial distributions.

-}

module Main where

import Statistics.Distribution
import Statistics.Distribution.Binomial
import Text.Printf (printf)

{-

Problem 1: Each child born to a particular set of parents has
probability 0.25 of having blood type O. If these parents have 5
children, what is the probability that exactly 2 of them have type O
blood?

-}

p1Bin = binomial 5 0.25
p1Prob = probability p1Bin 2

{-

Problem 2: A music distributor inspects an SRS of 10 CDs from a
shipment of 10,000 music CDs. Suppose that (unknown to the
distributor) 10% of the CDs in the shipment have defective
copy-protection schemes that will harm personal computers. The number
X of CDs with defective copy protection has approximately the binomial
distribution with n = 10 and p = 0.1. What is the probability that the
sample contains no more than 1 defective CD? What are the mean and
standard deviation of this distribution?

-}

p2Bin = binomial 10 0.1
p2Prob = cumulative p2Bin 1
p2Mean = mean p2Bin
p2StdDev = sqrt (variance p2Bin)

main = do
  printf "Problem 1: probability %.4f\n\n" p1Prob
  printf "Problem 2: probability %.4f\n" p2Prob
  printf "           mean %.4f\n" p2Mean
  printf "           standard dev. %.4f\n" p2StdDev
