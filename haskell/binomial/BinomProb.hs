{-

Demonstrates binomial distributions.

-}

module Main where

import Statistics.Distribution
import Statistics.Distribution.Binomial
import Text.Printf (printf)

{-

Each child born to a particular set of parents has probability 0.25 of
having blood type O. If these parents have 5 children, what is the
probability that exactly 2 of them have type O blood?

-}

b1 = binomial 5 0.25
p1 = probability b1 2

{-

A music distributor inspects an SRS of 10 CDs from a shipment of
10,000 music CDs. Suppose that (unknown to the distributor) 10% of the
CDs in the shipment have defective copy-protection schemes that will
harm personal computers. The number X of CDs with defective copy
protection has approximately the binomial distribution with n = 10 and
p = 0.1. What is the probability that the sample contains no more than
1 defective CD?

-}

b2 = binomial 10 0.1
p2 = cumulative b2 1

main = do
  printf "Probability 1: %.4f\n" p1
  printf "Probability 2: %.4f\n" p2
