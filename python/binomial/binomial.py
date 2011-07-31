#!/usr/bin/env python

from scipy.stats import binom

# Problem 1: Each child born to a particular set of parents has
# probability 0.25 of having blood type O. If these parents have 5
# children, what is the probability that exactly 2 of them have type O
# blood?

p1Prob = binom.pmf(2, 5, 0.25)

# Problem 2: A music distributor inspects an SRS of 10 CDs from a
# shipment of 10,000 music CDs. Suppose that (unknown to the
# distributor) 10% of the CDs in the shipment have defective
# copy-protection schemes that will harm personal computers. The
# number X of CDs with defective copy protection has approximately the
# binomial distribution with n = 10 and p = 0.1. What is the
# probability that the sample contains no more than 1 defective CD?

p2Prob = binom.cdf(1, 10, 0.1)
p2Mean = binom.mean(10, 0.1)
p2StdDev = binom.std(10, 0.1)

def main():
    print "Problem 1: probability %.4f\n" % p1Prob
    print "Problem 2: probability %.4f" % p2Prob
    print "           mean %.4f" % p2Mean
    print "           standard dev. %.4f" % p2StdDev

if __name__ == "__main__":
    main()
