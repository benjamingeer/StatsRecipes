#!/usr/bin/env python

# If women's height has the N(64, 2.7) distribution, what is the
# probability that a randomly chosen woman has height between 68 and
# 70 inches?

import numpy
import scipy.stats as stats

def main():
    mean = 64
    std_dev = 2.7
    normal_dist = stats.norm(loc = mean, scale = std_dev)
    p = normal_dist.cdf(70) - normal_dist.cdf(68)
    print "Probability: %.4f" % p

if __name__ == "__main__":
    main()
