#!/usr/bin/env python
# -*- coding: utf-8 -*-

# A one-sample t test.
#
# Cola makers test new recipes for loss of sweetness during
# storage. Trained tasters rate the sweetness before and after
# storage. File cola.csv contains the sweetness losses (sweetness
# before storage minus sweetness after storage) found by 10 tasters
# for one new cola recipe. Are these data good evidence that the cola
# lost sweetness? (Moore, David S. The Basic Practice of
# Statistics. 4th ed. New York: W. H. Freeman, 2007, pp. 440-441,
# example 18.3.)

import sys
import math
import numpy
import scipy.stats as stats

# Given an array of data and the expected mean in the null hypothesis,
# returns the sample mean, the t statistic and the probability of the
# sample mean if the null hypothesis is true.
def one_sample_t_test(sample, mu0):
    n = len(sample)
    x_bar = sample.mean()
    s = sample.std(ddof = 1)
    se = s / math.sqrt(n)
    t = (x_bar - mu0) / se
    p = stats.t.sf(t, n - 1)
    return (x_bar, t, p)

def main():
    if len(sys.argv) < 2:
        assert False, "Expected data file"

    data_file = sys.argv[1]
    sample = read_data(data_file)

    x_bar, t, p = one_sample_t_test(sample, 0)

    print "Sample mean: %.4f" % x_bar
    print "One-stample t statistic: %.4f" % t
    print "P: %.4f" % p

def read_data(data_file):
    try:
        with open(data_file, "rb") as f:
            return numpy.array(map(float, f))
    except IOError as (errno, strerror):
        print "Error reading {0}: {1}" . format(data_file, strerror)
        sys.exit()

if __name__ == "__main__":
    main()
