#!/usr/bin/env python
# -*- coding: utf-8 -*-

# A one-sample t test.
#
# Cola makers test new recipes for loss of sweetness during
# storage. Trained tasters rate the sweetness before and after
# storage. File cola.csv contains the sweetness losses (sweetness
# before storage minus sweetness after storage) found by 10 tasters
# for one new cola recipe. Are these data good evidence that the cola
# lost sweetness?

import sys
import math
import numpy
import scipy.stats as stats

def main():
    if len(sys.argv) < 2:
        assert False, "Expected data file"

    data_file = sys.argv[1]
    data = read_data(data_file)

    n = len(data)
    x_bar = data.mean()
    print "Sample mean: %.4f" % x_bar

    s = data.std(ddof = 1)
    print "Sample standard deviation: %.4f" % s

    se = s / math.sqrt(n)
    t = x_bar / se
    print "One-stample t statistic: %.4f" % t

    p = stats.t.sf(t, n - 1)
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
