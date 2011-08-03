#!/usr/bin/env python
# -*- coding: utf-8 -*-

# A one-sample t confidence interval.
#
# Biologists studying the healing of skin wounds measured the rate at
# which new cells closed a razor cut made in the skin of an
# anesthetized newt. The file newts.csv contains data from 18 newts,
# measured in micrometers per hour. The population distribution is
# Normal. Estimate the mean rate μ for all newts of this species by
# giving a 95% confidence interval.

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

    t_star = stats.t.interval(0.95, n - 1)[1]
    print "t*: %.4f" % t_star

    se = s / math.sqrt(n)
    abs_err = t_star * se
    lower_lim = x_bar - abs_err
    upper_lim = x_bar + abs_err
    print "Confidence interval: %.4f to %.4f" % (lower_lim, upper_lim)

def read_data(data_file):
    try:
        with open(data_file, "rb") as f:
            return numpy.array(map(float, f))
    except IOError as (errno, strerror):
        print "Error reading {0}: {1}" . format(data_file, strerror)
        sys.exit()

if __name__ == "__main__":
    main()
