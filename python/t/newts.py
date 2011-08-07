#!/usr/bin/env python
# -*- coding: utf-8 -*-

# A one-sample t confidence interval.
#
# Biologists studying the healing of skin wounds measured the rate at
# which new cells closed a razor cut made in the skin of an
# anesthetized newt. The file newts.csv contains data from 18 newts,
# measured in micrometers per hour. The population distribution is
# Normal. Estimate the mean rate μ for all newts of this species by
# giving a 95% confidence interval. (Moore, David S. The Basic
# Practice of Statistics. 4th ed. New York: W. H. Freeman, 2007,
# pp. 437-438, example 18.2.)

import sys
import math
import numpy
import scipy.stats as stats

# Given an array of data and a confidence level, returns the sample
# mean, the t statistic, and the lower and upper limits of the t
# confidence interval.
def one_sample_t_interval(sample, c):
    n = len(sample)
    x_bar = sample.mean()
    s = sample.std(ddof = 1)
    t = stats.t.interval(c, n - 1)[1]
    se = s / math.sqrt(n)
    abs_err = t * se
    lower_lim = x_bar - abs_err
    upper_lim = x_bar + abs_err
    return (x_bar, t, lower_lim, upper_lim)

def main():
    if len(sys.argv) != 2:
        assert False, "Expected data file"

    data_file = sys.argv[1]
    sample = read_data(data_file)

    x_bar, t, lower_lim, upper_lim = one_sample_t_interval(sample, 0.95)
    print "Sample mean: %.4f" % x_bar
    print "t: %.4f" % t
    print "95%% confidence interval: %.4f to %.4f" % (lower_lim, upper_lim)

def read_data(data_file):
    try:
        with open(data_file, "rb") as f:
            return numpy.array(map(float, f))
    except IOError as (errno, strerror):
        print "Error reading {0}: {1}" . format(data_file, strerror)
        sys.exit()

if __name__ == "__main__":
    main()
