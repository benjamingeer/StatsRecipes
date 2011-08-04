#!/usr/bin/env python
# -*- coding: utf-8 -*-

# A two-sample Welch's t test, along with a confidence interval.
#
# Problem: How quickly do synthetic fabrics such as polyester decay in
# landfills? A researcher buried polyester strips in the soil for
# different lengths of time, then dug up the strips and measured the
# force required to break them. Part of the study buried 10 strips of
# polyester fabric in well-drained soil in the summer. Five of the
# strips, chosen at random, were dug up after 2 weeks; the other 5
# were dug up after 16 weeks. The breaking strengths in pounds for the
# two samples are given in the files fabric1.csv and fabric2.csv. We
# suspect that decay increases over time. Do the data give good
# evidence that mean breaking strength is less after 16 weeks than
# after 2 weeks? Give a one-sided P-value for the null hypothesis, and
# a 90% confidence interval for the difference between the sample
# means. (Moore, David S. The Basic Practice of Statistics. 4th
# ed. New York: W. H. Freeman, 2007, pp. 463 and 466-468, examples
# 19.2, 19.3 and 19.4.)
#
# This procedure doesn't seem to be implemented in any library, so we
# implement it here using the formulas given on
# <http://en.wikipedia.org/wiki/Welch%27s_t_test>. This gives the same
# results as the t.test function in R. We can't use
# scipy.stats.ttest_ind, because it uses n1 + n2 - 2 degrees of
# freedom, which is correct only if the two samples have equal
# variances.

import sys
import math
import numpy as np
import scipy.stats as stats
from math import sqrt

def main():
    if len(sys.argv) < 3:
        assert False, "Expected two data files"

    sample1_file = sys.argv[1]
    sample2_file = sys.argv[2]
    sample1 = read_data(sample1_file)
    sample2 = read_data(sample2_file)

    t, df, p, lower_lim, upper_lim = welch_test(sample1,
                                                sample2,
                                                alternative="greater",
                                                c=0.90)
    print "t: %.4f" % t
    print "Degrees of freedom: %.4f" % df
    print "P: %.4f" % p
    print "90%% confidence interval: %.4f to %.4f" % (lower_lim, upper_lim)


# Performs Welch's two-sample t-test on two lists and calculates a
# confidence interval, given the expected difference between the
# population means (μ1 - μ2, default 0), the alternative hypothesis
# ("less", "greater" or the default "two.sided") and the confidence
# level (default 0.95). Returns a tuple containing the t-value, the
# degrees of freedom, the probability, and the upper and lower limits
# of the confidence interval.
def welch_test(a1, a2, delta_mu=0, alternative="two.sided", c=0.95):
    n1 = len(a1)
    n2 = len(a2)
    x1_bar = a1.mean()
    x2_bar = a2.mean()
    var1 = a1.var(ddof = 1)
    var2 = a2.var(ddof = 1)

    test_t = ((x1_bar - x2_bar) - delta_mu) / sqrt((var1 / n1) + (var2 / n2))

    df = ((var1 / n1) + (var2 / n2))**2 / ((var1**2 / (n1**2 * (n1 - 1))) +
                                           (var2**2 / (n2**2 * (n2 - 1))))

    if alternative == "less":
        p = stats.t.cdf(test_t, df)
    elif alternative == "greater":
        p = stats.t.sf(test_t, df)
    elif alternative == "two.sided":
        p = 2 * stats.t.sf(abs(test_t), df)
    else:
        assert False, "Unexpected alternative: " + alternative

    interval_t = stats.t.interval(c, df)[1]
    abs_err = interval_t * sqrt((var1 / n1) + (var2 / n2))
    lower_lim = (x1_bar - x2_bar) - abs_err
    upper_lim = (x1_bar - x2_bar) + abs_err

    return (test_t, df, p, lower_lim, upper_lim)

def read_data(data_file):
    try:
        with open(data_file, "rb") as f:
            return np.array(map(float, f))
    except IOError as (errno, strerror):
        print "Error reading {0}: {1}" . format(data_file, strerror)
        sys.exit()

if __name__ == "__main__":
    main()
