#!/usr/bin/env python

# Statistics for regression inference.

# Problem: Child development researchers explored the relationship
# between the crying of infants four to ten days old and their later
# IQ test scores. A snap of a rubber band on the sole of the foot
# caused the infants to cry. The researchers recorded the crying and
# measured its intensity by the number of peaks in the most active 20
# seconds. They later measured the children's IQ at age three years
# using the Stanford-Binet IQ test. Do children with higher crying
# counts tend to have higher IQ? (Moore, David S. The Basic Practice
# of Statistics. 4th ed. New York: W. H. Freeman, 2007, pp. 581-592,
# exercises 24.1-24.4.)

import numpy as np
import scipy.stats as stats
import csv
import sys

def main():
    if len(sys.argv) < 2:
        assert False, "Expected data file"

    data_file = sys.argv[1]
    xs, ys = read_data(data_file)

    (a, b, r, seB, p, lower_lim, upper_lim) = linear_regression(xs, ys, 0.95)

    print "Slope: %.4f" % b
    print "Intercept: %.4f" % a
    print "r: %.4f" % r
    print "Standard error of the slope: %.4f" % seB
    print "P (two-sided): %.4f" % p
    print "P (one-sided): %.4f" % (p / 2.0)
    print "95%% confidence interval for the slope: from %.4f to %.4f" % \
        (lower_lim, upper_lim)

def linear_regression(xs, ys, c=0.95):
    b, a, r, p, seB = stats.linregress(xs, ys)
    df = len(xs) - 2
    t_confidence = stats.t.interval(c, df)[1]
    abs_err = t_confidence * seB
    lower_lim = b - abs_err
    upper_lim = b + abs_err
    return (a, b, r, seB, p, lower_lim, upper_lim)

def read_data(data_file):
    xs = []
    ys = []

    try:
        with open(data_file, "rb") as f:
            reader = csv.reader(f)
            
            for row in reader:
                xs.append(int(row[0]))
                ys.append(int(row[1]))
    except IOError as (errno, strerror):
        print "Error reading {0}: {1}" . format(filename, strerror)
        sys.exit()
    
    return (np.array(xs), np.array(ys))

if __name__ == "__main__":
    main()
