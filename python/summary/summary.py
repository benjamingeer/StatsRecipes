#!/usr/bin/env python

# Prints summary statistics about a sample. (Moore, David S. The Basic
# Practice of Statistics. 4th ed. New York: W. H. Freeman, 2007,
# p. 43.)
#
# Cf. the "summary" function in R.

import sys
import numpy
import scipy.stats as stats

class Desc:
    def __init__(self, a):
        self.min = a.min()
        self.q1 = stats.scoreatpercentile(a, 25)
        self.median = numpy.median(a)
        self.mean = a.mean()
        self.q3 = stats.scoreatpercentile(a, 75)
        self.max = a.max()

def main():
    if len(sys.argv) != 2:
        assert False, "Expected data file"

    data_file = sys.argv[1]
    data = read_data(data_file)
    d = Desc(data)

    print "Minimum: %.2f" % d.min
    print "First quartile: %.2f" % d.q1
    print "Median: %.2f" % d.median
    print "Mean: %.2f" % d.mean
    print "Third quartile: %.2f" % d.q3
    print "Maximum: %.2f" % d.max

# Parse the numbers in the data file
def read_data(data_file):
    try:
        with open(data_file, "rb") as f:
            return numpy.array(map(float, f))
    except IOError as (errno, strerror):
        print "Error reading {0}: {1}" . format(data_file, strerror)
        sys.exit()

if __name__ == "__main__":
    main()
