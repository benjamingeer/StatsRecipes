#!/usr/bin/env python

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
        self.std_dev = a.std(ddof = 1)

data = numpy.array([17.2, 18.1, 16.5, 18.3, 12.6])

def main():
    d = Desc(data)
    print "Minimum: %.2f" % d.min
    print "First quartile: %.2f" % d.q1
    print "Median: %.2f" % d.median
    print "Mean: %.2f" % d.mean
    print "Third quartile: %.2f" % d.q3
    print "Maximum: %.2f" % d.max
    print "Standard deviation: %.3f" % d.std_dev

if __name__ == "__main__":
    main()
