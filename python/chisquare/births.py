#!/usr/bin/env python

# Problem: A random sample of 140 births from local records shows this
# distribution across the days of the week, represented in the file
# births.csv. Do these data give significant evidence that local
# births are not equally likely on all days of the week? (Moore, David
# S. The Basic Practice of Statistics. 4th ed. New York:
# W. H. Freeman, 2007, pp. 566-567, example 23.7.)

import numpy as np
import scipy.stats as stats
import csv
import sys

def main():
    if len(sys.argv) < 2:
        assert False, "Expected data file"

    data_file = sys.argv[1]
    obs = read_data(data_file)

    chi2, p = stats.chisquare(obs)

    print "Chi-square: %.4f" % chi2
    print "P: %.4f" % p

def read_data(data_file):
    obs = []

    try:
        with open(data_file, "rb") as f:
            reader = csv.reader(f)
            reader.next() # Skip the header
            obs = map(int, reader.next()[1:])
    except IOError as (errno, strerror):
        print "Error reading {0}: {1}" . format(filename, strerror)
        sys.exit()

    return np.array(obs)


if __name__ == "__main__":
    main()
