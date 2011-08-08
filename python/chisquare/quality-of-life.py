#!/usr/bin/env python

# Problem: A comparison of random samples of 2600 U.S. and 400
# Canadian heart attack patients looked at outcomes a year after the
# heart attack, including patients' own assessment of their quality of
# life relative to what it had been before the heart attack. File
# quality-of-life.csv contains the data for the patients who survived
# a year. Is there a significant difference between the two
# distributions of outcomes? (Moore, David S. The Basic Practice of
# Statistics. 4th ed. New York: W. H. Freeman, 2007, pp. 547-558,
# examples 23.1-23.4.)

import numpy as np
import scipy.stats as stats
import csv
import sys

def main():
    if len(sys.argv) < 2:
        assert False, "Expected data file"

    data_file = sys.argv[1]
    obs = read_data(data_file)

    chi2, p, df, ex = stats.chi2_contingency(obs)

    print "Chi-square: %.4f" % chi2
    print "Degrees of freedom: %.4f" % df
    print "P: %.4f" % p

def read_data(data_file):
    canada = []
    us = []

    try:
        with open(data_file, "rb") as f:
            reader = csv.reader(f)
            reader.next() # Skip the header
            
            for row in reader:
                canada.append(float(row[1]))
                us.append(float(row[2]))
    except IOError as (errno, strerror):
        print "Error reading {0}: {1}" . format(filename, strerror)
        sys.exit()

    return [np.array(canada), np.array(us)]


if __name__ == "__main__":
    main()
