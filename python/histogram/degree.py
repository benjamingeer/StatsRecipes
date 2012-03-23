#!/usr/bin/env python

# Problem: The file degree.csv contains data on the percent of
# population aged 25 and over with a bachelor's degree in each US
# state. Make a histogram of this data. (Moore, David S. The Basic
# Practice of Statistics. 4th ed. New York: W. H. Freeman, 2007,
# pp. 11-12, example 1.5.)
#
# Here we use Matplotlib's TeX renderer to generate the plot as an
# Embedded PostScript file, which will then be included in a LaTeX
# document.
#
# Cf. the "hist" function in R.

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy
import scipy.stats as stats
import csv
import sys
import getopt

def main():
    # Configure Matplotlib
    config_mpl()

    # Get the command-line options
    data_file, output_file = get_options()

    # Read the data
    state, percent = read_data(data_file)

    # Generate the plot
    plot_histogram(percent, output_file)

# Set some Matplotlib configuration parameters
def config_mpl():
    mpl.rc("text", usetex=True) # Use the TeX renderer
    mpl.rc("font", family="serif")
    mpl.rc("ps", usedistiller="xpdf") # Make scalable output
    mpl.rc("xtick.major", pad=8)
    mpl.rc("ytick.major", pad=8)

# Command-line option processing
def get_options():
    try:
        opts, args = getopt.getopt(sys.argv[1:],
                                   "d:o:",
                                   ["data=",
                                    "output="])
    except getopt.GetoptError, err:
        print str(err)
        usage()
        sys.exit(2)

    data_file = None
    output_file = None

    for o, a in opts:
        if o in ("-d", "--data"):
            data_file = a
        elif o in ("-o", "--output"):
            output_file = a
        else:
            assert False, "unhandled option"

    if data_file is None:
        assert False, "data filename required"
    elif output_file is None:
        assert False, "output filename required"
    else:
        return data_file, output_file

# Parse the numbers in the data file
def read_data(data_file):
    state = []
    percent = []

    try:
        with open(data_file, "rb") as f:
            reader = csv.reader(f)
            
            for row in reader:
                row_state = row[0]
                row_percent = float(row[1])
                state.append(row_state)
                percent.append(row_percent)
    except IOError as (errno, strerror):
        print "Error reading {0}: {1}" . format(filename, strerror)
        sys.exit()
 
    return (state, numpy.array(percent))

# Generate the plot
def plot_histogram(percent, output_file):
    plt.hist(percent, bins=12, range=(15, 45))
    plt.xlabel("Percent of adults with bachelor's degree")
    plt.ylabel("Number of states")
    plt.savefig(output_file)

if __name__ == "__main__":
    main()
