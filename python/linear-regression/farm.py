#!/usr/bin/env python

# Reads x and y values from a two-column CSV file, does a
# least-squares linear regression using SciPy and plots the data as a
# scatterplot along with the regression line. Uses Matplotlib's TeX
# renderer to generate the plot as an Embedded PostScript file, which
# can then be included in a LaTeX document.

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
    year, pop = read_data(data_file)

    # Do the calculation
    intercept, slope = linear_regression(year, pop)

    # Generate the plot
    plot(year, pop, intercept, slope, output_file)

# Set some Matplotlib configuration parameters
def config_mpl():
    mpl.rc("text", usetex=True) # Use the TeX renderer
    mpl.rc("font", family="serif")
    mpl.rc("ps", usedistiller="xpdf") # Make scalable output
    # mpl.rc("xtick.major", pad=8)
    # mpl.rc("ytick.major", pad=8)

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
    year = []
    pop = []

    try:
        with open(data_file, "rb") as f:
            reader = csv.reader(f)
            
            for row in reader:
                row_year = int(row[0])
                row_pop = float(row[1])
                year.append(row_year)
                pop.append(row_pop)
    except IOError as (errno, strerror):
        print "Error reading {0}: {1}" . format(filename, strerror)
        sys.exit()
    
    return (numpy.array(year), numpy.array(pop))

# Fit the regression line
def linear_regression(year, pop):
    slope, intercept, r, p, std_err = stats.linregress(year, pop)
    return intercept, slope

# Generate the plot
def plot(year, pop, intercept, slope, output_file):
    plt.axis(xmin = year[0], xmax = year[-1])
    plt.xticks(year)
    scatter_plot = plt.scatter(year, pop)
    regression_line = plt.plot([year[0], year[-1]],
                               [slope * x + intercept
                                for x in [year[0], year[-1]]],
                               'r-')
    plt.xlabel("Year")
    plt.ylabel("Millions of Persons")
    plt.legend([scatter_plot, regression_line], ["Population", "Regression"])
    plt.savefig(output_file)

if __name__ == "__main__":
    main()
