#!/usr/bin/env python

# Calculations using Normal distributions.

from math import sqrt
import scipy.stats as stats

# Problem 1: If women's height has the N(64, 2.7) distribution, what is the
# probability that a randomly chosen woman has height between 68 and
# 70 inches? (Moore, David S. The Basic Practice of Statistics. 4th
# ed. New York: W. H. Freeman, 2007, pp. 258-259, example 10.9.)

def problem_1():
    mean = 64
    std_dev = 2.7
    normal_dist = stats.norm(loc = mean, scale = std_dev)
    p = normal_dist.cdf(70) - normal_dist.cdf(68)
    print "Problem 1: P = %.4f" % p

# Problem 2: Investigators looked at a random sample of 97 articles
# reporting on placebo-controlled randomized trials in the top five
# general medical journals. Only 7 of the 97 discussed the success of
# blinding, and in 5 of these the blinding was imperfect. What
# proportion of all such studies discuss the success of blinding? Give
# a 95% confidence interval. (Moore, David S. The Basic Practice of
# Statistics. 4th ed. New York: W. H. Freeman, 2007, p. 500,
# example 20.5.)

def problem_2():
    lower_lim, upper_lim = plus_four_one_sample_interval(7, 97, 0.95)
    print "Problem 2: 95%% confidence interval from %.4f to %.4f" % (lower_lim,
                                                                     upper_lim)

# Calculates a plus-four confidence interval for a proportion. Returns a
# tuple containing the upper and lower limits of the interval.
def plus_four_one_sample_interval(successes, n, c):
    p_tilde = float(successes + 2) / float(n + 4)
    z = stats.norm.interval(c)[1]
    abs_err = z * sqrt((p_tilde * (1.0 - p_tilde)) / float(n + 4))
    return (p_tilde - abs_err, p_tilde + abs_err)

# Problem 3: A random sample found 13,173 boys among 25,468 firstborn
# children. The sample proportion of boys was therefore 0.5172. Is
# this sample evidence that boys are more common than girls in the
# entire population? (Moore, David S. The Basic Practice of
# Statistics. 4th ed. New York: W. H. Freeman, 2007, p. 505, example
# 20.7.)

def problem_3():
    probability = sig_test_prop(13173, 25468, 0.5, alternative="greater")
    print "Problem 3: P = %.10f" % probability

# Performs a significance test for a proportion, given the number of
# successes, the size of the sample, the expected proportion in the
# null hypothesis, and the alternative hypothesis ("less", "greater"
# or the default "two.sided"). Returns the probability of the sample
# proportion if the null hypothesis is true.
def sig_test_prop(successes, n, p0, alternative="two.sided"):
    p_hat = float(successes) / float(n)
    z = (p_hat - p0) / sqrt((p0 * (1.0 - p0)) / float(n))

    if alternative == "less":
        probability = stats.norm.cdf(z)
    elif alternative == "greater":
        probability = stats.norm.sf(z)
    elif alternative == "two.sided":
        probability = 2 * stats.norm.sf(abs(z))
    else:
        assert False, "Unexpected alternative: " + alternative

    return probability

# Problem 4: Some shrubs can resprout from their roots after their
# tops are destroyed. Fire is a serious threat to shrubs in dry
# climates, as it can injure the roots as well as destroy the
# tops. One study of resprouting took place in a dry area of
# Mexico. The investigators randomly assigned shrubs to treatment and
# control groups. They clipped the tops of all the shrubs. They then
# applied a propane torch to the stumps of the treatment group to
# simulate a fire. A shrub is a success if it resprouts. Here are the
# data for the shrub Xerospirea hartwegiana:
#
#              Population    Sample   Number of      Sample
# Population   description    size    successes    proportion
# ------------------------------------------------------------------
#      1         control       12        12        12/12=1.000
#      2        treatment      12        8          8/12=0.667
#
# How much does burning reduce the proportion of shrubs of this
# species that resprout? Give a 90% confidence interval for the
# difference of population proportions, p1 - p2. (Moore, David S. The
# Basic Practice of Statistics. 4th ed. New York: W. H. Freeman, 2007,
# p. 518, example 21.3.)

def problem_4():
    lower_lim, upper_lim = plus_four_two_sample_interval(12, 12, 8, 12, 0.90)
    print "Problem 4: 90%% confidence interval from %.4f to %.4f" % (lower_lim,
                                                                     upper_lim)

# Calculates a plus-four confidence interval for the differences
# between proportions in two samples. Returns a tuple containing the
# upper and lower limits of the interval.
def plus_four_two_sample_interval(successes1, n1, successes2, n2, c):
    p_tilde1 = float(successes1 + 1) / float(n1 + 2)
    p_tilde2 = float(successes2 + 1) / float(n2 + 2)
    se = sqrt(((p_tilde1 * (1.0 - p_tilde1)) / float(n1 + 2)) +
              ((p_tilde2 * (1.0 - p_tilde2)) / float(n2 + 2)))
    z = stats.norm.interval(c)[1]
    diff = p_tilde1 - p_tilde2
    abs_err = z * se
    return (diff - abs_err, diff + abs_err)


def main():
    problem_1()
    problem_2()
    problem_3()
    problem_4()

if __name__ == "__main__":
    main()
