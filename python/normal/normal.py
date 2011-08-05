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
    lower_lim, upper_lim = plus_four_interval(7, 97, 0.95)
    print "Problem 2: 95%% confidence interval from %.4f to %.4f" % (lower_lim,
                                                                     upper_lim)

# Calculates a plus-four confidence interval for a proportion. Returns a
# tuple containing the upper and lower limits of the interval.
def plus_four_interval(successes, n, c):
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
    print "Problem 3: z = %.10f" % z

    if alternative == "less":
        probability = stats.norm.cdf(z)
    elif alternative == "greater":
        probability = stats.norm.sf(z)
    elif alternative == "two.sided":
        probability = 2 * stats.norm.sf(abs(z))
    else:
        assert False, "Unexpected alternative: " + alternative

    return probability

def main():
    problem_1()
    problem_2()
    problem_3()

if __name__ == "__main__":
    main()
