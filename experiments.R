# Help here:
#
# https://en.wikipedia.org/wiki/Beta_distribution
#
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Beta.html
#
# https://www.rdocumentation.org/packages/LearnBayes/versions/2.15/topics/beta.select
#
# http://rstudio-pubs-static.s3.amazonaws.com/3504_fc4473790d7c43ed87b102e3c4857e45.html
#
# https://www.rdocumentation.org/packages/MASS/versions/7.3-47/topics/fitdistr

# Clear the console, and the environment.  Take this out when development is
# complete.
cat("\014")
rm(list=ls())

library(LearnBayes)
library(MASS)

# Here are the real 'a' and 'b'.
real_alpha <- 20.
real_beta <- 80.

# Okay, here we try to guess the quantiles of the resulting beta distrubtion.
# For the second quantile, it's just a guess that we add to the median...
# median <- real_alpha / (real_alpha + real_beta)
# quantile1 <- list(p = 0.5, x = median)
# quantile2 <- list(p = 0.9, x = median + 0.2)

my_guesses <- pbeta(seq(0., 1., 0.1), real_alpha, real_beta)
quantile1 <- list(p = my_guesses[2], x = 0.2)
quantile2 <- list(p = my_guesses[4], x = 0.4)

# Now we arrive the Bayesian prior alpha and beta based on our guess. And then
# we create five points from a beta distribution with the real alpha and beta.
initial <- beta.select(quantile1, quantile2)
points <- rbeta(5, real_alpha, real_beta)

# Great! Now try to fit a distribution with a starting parameters based on our
# guess
distribution <- fitdistr(points, 'beta',
                         start = list(shape1 = initial[1],
                                      shape2 = initial[2])
                         ) # ,
                         # lower = c(0., 0.),
                         # upper = c(10., 10.))

# How close did we get?
my_estimate <- distribution$estimate
cat(sprintf('Guesses are %f and %f.', my_estimate[1], my_estimate[2]))

# Actual results from Alabama, 2000 to 2016
points2 <- c(0.4239, 0.3710, 0.3911, 0.3878, 0.3563)
points2_mean <- mean(points2)
q1 <- list(p = 0.5, x = points2_mean)
q2 <- list(p = 0.9, x = points2_mean + 0.04)
initial2 <- beta.select(q1, q2)
initial2
