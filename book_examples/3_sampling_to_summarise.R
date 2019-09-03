library(rethinking)
library(magrittr)

########################################################################
## 3.1 Sampling from a grid-aaproximate posterior
########################################################################

## Generate the posterior distribution with uniform prior and
## likelihood of observing 6 success out of 9.
n = 9
success = 3
p_grid = seq(from = 0, to = 1, length.out = 1000)
prior = rep(1, 1000)
likelihood = dbinom(success, size = n, prob = p_grid)
posterior = prior * likelihood
posterior = posterior/sum(posterior)

## Sample from the posterior distribution to make inference
sample_size = 10000
samples = sample(p_grid, prob = posterior, size = sample_size, replace = TRUE)
par(mfrow = c(1, 2))
plot(samples)
dens(samples)
abline(v = success/n, col = "red", lty = 2)

########################################################################
## 3.2 sampling to summarise
########################################################################

## distribution function

## Cumulative distribution pxxx(0.5), but this is impractical in high
## dimension setting.
sum(posterior[p_grid < 0.5])

## We can do the proportion of sample that is less than 0.5 from the
## sampled distribution. The numbers are very close in this case.
sum(samples < 0.5)/sample_size

## We can also do between 2 percentile.
sum(samples >= 0.5 & samples < 0.75)/sample_size


## Interval

## Calculate the percentile interval (PI), this is equivalent to
## taking the upper and the lower boundary.
##
## prob of 0.5 = 0.25 to 0.75 percentile.
PI(samples, prob = 0.5)

## The PI works well if the distribution is symmetric and not
## skewed. However, if we want to obtain the region in which the
## parameter is most likely to be contained, then the Highest
## Posterior Density Interval (HPDI) is a better measure.
HPDI(samples, prob = 0.5)

## Point estimates

## Maximum a posteriori (MAP), which is the value with the highest
## posterior probability.
p_grid[which.max(posterior)]


## Use density estimation to estimate the highest point in the
## posterior sample.
chainmode(samples, adj = 0.01)


## But why just mean, why not median?
mean(samples)
median(samples)


## If we are to use a threshold such as p = 0.5 (often in the case of
## binary classification), then the cost of using mean would be:
sum(abs(p_grid - 0.5) * posterior)

## Now if were to calculate the loss for each decision (any p)
loss = sapply(p_grid, function(p) sum(abs(p_grid - p) * posterior))

## This is the point which minimises the loss, and it should conincide
## with the posterior median
p_grid[which.min(loss)]

## Different loss function resutls in different point estimate.
##
## L1-norm |d - p|: median
##
## L2-norm (d - p)^2: mean
##
## What that means, we can choose our loss function and then find the
## optimal point estimate with respect to the loss function.


########################################################################
## 3.3 Sampling to simulate prediction
########################################################################

## posterior is the distribution of the parameter P(p|x)
##
## posterior predictive distribution is the distribution of the
## probability of the observation. P(x|p_u).

## Let's simulate the posterior predictvie distritbuion.
ppd = rbinom(10000, size = 9, prob = samples)
table(ppd) %>% plot
