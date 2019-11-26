# week 9: likelihood and log likelihood exercises from lecture notes
# likelihood of success of launch vehicles
(like.3 <- dbinom(3,11,0.3))  # 3 success from 11 attempts, pi = 0.3
# [1] 0.2568219

(like.8 <- dbinom(3,11,0.8)) # pi = 0.8
# [1] 0.0002162688

(ratio.like <- like.3 / like.8)
# [1] 1187.512

# can conlcude much more likely to see 3 successfull launcehs if pi = 0.3 rather than 0.8.
# therefore say pi = 0.3 is more PLAUSIBLE than pi = 0.8

# observed likelihood for binomial data
par(mfrow = c(2,1), mar = c(5.1, 4.1, 1.1,1.1))
curve(dbinom(3,11, prob = x), from = 0, to = 1, n = 401, xlab = expression(pi), ylab = "Pr[Y = 3]")
abline(v = 3/11)
curve(dbinom(3,11, prob = x, log = TRUE), from = 0, to = 1, n = 401, xlab = expression(pi), ylab = "log(Pr[Y = 3])")
abline(v = 3/11)

# observed likelihood for Poisson data
# example number of new disease cases reported per year
# R functions for the likelihood and proportional likelihood for a poisson observation

# including the multiplier 1 / y!
like.pois <- function(lambda, y) {
  like <- exp(-lambda) * lambda^y/factorial(y)
  return(like)
}
# excluding the multiplier 1 / y!
prop.like.pois <- function(lambda, y) {
  prop.like <- exp(-lambda) * lambda^y
  return(prop.like)
}

# R functions for the log likelihood and log likelihood without the constant for a poisson observation

# including the constant -log(3!)
log.like.pois <- function(lambda, y) {
  log.like <- - lambda + y * log(lambda) - lfactorial(y)
  return(log.like)
}

# excluding the constant -log(3!)
log.prop.like.pois <- function(lambda, y) {
  prop.log.like <- -lambda + y * log(lambda)
  return(prop.log.like)
}

# R commands to plot the likelihoods and log likelihoods with/ without the multiplier and constant 
# respectively for a poisson observation
lambda.grid <- seq(0.001, 6, by = 0.001)
par(mfrow = c(2,2))
plot(lambda.grid, like.pois(lambda.grid,3), type = 'l')
abline(v= 3)
plot(lambda.grid, prop.like.pois(lambda.grid, 3), type = 'l')
abline(v = 3)
plot(lambda.grid,log.like.pois(lambda.grid,3), type = 'l')
abline(v = 3)
plot(lambda.grid, log.prop.like.pois(lambda.grid,3), type = 'l')
abline(v = 3)
# same value of lambda maximises the functions in each plot

#---------------------------------------------------------
# log likelihood for binary data
# 2 players meet reguarly for a game of squash. results of last ten matches are 0,0,1,0,1,1,1,0,1,1

# R code to plot  the log likelihood functions
squash <- c(0,0,1,0,1,1,1,0,1,1)
n = length(squash)
y.sum <- sum(squash)
pi.grid <- seq(0,1,length = 201)

par(mfrow = c(1,2))

ber.lik <- pi.grid^y.sum * (1 - pi.grid) ^ (n - y.sum)
plot(pi.grid, log(ber.lik), xlab = expression(pi), ylab = 'Log Likelihood', type = 'l')
title('Bernoulli Model')

bin.like <- dbinom(y.sum, n, pi.grid)
plot(pi.grid, log(bin.lik), xlab = expression(pi), ylab = "Log Likelihood", type = 'l')
title("Binomial model")

#-------------------------------------
# R syntax for constructing likelihood and log likelihood for the 2 parameter soil data
Coated <- c(64,35)
Number <- c(84, 75)
pi1.grid <- seq(0.001, 0.999, length = 50)
pi2.grid <- seq(0.001, 0.999, length = 50)
log.lik <- matrix(0, ncol = 50, nrow = 50)
pmf <- matrix(0, ncol = 50, nrow = 50)

for (i in 1:50) {    # iterating over pi1
  for (j in 1:50) {    # iterating over pi2
    pmf[i,j] <- prod(dbinom(Coated, size = Number, prob = c(pi1.grid[i], pi2.grid[2])))
    log.lik[i,j] <- log(pmf[i,j])
  }
  
}
# R syntax for perspective plots of Likelihood and log likelihood for two parameter soil data
persp(pi1.grid, pi2.grid, pmf,
      main = 'Perspective plot of likelihood',
      ticktype = 'detailed', theta = 30, phi = 30, col = 'blue')
persp(pi1.grid, pi2.grid, log.lik,
      main = 'perspective plot of log likelihood',
      ticktype = 'detailed', theta = 30, phi = 30, col = 'blue')

# --------------------------------------------------------------------
# exercise 2: likelihood function for a raffle
# plot the likelihood function against different values of theta. code below generates the likelihood functio
# based on a single data point 'y'
# can see the max. likelihood estimate occurs at theta = y (in this case y = 87)
theta <- 1:200
y <- 87
like <- ifelse(theta >= y, 1/theta,0)
plot(theta, like, ylab = expression(L(theta)), xlab = expression(theta))
abline(v = 87, h = 1/y, col = 'blue')

# --------------------------------------------------------------------
# exercise 3: likelihood function for computer failures
# in R write a function to calculate likelihood and log likelihood
y <- c(1,5,1,4,2,3,1,3,6,4,4,4,2,3,2,2,4,5,5,2,5,3,2,2,3,1,2,5,1,4,1,1,1,2,1,3,2,5,3,5,2,5,1,1,5,2,3)
# function for the likelihood
like.pois <- function(lambda, y){
  n <- length(y)
  n.ybar <- n * mean(y)
  like <- exp(-n * lambda) * lambda^n.ybar
  return(like)
}

# function for log likelihood. calls the likelihood function
log.like.pois <- function(lambda, y) {
  log.lik <- log(like.pois(lambda,y))
  return(log.lik)
}

# plot the likelihood and th elog likelihood 
# set up a grid from lambda over a suitable range
lambda.grid <- seq(0.001, 10, by = 0.01)
# evaluate the likelihood and log likelihood over the values of lambda
lik <- like.pois(lambda.grid, y)
l <- log.like.pois(lambda.grid,y)
# plot the likelihood and log likelihood on the same plot
par(mfrow = c(2,1))
plot(lambda.grid, lik, ylab = expression(L(lambda)), xlab = expression(lambda), type = 'l')
abline(v = mean(y))
plot(lambda.grid, l, ylab = expression(L(lambda)), xlab = expression(lambda), type = 'l')
abline(v= mean(y))

# -------------------------------
# exercise 4: likelihood function for multiple births in cattle
cows <- c(364,183,8)
p1 <- seq(0.1,0.9,length = 50)
p2 <- seq(0.1,0.9,length = 50)
log.lik <- matrix(NA, ncol = 50, nrow = 50)

for (i in 1:50){
  for (j in 1:50) {
    if (p1[i] + p2[i] < 1)
      log.lik[i,j] <- cows[1] * log(p1[i]) + cows[2] * log(p2[j]) + cows[3] * log(1 - p1[i] - p2[j])
  }
}

persp(p1,p2, exp(log.lik), main = 'perspective plot', col = 'cyan', ticktype = 'detailed')
persp(p1, p2, log.lik, main = 'perspective plot', col='cyan', ticktype = 'detailed')
# the max. on th elikelihood plot roughly agrees with with the estimates of pi1 and pi2 (364/555 and 183/555)
# there is a region of the plot where the likelihood is 0 and log likelihood is undefined. This region 
# corresponds to values of pi1 and pi2 such that pi1 + pi2 >1 and outside the allowable range of these
# parameters
