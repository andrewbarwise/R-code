# Lab 10: Maximum likelihood estimation; Analytic and Numerical Solutions
# exercise 1: likelihood and log likelihood exercises from lecture notes
# plot likelihood and log likelihood functions for pi(1)
curve(dbinom(64, size = 84, prob = x), from = 0, to = 1, n = 301, main = 'likelihood',
      xlab = expression(pi[1]), ylab = expression(paste("L("*pi[1] *")")))
abline(v=64/84)
curve(dbinom(64, size = 84, prob = x, log = TRUE), from = 0, to = 1, n = 301, main = 'log- likelihood',
      xlab = expression(pi[1]), ylab = expression(paste("l(" * pi[1] * ")")))
abline(v= 64/84)

# ML estimate for zero truncated poisson data
score.fun <- function(lam, ybar, n) {
  f <- -n + n * ybar/lam - n * exp(-lam)/(1 - exp(-lam))
  return(f)
}
curve(score.fun(lam = x, ybar = 1.7, n = 50), from = 0.5, to = 4, n= 401, main= 'plot of score function')
abline(0,0,lty = 2)
uniroot(score.fun, interval = c(0.5,4), ybar = 1.7, n = 50)$root
# [1] 1.175003 
# uniroot() finds the root of a function in R

# numerical methods: cow litter data in R
library(stats4)
# implements the negative of the log likelihood
cow.neg.ll <- function(p1,p2) {
  ll <- 364 * log(p1) + 183 * log(p2) + 8 * log(1- p1 - p2)
  - ll
}
mle(cow.neg.ll, start = list(p1 = 0.5, p2 = 0.2))
# Call:
# mle(minuslogl = cow.neg.ll, start = list(p1 = 0.5, p2 = 0.2))

# Coefficients:
#   p1        p2 
# 0.6558403 0.3297226
# compare these to 364/555 = 0.655 and 183/555 = 0.32

# use R syntax to calculate the likelihood and log likelihood for the elephant mating data
library(Sleuth3)
elephants.lik <- function(b0,b1) {
  lambda <- exp(b0 + b1 * case2201$Age)
  lik <- prod(dpois(case2201$Matings, lambda))
  return(lik)
}
b0.grid <- seq(-1.76, -1.4, length = 50) 
b1.grid <- seq(0.06, 0.076, length = 50)
lik <- matrix(0, nrow = 50, ncol = 50)
for (i in 1:50) {
  for (j in 1:50) {
    lik[i,j] <- elephants.lik(b0.grid[i], b1.grid[j])
  }
}
# plot the log likelihood for the elephant data
persp(b0.grid, b1.grid, lik, main = 'perspective plot', col = 'cyan', ticktype = 'detailed', theta = 225)
persp(b0.grid, b1.grid, log(lik), main = 'perspective plot', col = 'cyan', ticktype = 'detailed', theta = 225)

# maximum likelihood estimates: numerical calculation
elephants.neg.ll <- function(b0, b1) {
  lambda <- exp(b0 + b1 * case2201$Age)
  ll <- sum(dpois(case2201$Matings, lambda, log = TRUE))
  return(-ll)
}
summary(mle(elephants.neg.ll, start = list(b0 = 0, b1 = 0)))
# Maximum likelihood estimation
#
# Call:
#  mle(minuslogl = elephants.neg.ll, start = list(b0 = 0, b1 = 0))
#
# Coefficients:
#   Estimate Std. Error
# b0 -1.57323618 0.54417884
# b1  0.06847259 0.01373422
#
# -2 log L: 152.4581
# compare this to the glm()
summary(glm(Matings ~ Age, data = case2201, family = poisson(link = log)))
# Call:
# glm(formula = Matings ~ Age, family = poisson(link = log), data = case2201)
#
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.80798  -0.86137  -0.08629   0.60087   2.17777  
#
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -1.58201    0.54462  -2.905  0.00368 ** 
# Age          0.06869    0.01375   4.997 5.81e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for poisson family taken to be 1)
#
# Null deviance: 75.372  on 40  degrees of freedom
# Residual deviance: 51.012  on 39  degrees of freedom
# AIC: 156.46
#
# Number of Fisher Scoring iterations: 5

#-----------------------------------------------------------------
# exercise 2: likelihood function for the hardy - weinberg law
# y1 = 342, y2 = 500, y3 = 187, n = 1029
# l(theta) = (2y1 + y2) * log(1-theta) + (2y3 + y2) * log(theta) + c)

# function to evaluate l(theta) with the constant c = 0
blood.ll <- function(theta) {
  ll <- 1184 * log(1- theta) + 874 * log(theta)
  ll
}

# write a function to evaluate -l(theta)
blood.neg.ll <- function(theta) -blood.ll(theta)

# use mle()to find teh MLE of theta and its SE
library(stats4)
res <- mle(blood.neg.ll, start = list(theta = 0.5))
summary(res)
# Maximum likelihood estimation
#
# Call:
#   mle(minuslogl = blood.neg.ll, start = list(theta = 0.5))
# 
# Coefficients:
#   Estimate Std. Error
# theta 0.4246877 0.01089589
# 
# -2 log L: 2806.12

# compute a 95% CI for theta using a normal approximation
est <- coef(summary(res))[1]
se <- coef(summary(res))[2]
(res.ci<- c(est - 1.96 * se, est + 1.96 * se))
# [1] 0.4033318 0.4460436

# plot the log likelihood for theta in the interval (0.3,0.6) and compare it to the estimate contained 
# in the mle() output
curve(blood.ll(theta = x), from = 0.3, to = 0.55, n = 301, xlab = expression(theta),
      ylab = expression(paste("1(" * theta * ")")), main = 'log - likelihood')
abline(v= est, col = 'red')

#----------------------------------------------------------------
# analysis of bird hop data
hops <- scan(file = 'birdhops.txt')
head(hops)
str(hops)
summary(hops)

# produce a barplot
barplot(table(hops))

# use table(hops) to find u
table(hops)
# hops
# 1  2  3  4  5  6 
# 48 31 20  9  6 16
(u <- sum(table(hops)[1:5]))
# [1] 114

# find the sum of hops
(sum.y <- sum(hops))
# [1] 332

# use length of hops to find n
(n <- length(hops))
# [1] 130

# write a function to calculate the negative log likelihood
hops.neg.ll <- function(pi) {
  ll <- u * log(pi) + sum.y * log(1 - pi) - n * log(1 - pi)
  return(-ll)
}

# fnd the mle of pi and its SE
library(stats4)
summary(mle(hops.neg.ll, start = list(pi = 0.5)))
# Maximum likelihood estimation

# Call:
#   mle(minuslogl = hops.neg.ll, start = list(pi = 0.5))
# 
# Coefficients:
#   Estimate Std. Error
# pi 0.3607599 0.02701439

