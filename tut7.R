# Lab 7
# exercise 1: age and mating success of male elephants

library(Sleuth3)
elephants <- case2201

head(elephants,5)
#   Age Matings
# 1  27       0
# 2  28       1
# 3  28       1
# 4  28       1
# 5  28       3

summary(elephants)
#       Age           Matings     
# Min.   :27.00   Min.   :0.000  
# 1st Qu.:29.00   1st Qu.:1.000  
# Median :34.00   Median :2.000  
# Mean   :35.85   Mean   :2.683  
# 3rd Qu.:42.00   3rd Qu.:3.000  
# Max.   :52.00   Max.   :9.000

# histogram 
with(elephants, barplot(table(factor(Matings, levels = 0:9))))
# histogram is right skewed

# confidence interval in R: african elephants
# observed estimate
(lambda.hat.obs <- with(elephants, mean(Matings)))
# [1] 2.682927

# the standard error
n <- length(elephants$Matings)  # number of ob's
(lambda.hat.se <- sqrt(lambda.hat.obs/n))
# [1] 0.255807

# 95% CI
(lambda.ci <- c(lambda.hat.obs - 1.96 * lambda.hat.se, lambda.hat.obs + 1.96 * lambda.hat.se))
#  [1] 2.181545 3.184309
# if we repeatedly sample 41 elephants from the population we would expect 95% of the CI's to contain the 
# true parameter lambda

# Hypothesis test in R
# want to test if the true population pararmeter is 2. h0: lambda = 2, h1: lambda > 2. one sided test
pnorm(lambda.hat.obs, mean = 2, sd= sqrt(2/n), lower.tail = FALSE)
# [1] 0.0009937872
# reject h0 in favour of the alternative hypothesis. conclude lambda is > than 2

# plot the observed probability density and estimated poisson prob. density based on lambda.hat.obs
hist(elephants$Matings, breaks = (0:10) - 0.5, ylim = c(0,0.3), freq = FALSE, xlab = 'Matings')
points(seq(0,10,1), dpois(x= seq(0,10,1), lambda = lambda.hat.obs), col = 'blue', pch = 19)

# estimation of the regression coefficeints in R
elephants.glm1 <- glm(Matings ~ Age, data = elephants, family = poisson(link = log))
summary(elephants.glm1)
# Call:
# glm(formula = Matings ~ Age, family = poisson(link = log), data = elephants)

# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.80798  -0.86137  -0.08629   0.60087   2.17777  

# Coefficients:
#               Estimate  Std. Error  z value  Pr(>|z|)    
#   (Intercept) -1.58201    0.54462  -2.905  0.00368 ** 
#   Age          0.06869    0.01375   4.997  5.81e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

# Null deviance: 75.372  on 40  degrees of freedom
# Residual deviance: 51.012  on 39  degrees of freedom
# AIC: 156.46

# Number of Fisher Scoring iterations: 5

# calculate the mean and variance of the number of successful breedings over 8 years for a 25 year old elephant
exp(-1.58201 + 0.06869 * 25)
# [1] 1.144812   # important to remember the E[Y] = lambda = Var[Y]

# --------------------------------------------------------------------
# exercise2: modelling the number of hurricanes each year
library(Sleuth3)
hurricanes <- ex1028
head(hurricanes)
#   Year  ElNino Temperature WestAfrica Storms Hurricanes StormIndex
# 1 1950    cold          -1          1     13         11        243
# 2 1951    warm           1          0     10          8        121
# 3 1952 neutral           0          1      7          6         97
# 4 1953    warm           1          1     14          6        121
# 5 1954    cold          -1          1     11          8        127
# 6 1955    cold          -1          1     12          9        198

# concentrate on a subset of these variables
hurricanes <- within(hurricanes, {
  ElNino <- Storms <- StormIndex <- NULL
})
head(hurricanes)
#   Year Temperature WestAfrica Hurricanes
# 1 1950          -1          1         11
# 2 1951           1          0          8
# 3 1952           0          1          6
# 4 1953           1          1          6
# 5 1954          -1          1          8
# 6 1955          -1          1          9

# obtain the observed estimates of the regression coefficeints
fm1 <- glm(Hurricanes ~ ., data = hurricanes, family = poisson(link = log))
summary(fm1)
# Call:
# glm(formula = Hurricanes ~ ., family = poisson(link = log), data = hurricanes)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.1704  -0.6230  -0.0813   0.3521   2.0118  

# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)   
# (Intercept)  4.983183   9.615358   0.518  0.60428   
# Year        -0.001687   0.004861  -0.347  0.72856   
# Temperature -0.218985   0.080872  -2.708  0.00677 **
# WestAfrica   0.180475   0.144660   1.248  0.21219   
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

# Null deviance: 44.414  on 47  degrees of freedom
# Residual deviance: 29.304  on 44  degrees of freedom
# AIC: 207.13

# Number of Fisher Scoring iterations: 4

# the wald test suggests that both Year and WestAfrica are not significant
# perform a drop in deviance test for Year and WestAfrica
fm2 <- glm(Hurricanes ~Temperature, data = hurricanes, family = poisson(link = log))
anova(fm2,fm1, test = 'Chisq')
# Analysis of Deviance Table

# Model 1: Hurricanes ~ Temperature
# Model 2: Hurricanes ~ Year + Temperature + WestAfrica
# Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1        46     31.820                     
# 2        44     29.304  2   2.5156   0.2843
# not enough evidence to reject h0 (p- value = 0.2843). No evidence that Year and West Africa are associated with the number 
# of hurricanes after Temperature has been accounted for in the regression model.

# regress HUrricanes on only WestAfrica
fm3 <- glm(Hurricanes ~ WestAfrica, data = hurricanes, family = poisson(link = log))
summary(fm3)
# Call:
# glm(formula = Hurricanes ~ WestAfrica, family = poisson(link = log), 
#     data = hurricanes)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.7077  -0.5469  -0.1938   0.5060   1.7134  

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.60944    0.08165  19.712  < 2e-16 ***
#   WestAfrica   0.33647    0.12084   2.784  0.00536 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

# Null deviance: 44.414  on 47  degrees of freedom
# Residual deviance: 36.771  on 46  degrees of freedom
# AIC: 210.6

# Number of Fisher Scoring iterations: 4
# results from this regression show that Hurricanes is associated with West Africa in the absence 
# of info from Temperature

# it seems that both Temperature and WestAfrica are both significant on their own but jointly
# West Africa becomes not significant. Tabulate Temperature and west Africa to help explore this.
xtabs(~Temperature + WestAfrica, data = hurricanes)
#           WestAfrica
# Temperature  0  1
#        -1    6 10
#         0    11  5
#         1    13  3
# summarise that Temperature and WestAfrica are propably not independent. When WestAfrica equals 0 more
# likely to see positive values for Temperature and vice versa.

# plot the residuals and pearson residuals against the fitted values
plot(fitted(fm2), residuals(fm2))

#-------------------------------------------------------------------
# exercise3: writing own functions in R
my.sq <- function(x) {
  x2 <- x ^ 2
  return(x2)
}

# use this function to calculate x^2 for a sequence of values of x between -1 and 1. plot the results
x.grid <- seq(-1,1,by = 0.001)
x.grid.sq <- my.sq(x.grid)
plot(x.grid,x.grid.sq)

# write a function y = a + bx + cx^2 and plot for a = 1, b = 2, c = 3. use the x values given by x.grid
my.poly2 <- function(a,b,c,x) {
  xpol2 <- a + b * x + c * x^2
  return(xpol2)
}
plot(x.grid, my.poly2(a=1,b=2,c=3,x=x.grid), type = 'l')

# -----------------------------------------
# exercise4: examining the likelihood of simulated Poisson data set
# going to examine the likelihood for a single Poisson observation.
# Y ~ Pois(lambda)
# functiion to observe the likelihood
like.pois <- function(lambda, y) {
  like <- exp(-lambda) * lambda^y/factorial(y)
  return(like)
}

# we are interested in what values of lambda give plausible values of the observed data set. for one data
# point 'y' the observed estimate of lambda is simply lambda.hat.obs = 'y'.
# create a grid of values of lambda over a sensible range
lambda.grid <- seq(0.001, 10, by = 0.01)

# plot the likelihood for 4 different observations of 'Y' and indicate on the plot the value of lambda
# for which Pr[Y=y] is a maximum.
par(mfrow = c(2,2))
plot(lambda.grid, like.pois(lambda.grid, 3), xlab = 'lambda', ylab = 'like(lambda)', type = 'l', main = 'y=3')
abline(v=3)
plot(lambda.grid, like.pois(lambda.grid, 4), xlab = 'lambda', ylab = 'like(lambda)', type = 'l', main = 'y=4')
abline(v=4)
plot(lambda.grid, like.pois(lambda.grid, 5), xlab = 'lambda', ylab = 'like(lambda)', type = 'l', main = 'y=5')
abline(v=5)
plot(lambda.grid, like.pois(lambda.grid, 6), xlab = 'lambda', ylab = 'like(lambda)', type = 'l', main = 'y=6')
abline(v=6)