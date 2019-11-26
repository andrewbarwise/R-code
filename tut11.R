# exercise 3
# access the donner party data and turn the two variabes stored as factors into indicator variables
library(Sleuth3)
donner <- case2001
donner <- within(donner, {
  Surv <- as.numeric(Status) - 1 # died = 0, survived = 1
  SexN <- as.numeric(Sex) - 1 # female = 0, survived = 1
})

donner.neg.ll <- function(beta0, beta1, beta2){
  tmp <- beta0 + beta1 * donner$Age + beta2 * donner$SexN
  p <- 1/(1 + exp(-tmp))
  ll <- sum(dbinom(donner$Surv, 1, p, log = TRUE))
  return(-ll)
}

# call the mle()
library(stats4)
summary(mle(donner.neg.ll, start = list(beta0 = 0, beta1 = 0, beta2 = 0)))

#Maximum likelihood estimation
#
#Call:
#  mle(minuslogl = donner.neg.ll, start = list(beta0 = 0, beta1 = 0, 
#                                              beta2 = 0))
#
#Coefficients:
#  Estimate Std. Error
#beta0  3.23025701 1.38675433
#beta1 -0.07818731 0.03728222
#beta2 -1.59786485 0.75548322
#
#-2 log L: 51.25629 

# compare these resulte to the results obtained in week 5.
# in particular look at the residual deviance  compared to 2 times the negative
# log likelihood output
summary(glm(Surv ~ Age + Sex, donner, family = binomial(link = logit)))
# Call:
# glm(formula = Surv ~ Age + Sex, family = binomial(link = logit), 
#    data = donner)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.7445  -1.0441  -0.3029   0.8877   2.0472  
#
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)  
# (Intercept)  3.23041    1.38686   2.329   0.0198 *
# Age         -0.07820    0.03728  -2.097   0.0359 *
# SexMale     -1.59729    0.75547  -2.114   0.0345 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 61.827  on 44  degrees of freedom
# Residual deviance: 51.256  on 42  degrees of freedom
# AIC: 57.256
#
# Number of Fisher Scoring iterations: 4

# the results for the regression coefficeints and their SE are almost identical
# residual deviance is equal to twice the negative log likelihood