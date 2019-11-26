# lab 8
# exercise 1: predicting the number of days absent
# a)
library(MASS)

# R commands to plot the negative binomial model for mu = 3 and different values for overdispersion
ygrid <- seq(0,10, length = 11)
plot(ygrid, dnbinom(ygrid, size = exp(0.5), mu = 3), col = 1, xlab = 'y', ylim = c(0,0.4), ylab = 'Negative binomial prob.')
lines(ygrid, dnbinom(ygrid, size = exp(0.5), mu = 3), col = 1)
points(ygrid, dnbinom(ygrid, size = exp(1), mu = 3), col = 2)
lines(ygrid, dnbinom(ygrid, size = exp(1), mu = 3), col = 2)
points(ygrid, dnbinom(ygrid, size = exp(2), mu = 3), col = 3)
lines(ygrid, dnbinom(ygrid, size = exp(2), mu = 3), col = 3)
points(ygrid, dnbinom(ygrid, size = exp(3), mu = 3), col = 4)
lines(ygrid, dnbinom(ygrid, size = exp(3), mu = 3), col = 4)

# predicting the number of days absent
dat <- read.csv('nb_data.csv', header = TRUE)

dat <- within(dat, {
  prog <- factor(prog, levels = 1:3, labels = c("general", "academic", "vocational"))
  id <- factor(id)
})
summary(dat)

library(lattice)
histogram(~daysabs | prog, data = dat) # gives 3 histograms. one for each program

with(dat, tapply(daysabs, prog, function(x) {
  sprintf("M (SD) %1.2f (%1.2f)", mean(x), sd(x))
}))
#               general              academic            vocational 
#      "M (SD) 10.65 (8.20)"  "M (SD) 6.93 (7.45)"  "M (SD) 2.67 (3.73)" 
# This suggests that program type appears to be associated with number of days absent as the mean value 
# of the outcome varies by program type. the variances are high relative to the mena suggesting there 
# is over dispersion

# due to over dispersion perform a negative binomial regression with the number of days absent as the 
# response variable and the maths score and instructional program type as explanatory variables
# must change from glm() to glm.nb() using library(MASS)
library(MASS)
summary(m1 <- glm.nb(daysabs ~ math + prog, data = dat))
# Call:
# glm.nb(formula = daysabs ~ math + prog, data = dat, init.theta = 1.032713156, 
#        link = log)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.1547  -1.0192  -0.3694   0.2285   2.5273  

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)     2.615265   0.197460  13.245  < 2e-16 ***
#  math           -0.005993   0.002505  -2.392   0.0167 *  
#  progacademic   -0.440760   0.182610  -2.414   0.0158 *  
#  progvocational -1.278651   0.200720  -6.370   1.89e-10 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for Negative Binomial(1.0327) family taken to be 1)

# Null deviance: 427.54  on 313  degrees of freedom
# Residual deviance: 358.52  on 310  degrees of freedom
# AIC: 1741.3

# Number of Fisher Scoring iterations: 1


# Theta:  1.033 
# Std. Err.:  0.106 

# 2 x log-likelihood:  -1731.258
# math variale is statistically sighnificant(wald test)
# every one unit increase in math the expected log count of the number of days absent decreases by 0.006

# indicator variable shown as progAcademic is the expected difference in log count between this group and the
# reference group( progGeneral). The expected log count for progAcademic is 0.44 lower than the expected
# log count for progGeneral

# to determine if prog itself is statistically significant we we need to compare a model with and without 
# prog. the reason it is important to fit separate models, is that unless we do the overdispersion pararmeter
# is held constant.
m2 <- update(m1, .~. - prog)
anova(m1,m2)
# Likelihood ratio tests of Negative Binomial Models

# Response: daysabs
# Model             theta Resid. df    2 x log-lik.   Test    df LR stat.     Pr(Chi)
# 1        math 0.8558565       312       -1776.306                                  
# 2 math + prog 1.0327132       310       -1731.258  1 vs 2     2 45.04798  1.65179e-10
# the 2 degree of freedom chi sq test indicates that 'prog' is a statistically significant
# predictor of daysabs.

# can compare our negative binomial regression model against a similar poisson regression model
m3 <- glm(daysabs ~ math + prog, family = 'poisson', data = dat)
(llm1 <- logLik(m1))
# 'log Lik.' -865.6289 (df=5)
(llm3 <- logLik(m3))
# 'log Lik.' -1328.642 (df=4)
pchisq(as.vector(2 * (llm1 - llm3)), df = 1, lower.tail = FALSE)
# [1] 2.157298e-203
# strong suggestion that the negative binomial model estimating the dispersion parameter is more appropiate 
# than the Poisson model

# predicting the number of days absent
# can get the CI's for the coeffiecints by profiling the likelihood function
exp(est <- cbind(Estimate = coef(m1), confint(m1)))
#                  Estimate     2.5 %     97.5 %
# (Intercept)    13.6708448 9.4126616 20.3470498
# math            0.9940249 0.9891583  0.9989340
# progacademic    0.6435471 0.4448288  0.9115184
# progvocational  0.2784127 0.1857247  0.4106239
# the output indicates that the incident rate for progAcademic is 0.64 times the incident rate for the 
# reference group(progGeneral)
# percentage change in the incident rate of 'daysabs' is 1% decrease for every unit increase in math

#--------------------------------------------------------------
# exercise2: absenteesim from school in rural NSW
library(MASS)
summary(quine)
#  Eth    Sex    Age     Lrn          Days      
# A:69   F:80   F0:27   AL:83   Min.   : 0.00  
# N:77   M:66   F1:46   SL:63   1st Qu.: 5.00  
#               F2:40           Median :11.00  
#               F3:33           Mean   :16.46  
#                               3rd Qu.:22.75  
#                               Max.   :81.00  

# check for evidence of over dispersion of the four categorical explanatory variables
with(quine, tapply(Days, list(Lrn,Age,Sex,Eth), function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
# , , F, A
#
# F0                       F1                       F2                       F3                      
# AL "M (SD) = 21.25 (17.71)" "M (SD) = 11.40 (6.54)"  "M (SD) = 2.00 (NA)"     "M (SD) = 14.56 (14.85)"
# SL "M (SD) = 3.00 (NA)"     "M (SD) = 22.60 (18.69)" "M (SD) = 36.38 (26.51)" NA                      
#
# , , M, A
#
# F0                      F1                      F2                       F3                      
# AL "M (SD) = 13.00 (8.03)" "M (SD) = 10.50 (4.95)" "M (SD) = 27.43 (14.70)" "M (SD) = 27.14 (10.37)"
# SL "M (SD) = 9.00 (6.24)"  "M (SD) = 9.00 (5.20)"  "M (SD) = 37.00 (23.40)" NA                      
# 
# , , F, N
# 
# F0                       F1                      F2                     F3                      
# AL "M (SD) = 18.50 (10.66)" "M (SD) = 11.00 (8.94)" "M (SD) = 1.00 (NA)"   "M (SD) = 13.50 (11.49)"
# SL "M (SD) = 25.00 (NA)"    "M (SD) = 6.00 (4.17)"  "M (SD) = 6.22 (4.97)" NA                      
# 
# , , M, N
# 
# F0                       F1                     F2                      F3                      
# AL "M (SD) = 5.33 (5.43)"   "M (SD) = 3.50 (0.71)" "M (SD) = 9.14 (9.48)"  "M (SD) = 27.29 (22.93)"
# SL "M (SD) = 30.00 (32.51)" "M (SD) = 6.14 (6.07)" "M (SD) = 29.33 (7.02)" NA 

# In all the strata the sample SD is about equal to the sample mean, so the sample variance would be much
# higher. In a poisson model var[y] and mean[y] should be roughly equal
# therefore must fit a negative binomial
summary(fm1 <- glm.nb(Days ~ Sex + Eth + Age + Lrn, data = quine))
# Call:
# glm.nb(formula = Days ~ Sex + Eth + Age + Lrn, data = quine, 
#        init.theta = 1.274892646, link = log)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -2.7918  -0.8892  -0.2778   0.3797   2.1949  

# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  2.89458    0.22842  12.672  < 2e-16 ***
# SexM         0.08232    0.15992   0.515 0.606710    
# EthN        -0.56937    0.15333  -3.713 0.000205 ***
# AgeF1       -0.44843    0.23975  -1.870 0.061425 .  
# AgeF2        0.08808    0.23619   0.373 0.709211    
# AgeF3        0.35690    0.24832   1.437 0.150651    
# LrnSL        0.29211    0.18647   1.566 0.117236    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Negative Binomial(1.2749) family taken to be 1)
#
# Null deviance: 195.29  on 145  degrees of freedom
# Residual deviance: 167.95  on 139  degrees of freedom
# AIC: 1109.2
# 
# Number of Fisher Scoring iterations: 1
#
#
# Theta:  1.275 
# Std. Err.:  0.161 
#
# 2 x log-likelihood:  -1093.151 

# age has 4 levels which makes the significance of age difficult to assess.
# use dropterm()
dropterm(fm1, test = 'Chisq')
# Single term deletions

# Model:
#   Days ~ Sex + Eth + Age + Lrn
#        Df    AIC     LRT   Pr(Chi)    
# <none>    1107.2                      
# Sex     1 1105.4  0.2497 0.6172762    
# Eth     1 1117.7 12.5235 0.0004019 ***
# Age     3 1112.7 11.5238 0.0092059 ** 
# Lrn     1 1107.7  2.5017 0.1137250    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# drop sex as it is the least significant term and re run regression
summary(fm2 <- update(fm1,. ~ . - Sex))
# Call:
# glm.nb(formula = Days ~ Eth + Age + Lrn, data = quine, init.theta = 1.271992937, 
#       link = log)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.8081  -0.8820  -0.2790   0.3977   2.2727  
#
# Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   2.9351     0.2017  14.551  < 2e-16 ***
# EthN         -0.5627     0.1535  -3.667 0.000246 ***
# AgeF1        -0.4703     0.2363  -1.990 0.046590 *  
# AgeF2         0.1009     0.2363   0.427 0.669391    
# AgeF3         0.3591     0.2452   1.465 0.142999    
# LrnSL         0.2848     0.1848   1.541 0.123203    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for Negative Binomial(1.272) family taken to be 1)
#
# Null deviance: 194.92  on 145  degrees of freedom
# Residual deviance: 167.89  on 140  degrees of freedom
# AIC: 1107.4
#
# Number of Fisher Scoring iterations: 1
#
#
# Theta:  1.272 
# Std. Err.:  0.161 
# 
# 2 x log-likelihood:  -1093.401 

# repeat the process with dropterm()
dropterm(fm2, test = 'Chisq')
# Single term deletions
#
# Model:
#  Days ~ Eth + Age + Lrn
#         Df    AIC     LRT   Pr(Chi)    
# <none>     1105.4                      
# Eth      1 1115.7 12.3118 0.0004501 ***
# Age      3 1112.6 13.1754 0.0042722 ** 
# Lrn      1 1105.8  2.4005 0.1212988    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# will drop Lrn as it is not statistically significant
(summary(fm3 <- update(fm2, . ~ . -Lrn)))
# 
# Call:
#   glm.nb(formula = Days ~ Eth + Age, data = quine, init.theta = 1.249142794, 
#          link = log)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.7902  -0.9310  -0.3795   0.3845   2.6038  
#
# Coefficients:
#              Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   3.0382     0.1957  15.529  < 2e-16 ***
# EthN         -0.5611     0.1547  -3.628 0.000286 ***
# AgeF1        -0.3855     0.2274  -1.695 0.090019 .  
# AgeF2         0.1846     0.2313   0.798 0.424744    
# AgeF3         0.2550     0.2407   1.060 0.289332    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for Negative Binomial(1.2491) family taken to be 1)
#
# Null deviance: 192.04  on 145  degrees of freedom
# Residual deviance: 167.84  on 141  degrees of freedom
# AIC: 1107.8
#
# Number of Fisher Scoring iterations: 1
# 
# Theta:  1.249 
# Std. Err.:  0.157 
#
# 2 x log-likelihood:  -1095.801

dropterm(fm3, test = 'Chisq')
# Single term deletions
#
# Model:
#  Days ~ Eth + Age
#         Df    AIC    LRT   Pr(Chi)    
# <none>    1105.8                     
# Eth     1 1115.8 12.014 0.0005279 ***
# Age     3 1110.6 10.833 0.0126664 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# even though the individual wald tests for age show age as not significant, Age is significantly associated
# with Days. therefore keep in the model

# for our final model
exp(cbind(coef(fm3), confint(fm3)))
#                             2.5 %     97.5 %
# (Intercept) 20.8681314 14.0862454 31.9117365
# EthN         0.5705882  0.4175413  0.7785362
# AgeF1        0.6800884  0.4301261  1.0599214
# AgeF2        1.2027470  0.7516406  1.9027972
# AgeF3        1.2904623  0.8000806  2.0681061