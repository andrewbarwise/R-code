# exercise 2: island size and bird extinctions
# aim of this exercise is to get acquainted with logistic regression for binomial counts

library(Sleuth3)
krunnit <- case2101
head(krunnit)
#          Island  Area AtRisk Extinct
# 1     Ulkokrunni 185.8     75       5
# 2      Maakrunni 105.8     67       3
# 3      Ristikari  30.7     66      10
# 4 Isonkivenletto   8.5     51       6
# 5 Hietakraasukka   4.8     28       3
# 6      Kraasukka   4.5     20       4

summary(krunnit)
#           Island        Area             AtRisk         Extinct     
# Hietakraasukka: 1   Min.   :  0.070   Min.   : 6.00   Min.   : 2.00  
# Isonkivenletto: 1   1st Qu.:  0.625   1st Qu.:22.00   1st Qu.: 3.25  
# Kraasukka     : 1   Median :  2.150   Median :31.00   Median : 5.50  
# Lansiletto    : 1   Mean   : 19.804   Mean   :35.11   Mean   : 6.00  
# Luusiletto    : 1   3rd Qu.:  4.725   3rd Qu.:42.25   3rd Qu.: 8.00  
# Maakrunni     : 1   Max.   :185.800   Max.   :75.00   Max.   :13.00  
# (Other)       :12 

# create a histogram
par(mfrow = c(2,1))
with(krunnit, hist(Area, col = 'blue', breaks = seq(0,190, by = 5)))
with(krunnit,hist(log(Area), col = 'blue'))

# create 2 variables to add to the data frame
krunnit <- within(krunnit,{
  logArea <- log(Area)
  EmpProp <- Extinct / AtRisk
  })
head(krunnit)

# plot empirical proportions versus the log(Area)
with(krunnit, plot(logArea, EmpProp, xlab = 'log of area', ylab = 'empirical proportions', col = 2))
 
# logistic regression for krunnit dataframe
krunnit.glm1 <- glm(cbind(Extinct, AtRisk - Extinct) ~ logArea, family = binomial(link=logit), data = krunnit)
# summary(krunnit.glm1)
# Call:
# glm(formula = cbind(Extinct, AtRisk - Extinct) ~ logArea, family = binomial(link = logit), 
#     data = krunnit)

# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -1.71726  -0.67722   0.09726   0.48365   1.49545  

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -1.19620    0.11845 -10.099  < 2e-16 ***
#   logArea     -0.29710    0.05485  -5.416 6.08e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 45.338  on 17  degrees of freedom
# Residual deviance: 12.062  on 16  degrees of freedom
# AIC: 75.394

# Number of Fisher Scoring iterations: 4

# interpretation of the logarithm of area in the krunnit example
# if we double the km^2 land size there is a change in the odds by a factor of 2^-0.2971
2^-0.2971
# [1] 0.8138868
# therefore the odds of extinction on a reserve land size of 2A km^2 is estimated to be 81.39% of the
# odds of extinction on a reserve land size of A km^2

# calculate odds of extinction for 50, 100, 200 km^2
(odds_50 <- exp(-1.1962 - 0.2971 * log(50)))
# [1] 0.09456555
odds_50* 2^(-0.2971)
# [1] 0.07696565
odds_50 * 4^(-0.2971)
# [1] 0.06264132
# we can see as reserve size increases the odds of extinction fall

# ------------------------------------------------------------------------
# exercise 3: moth colouration and natural selection
library(Sleuth3)
moths<- case2102

# examine the data
str(moths)
# 'data.frame':	14 obs. of  4 variables:
# $ Morph   : Factor w/ 2 levels "dark","light": 2 1 2 1 2 1 2 1 2 1 ...
# $ Distance: num  0 0 7.2 7.2 24.1 24.1 30.2 30.2 36.4 36.4 ...
# $ Placed  : int  56 56 80 80 52 52 60 60 60 60 ...
# $ Removed : int  17 14 28 20 18 22 9 16 16 23 ...
summary(moths)
#    Morph      Distance         Placed         Removed     
# dark :7   Min.   : 0.00   Min.   :52.00   Min.   : 9.00  
# light:7   1st Qu.:11.43   1st Qu.:57.00   1st Qu.:16.25  
# Median :30.20   Median :60.00   Median :20.00  
# Mean   :27.23   Mean   :69.14   Mean   :21.86  
# 3rd Qu.:40.23   3rd Qu.:83.00   3rd Qu.:23.75  
# Max.   :51.20   Max.   :92.00   Max.   :40.00 
with(moths, hist(Distance, col = 'blue', breaks = 10))

# calculate the empirical probabilities and plot against 1) distance and 2) morph to get a rough idea 
# about any general relationships
moths$empprop <- with(moths, Removed/Placed)
with(moths, plot(factor(Morph), empprop, xlab = "distance", ylab = 'empirical proportions', col = 2))

with(moths, plot(Distance, empprop, xlab = 'distance', ylab = 'empirical proportions', col = 2))

with(moths, plot(Distance, log(empprop/(1-empprop)), xlab = 'distance',ylab = 'logit(empirical proportions',
                 col = 2))

# graphically display the data
library(lattice)
xyplot(empprop ~ Distance, data = moths, groups = Morph, type = 'l', auto.key = list(columns = 2),
       xlim = c(0,52), ylim = c(0,0.6), ylab = 'proportions of moths taken',
       xlab = 'distance from liverpool (km)')

# run a logistic regression on the dataframe
# the appropiate model to answer the research question is:
# logit(pi) = B0 + B1Morph + B2Distance + B3Morph:Distance
# B0 is the estimate of the log odds of a moth being taken 0 distance from liverpool for a dark coloured morph.
moths.glm3 <- glm(cbind(Removed, Placed - Removed) ~ Morph * Distance, data = moths, family = binomial(link = logit))
summary(moths.glm3)
# Call:
# glm(formula = cbind(Removed, Placed - Removed) ~ Morph * Distance, 
#     family = binomial(link = logit), data = moths)

# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -2.21183  -0.39883   0.01155   0.68292   1.31242  

# Coefficients:
#                    Estimate   Std. Error z value   Pr(>|z|)    
# (Intercept)         -1.128987   0.197906  -5.705   1.17e-08 ***
# Morphlight           0.411257   0.274490   1.498   0.134066    
# Distance             0.018502   0.005645   3.277   0.001048 ** 
# Morphlight:Distance -0.027789   0.008085  -3.437   0.000588 ***
#  ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 35.385  on 13  degrees of freedom
# Residual deviance: 13.230  on 10  degrees of freedom
# AIC: 83.904

# Number of Fisher Scoring iterations: 4


# calculate the pearson residuals
# extract the residuals and fitted values and store them in the dataframe
moths$Pres <- residuals(moths.glm3,'pearson')
moths$Fitted <- fitted(moths.glm3)
with(moths, cbind(empprop, Fitted, Pres))
#         empprop    Fitted        Pres
# [1,] 0.3035714 0.3278932 -0.38770684
# [2,] 0.2500000 0.2443482  0.09842764
# [3,] 0.3500000 0.3133306  0.70708882
# [4,] 0.2500000 0.2697738 -0.39848098
# [5,] 0.3461538 0.2805892  1.05231913
# [6,] 0.4230769 0.3355738  1.33631238
# [7,] 0.1500000 0.2692974 -2.08314815
# [8,] 0.2666667 0.3611865 -1.52421287
# [9,] 0.2666667 0.2581190  0.15130310
# [10,] 0.3833333 0.3880529 -0.07501947
# [11,] 0.2380952 0.2491537 -0.23432889
# [12,] 0.4761905 0.4106830  1.22040170
# [13,] 0.2608696 0.2326848  0.63978903
# [14,] 0.4239130 0.4547068 -0.59316567

# plot the pearson residuals vs the fitted values
plot(Pres ~ Fitted, moths, xlab = 'fitted values', ylab = 'pearson residuals', col = 'blue')
abline(h = 0)

# perform deviance goodness of fit test. h0: specified model is adequate. h1: more explanatory variables are needed
# Residual deviance: 13.230  on 10  degrees of freedom
# need to perform a one sided test
1 - pchisq(13.230, 10)
# [1] 0.2110951
# Fail to reject th null hypothesis so we conclude that there is insufficeint evidence to claim the 
# model is inadequate

#---------------------------------------------------------------
# exercise 4: conditional logistic regression for case controlled data: driving and backpain
# data taken from a study investigating whether driving a car is a raisk fator for lower back pain reulting
# in AHLID. case control study was conducted with cases selected from people who recently had xrays and
# and been diagnosed with AHLID. Controls taken from patients from the same hospital but for a unrelated
# medical condition
library(HSAUR3)
