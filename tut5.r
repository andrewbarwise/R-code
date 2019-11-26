# lab 5
# exercise 1 donner party
library(Sleuth3)
donner <- case2001

donner <- within(donner, {
  Surv <- as.numeric(Status) - 1
  SexN <- 2 - as.numeric(Sex)
})

donner$Sex <- relevel(donner$Sex, ref = 'Male') # therefore male has reference value 0 and female = 1

head(donner)
summary(donner)

# plot survival against age
with(donner, plot(jitter(Age), Surv, ylab = 'Survival', col = SexN + 1, pch = SexN))
legend('topright', col = c(1,2), pch = c(0,1), legend = c("Male", "Female"))

# estimate the model in R using binary logistic regression
donner.glm1 <- glm(Status ~ Age + Sex, data = donner, family = binomial(link = logit))
summary(donner.glm1)
# Call:
#   glm(formula = Status ~ Age + Sex, family = binomial(link = logit), 
#       data = donner)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.7445  -1.0441  -0.3029   0.8877   2.0472  

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept)  1.63312    1.11018   1.471   0.1413  
# Age         -0.07820    0.03728  -2.097   0.0359 *
# SexFemale    1.59729    0.75547   2.114   0.0345 *
#   ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 61.827  on 44  degrees of freedom
# Residual deviance: 51.256  on 42  degrees of freedom
# AIC: 57.256

# Number of Fisher Scoring iterations: 4

# plot a graph showing survival of each sex as a function of age
with(donner, plot(Surv ~ jitter(Age), xlab = "Age", ylab = expression(pi), col= SexN +
                    1, pch = SexN))
age.grid <- with(donner, seq(min(Age), max(Age), by = 0.1))
pi.fem <- exp(1.6631 - 0.0782 * age.grid + 1.5973) / ( 1 + exp(1.6331 - 0.0782 * age.grid + 1.5973) )
pi.male <- exp(1.6631 - 0.0782 * age.grid) / (1 + exp(1.6331 - 0.0782 * age.grid))
lines(age.grid, pi.fem, type = 'l', col = 'red')
lines(age.grid, pi.male, type = 'l', col = 'black')
legend('topright', col = c(1,2), pch = c(0,1), legend = c("Male","Female"))

# compare the log odds of survival if the Sex indicator is Female. i.e female = 0
summary(donner.glm1)
exp(cbind(OR = coef(donner.glm1), confint(donner.glm1)))
#                 OR     2.5 %     97.5 %
# (Intercept) 5.1198252 0.6644486 55.9655564
# Age         0.9247757 0.8500691  0.9860327
# SexFemale   4.9396452 1.2154349 25.2460693

donner$Sex <- relevel(donner$Sex, ref = 'Female')
donner.glm2 <- glm(Status ~ Age + Sex, data = donner, family = binomial(link = logit))
summary(donner.glm2)
exp(cbind(OR = coef(donner.glm2), confint(donner.glm2)))
#                OR      2.5 %      97.5 %
# (Intercept) 25.2901201 2.34296918 618.1277279
# Age          0.9247757 0.85006909   0.9860327
# SexMale      0.2024437 0.03961013   0.8227507

# change the status level to survived
donner$Died <- relevel(donner$Status, ref = 'Survived')
donner.glm3 <- glm(Died ~ Age + Sex, data = donner, family = binomial(link = logit))
summary(donner.glm3)

# Call:
#   glm(formula = Died ~ Age + Sex, family = binomial(link = logit), 
#       data = donner)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.0472  -0.8877   0.3029   1.0441   1.7445  

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)  
# (Intercept) -3.23041    1.38686  -2.329   0.0198 *
#   Age          0.07820    0.03728   2.097   0.0359 *
#   SexMale      1.59729    0.75547   2.114   0.0345 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 61.827  on 44  degrees of freedom
# Residual deviance: 51.256  on 42  degrees of freedom
# AIC: 57.256

# Number of Fisher Scoring iterations: 4
exp(cbind(OR = coef(donner.glm3), confint(donner.glm3)))
#                  OR       2.5 %     97.5 %
# (Intercept) 0.03954113 0.001617789  0.4268089
# Age         1.08134331 1.014165144  1.1763750
# SexMale     4.93964518 1.215434931 25.2460693

#-------------------------------------------------------------------------
# EXERCISE 2
# this study aims to explore the link between birdkeeping and lung cancer risk
# seeking to see evidence of an association between increased risk of lung cancer and birdkeeping,
# after accounting for other important factors such as smoking

library(Sleuth3)
birdkeeping <- case2002
summary(birdkeeping)

# 4 binary variables: LC = lung cancer or not; FM = sex as subject(female is reference value); SS = socioeconomic
# status; BK = indicator of birdkeeper or not

# 3 continous variables: AG = age of subject; YR = years of smoking; CD = average rate of smoking

# tabulate each binary explanatory variable with LC to get a rough idea of any relationship
with(birdkeeping, table(FM,LC))
#        LC
# FM       LungCancer NoCancer
# Female         12       24
# Male           37       74
with(birdkeeping, table(BK,LC))
with(birdkeeping, table(SS,LC))

# estimate the correlation between years of smoking and cigarettes per day
with(birdkeeping, cor(YR,CD))
# [1] 0.5734081

# there is another measure of smoking impact called Smoking PAck Years(spy). spy is dfeined ny the number of 
# packs per day by the number of years. create another variable for the birdkeeping dataframe
birdkeeping$spy <- with(birdkeeping,(CD/20)*YR)

# plot the data using the lattice package
library(lattice)
xyplot(spy~AG|BK, data = birdkeeping, groups = LC, auto.key = list(columns = 2))
# 2 plots have been created side by side. first plot shows spy against AG for all the those observations 
# in the dataframe that corresponds to individuals that owned birds. plot in the second panel corresponds
# to individuals that didnt own birds.
# "| BK" instructs R to create a separate plot for each level in BK. the optional argument "groups=LC"
# specifies that the points should be coloured accordng to the levels of the factor of LC.
# 'auto.key' produces a simple legend

# run a logistic regression to answer the question "after accounting for other factors does owning birds 
# increase the prob. of lung cancer."
cancerbirds.glm <- glm(LC~AG + FM + SS + spy + BK, family = binomial(link=logit),
                       data = birdkeeping)
summary(cancerbirds.glm)
# Call:
# glm(formula = LC ~ AG + FM + SS + spy + BK, family = binomial(link = logit), 
#    data = birdkeeping)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.1783  -1.0896   0.5388   0.8410   1.6684  

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.41627    1.59950   0.260  0.79467    
# AG           0.00939    0.02876   0.326  0.74407    
# FMMale       0.14659    0.48793   0.300  0.76384    
# SSLow       -0.13640    0.45417  -0.300  0.76393    
# spy         -0.03493    0.01177  -2.967  0.00301 ** 
# BKNoBird     1.41477    0.40317   3.509  0.00045 ***
#   ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 187.14  on 146  degrees of freedom
# Residual deviance: 162.00  on 141  degrees of freedom
# AIC: 174

#Number of Fisher Scoring iterations: 4
exp(cbind(OR = coef(cancerbirds.glm),confint(cancerbirds.glm)))
#                    OR      2.5 %     97.5 %
# (Intercept) 1.5162940 0.06887182 38.1846316
# AG          1.0094347 0.95316516  1.0678622
# FMMale      1.1578808 0.43761033  3.0017284
# SSLow       0.8724933 0.35098611  2.1137864
# spy         0.9656718 0.94228017  0.9872518
# BKNoBird    4.1155399 1.90127549  9.3073692