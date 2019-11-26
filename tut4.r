## exercise 1 promotion data

Promotions <- matrix(c(21,14,3,10), nrow = 2)
rownames(Promotions) <- c("Male", "Female")
colnames(Promotions) <- c("Promoted", "Not Promoted")
Promotions

#          Promoted    Not Promoted
# Male         21            3
# Female       14            10

fisher.test(Promotions, alternative = "greater")

#   Fisher's Exact Test for Count Data

# data:  Promotions
# p-value = 0.0245
# alternative hypothesis: true odds ratio is greater than 1
# 95 percent confidence interval:
#  1.230224      Inf
# sample estimates:
# odds ratio 
#    4.83119 

# make the fisher test two sided
fisher.test(Promotions, alternative = "two.sided")

#        Fisher's Exact Test for Count Data

# data:  Promotions
# p-value = 0.04899
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   1.00557 32.20580
# sample estimates:
# odds ratio 
#   4.83119 
#-----------------------------------------------------------
# exercise 2 common cold data

ComCold <- matrix(c(335,302,76,105), nrow = 2, dimnames = list(c("placebo", "vitamin C"), c("Cold", "No cold")))
ComCold

#         Cold    No cold
# placebo    335      76
# vitamin C  302     105

(prop.table(ComCold))   # conditional probabilities 
#            Cold    No cold
# placebo   0.4095355 0.09290954
# vitamin C 0.3691932 0.12836186

# perform a fisher test to evaluate the independence between drugs and disease
fisher.test(ComCold, alternative = 'two.sided')

#         Fisher's Exact Test for Count Data

# data:  ComCold
# p-value = 0.01444
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#  1.083492 2.172490
# sample estimates:
# odds ratio 
#   1.531722 
#   at a 5% significance level there is enough evidence to reject the null hypothesis

# calculate the observed odds of catching a cold in the vitamin c group compared to the placebo
(odds.ratio <- (302 * 76) / (335 * 105))
# [1] 0.65
# now calculate the log
(log.odds <- log(odds.ratio))
# [1] -0.42
# calculate the SE of log
(log.phi.hat.se.ci <- sqrt(sum(1/ComCold)))
# [1] 0.17
ll <- exp(log.odds - 1.96 * log.phi.hat.se.ci)
ul <- exp(log.odds + 1.96 * log.phi.hat.se.ci)
(c(ll,ul))
# [1] 0.4673952 0.9109375

# -------------------------------------------------------------------
# exercise 3 titanic data
Titanic1 <- data.frame(Count = c(212,203,118,178,673,122,167,528), Status = gl(2,4, labels = c('Alive','Dead')),
                       Class = gl(4,1,length =8, labels = c("Crew", "First", "Second","Third")))

(tab <- xtabs(Count ~ Status + Class, Titanic1))
#                    Class
# Status        Crew First Second Third
#  Alive         212   203    118   178
#  Dead          673   122    167   528

(colsum <- colSums(tab))
# Crew  First Second  Third 
# 885    325    285    706 
(survival.rates <- sweep(tab, 2, colsum, "/"))
#        Class
# Status       Crew     First    Second     Third
#    Alive 0.2395480 0.6246154 0.4140351 0.2521246
#    Dead  0.7604520 0.3753846 0.5859649 0.7478754

# reformat the 4x2x2x2 Titanic dataset into a 2x2 table (stratified by age and survived) and a 2x2x4 table
# (stratified by age, survived and class)
Titanic.AS <- margin.table(Titanic, margin = 3:4)
Titanic.ASC <- margin.table(Titanic, margin = c(3:4, 1))
Titanic.AS
#         Survived
# Age       No  Yes
#   Child   52   57
#   Adult 1438  654

# calculate the row conditional probabilities 
prop.table(Titanic.AS, margin = 1)
#               Survived
# Age            No       Yes
#     Child 0.4770642 0.5229358
#     Adult 0.6873805 0.3126195


fisher.test(Titanic.AS)
#           Fisher's Exact Test for Count Data
# data:  Titanic.AS
# p-value = 1.234e-05
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   0.2760923 0.6228448
# sample estimates:
#   odds ratio 
#   0.4150843 

# can look at the relationship between survival and age separately across all 4 levels of class
fisher.test(Titanic.ASC[, ,'1st'])
#     Fisher's Exact Test for Count Data

# data:  Titanic.ASC[, , "1st"]
# p-value = 0.08725
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#  0.000000 1.399465
# sample estimates:
#odds ratio 
#         0 

fisher.test(Titanic.ASC[,,'2nd'])
#         Fisher's Exact Test for Count Data

# data:  Titanic.ASC[, , "2nd"]
# p-value = 1.415e-10
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   0.00000000 0.09705438
# sample estimates:
# odds ratio 
#         0 

fisher.test(Titanic.ASC[,,"3rd"])
#       Fisher's Exact Test for Count Data

# data:  Titanic.ASC[, , "3rd"]
# p-value = 0.05523
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
# 0.3623786 1.0501753
# sample estimates:
# odds ratio 
#   0.611422 

fisher.test(Titanic.ASC[,,'Crew'])
#          Fisher's Exact Test for Count Data

# data:  Titanic.ASC[, , "Crew"]
# p-value = 1
# alternative hypothesis: true odds ratio is not equal to 1
# 95 percent confidence interval:
#   0 Inf
#sample estimates:
#odds ratio 
#         0 

# use a cochrane- mantell- haensel test to explore the effect of age on survival across all leves of class
# h0 is that there is a common odds ratio of 1 for the survival of children compared to adults across
# all levels of class
mantelhaen.test(Titanic.ASC)
#      Mantel-Haenszel chi-squared test with continuity correction

# data:  Titanic.ASC
# Mantel-Haenszel X-squared = 26.236, df = 1, p-value = 3.022e-07
# alternative hypothesis: true common odds ratio is not equal to 1
# 95 percent confidence interval:
#   0.2100398 0.4942222
# sample estimates:
#   common odds ratio 
# 0.3221899 

# -----------------------------------------------
# exercise 4 rabbit data

Rabbits <- array(c(0,0,6,5,3,0,3,6,6,2,0,4,5,6,1,0,2,5,0,0), dim = c(2,2,5),
                 dimnames = list(Delay=c("None","1.5hr"), Response=c("Cured","Died"),
                                 Penicillin.Level = c('1/8', '1/4','1/2','1','4')))
Rabbits
mantelhaen.test(Rabbits, alternative = "two.sided")
#               Mantel-Haenszel chi-squared test with continuity correction

# data:  Rabbits
# Mantel-Haenszel X-squared = 3.9286, df = 1, p-value = 0.04747
# alternative hypothesis: true common odds ratio is not equal to 1
#95 percent confidence interval:
#  1.026713 47.725133
# sample estimates:
#  common odds ratio 
#    7 
# we cant use fishers exact test as the data is presented as 5x2x2 tables. 