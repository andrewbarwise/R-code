# compare efficeincy of truck engines using two different oil types.
# oil type Y: n = 112, 84 > 20000 hours
# oil type Z: n = 108, 66 > 20000 hours

# 95% CI for prob that truck using oil type Y lasts longer than 20000 hours
n.y = 112
success.y
pi.y.obs <- success.y / n
se.pi.y.obs <- sqrt((pi.y.obs * (1 - pi.y.obs)) / n.y)
ll.y <- pi.y.obs - 1.96 * se.pi.y.obs
ul.y <- pi.y.obs + 1.96 * se.pi.y.obs
pi.y.obs    # 0.75
c(ll.y,ul.y) # 0.67, 0.83

# 95% CI for prob that truck using oil type Z lasts longer than 20000 hours
n.z <- 108
success.z <- 66
pi.z.obs <- success.z / n.z
se.pi.z.obs <- sqrt((pi.z.obs * (1 - pi.z.obs)) / n)
ll.z <- pi.z.obs - 1.96 * se.pi.z.obs
ul.z <- pi.z.obs + 1.96 * se.pi.z.obs
pi.z.obs   # 0.61
c(ll.z, ul.z)  # 0.52, 0.70

# write the estimator of the difference in the two probabilities
pi.d.obs <- pi.y.obs - pi.z.obs

# sampling distribution of pi.d.obs if H0 is true:
#   pi.d ~ N(0, SE[pi.d]^2)

# SE[pi.d]
# if h0 is true the combined estmate of pi 
pi.c.obs <- (84+66) / (112+108)

se.pi.d <- sqrt((pi.c.obs * (1 - pi.c.obs)) / n.y + (pi.c.obs * (1 - pi.c.obs)) / n.z)
se.pi.d

# calculate 95% CI for the difference in effectiveness
ll.d <- pi.d.obs - 1.96 * se.pi.d
ul.d <- pi.d.obs + 1.96 *se.pi.d
c(ll.d, ul.d)  # 0.15, 0.26


## EXERCISE 2: INFERENCE FOR ODDS RATIOS
# the data set contains variables 'death': 1 = die, 0 = survive; 'anterior': 1 = anterior, 0 = inferior
# the variable anterior records if someone has a myocardial infarction in the anterior or interior 
# of the heart. Variable death records if someone died within 48 hrs of admission into the hospital.
heart <- read.table(file = 'heart.txt', header = TRUE)
summary(heart)

# tabulate death vs anterior
(heart.table <- table(data.frame(heart$death, heart$anterior)))
# display this data in a barplot
barplot(heart.table, col = c(1,2))

# calculate the observed odds ratio of death from anterior infraction to interior infraction
(odds.ratio <- heart.table[1,1] * heart.table[2,2] / (heart.table[1,2] * heart.table[2,1])) # 2.2368

# calculate the log odds
(log.odds.ratio <- log(odds.ratio)) # 0.805

# calculate the approx. SE of log.odds.ratio
(log.phi.hat.se <- sqrt(sum(1/heart.table)))   # 0.155

# calculate the CI (symmetric on the log scale) and convert back to original scale
ul <- exp(log.odds.ratio + 1.96 * log.phi.hat.se)
ll <- exp(log.odds.ratio - 1.96 * log.phi.hat.se)
c(ll,ul)  # 1.65, 3.03
# this CI would suggest the odds would be greater than 1 of dying of anterior infraction to interior infraction







