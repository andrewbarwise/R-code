#EXERCISE 1: INFERENCE FOR PROPORTIONS
# 1a) ensure sample size is big enough for gaussian approx. distribution of the estimator pi
n <- 226
success <- 20
pi.obs <- success / n
pi.obs  # 0.09

n * pi.obs  # equals 20 
n * (1- pi.obs) # equals 206
# both these results are above 10 so the estimator of pi is applicable

# b) construct a 95% CI
se.pi <- sqrt((pi.obs * (1 - pi.obs)) / n)
ll <- pi.obs - 1.96 * se.pi
ul <- pi.obs + 1.96 * se.pi
c(ll,ul)  # 0.05, 0.13

# c) interpret these CI's
# if these figures were simulated repeatedly we would be confident that 95% of the 
# time this range of CI's would contain the true valu of pi.

# d) test h0: pi = 0.0722 vs h1 pi > 0.0722
pi.h0 <-  0.0722
se.pi.h0 <- sqrt((pi.h0 * (1 - pi.h0)) / n)
pnorm(pi.obs, mean= pi.h0, sd= se.pi, lower.tail=FALSE)
# p-value = 0.194, Therefore not enough evidence 
# to reject h0 in favour of pi being > than 0.0722.

# EXERCISE 2

dbinom(x= 2, size = 2, prob = 0.5) # 0.25
pi.h0.1 <- 0.25
se.pi.h0.1 <- sqrt((pi.h0.1 * (1 - pi.h0.1)) / n)
pnorm(0.5, mean = pi.h0.1, sd = se.pi.h0.1, lower.tail = FALSE)
# for the above pnorm() reject h0 of pi = 0.5 in favour of h1 pi > 0.5.  
# p-value = 1.98e-18

dbinom(x=3,size = 3, prob = 0.5) # 0.125
dbinom(x= 7, size = 8, prob = 0.5) # 0.03123

# DAYLIGHT SAVING POLL
day.poll <- read.table(file = "day.saving.poll(2).txt", header = TRUE)

summary(day.poll)

head(day.poll)

table(day.poll)

# use barplot() for basic visual aid.
barplot(table(day.poll), col = c(1,2))

# convey more detail with the barplot
barplot(table(day.poll), xlab = "Region", col = c("red","blue"),
              main = "Daylight savings referendum. Yes is blue, No is red",
              beside = TRUE, names.arg = c("Metro", "Regional"))

# EXERCISE 3: RANDOM NUMBER GENERATORS: BERNOULLI AND BINOMIAL DISTRIBUTIONS

# to make the study reproducible set.seed
set.seed(2402)

# simulate a sequence of n = 1000 obs from bernoulli r.v. wiyh pi = 0.2
n.obs <- 1000
binom.size <- 1
pi.true <- 0.2
# store simulated values in a vector
bern.sim <- rbinom(n.obs,binom.size,pi.true)

# a bernoulli r.v. with parameter pi has E[y] = pi and Var[y] = pi(1-pi). Examine whether 
# the sequence generated agrees

# theoretical expected value
mean.true.bern <- pi.true

# theoretical variance
var.true.bern <- pi.true * (1 - pi.true)

# calculate the estimated expected value ( the ave.)
mean.sim.bern <- mean(bern.sim)

# calculate the estimated expected variance
var.sim.bern <-var(bern.sim)

# compare the true and simulated means
c(mean.true.bern, mean.sim.bern)    # 0.2 and 0.201

# compare the true and simulated variances
c(var.true.bern, var.sim.bern)       # 0.16 and 0.161


# BINOKMIAL DISTRIBUTION
# if all we knew was the total number of 1's we would have a Binom(n.obs, pi). 
#Total number of 1's is obtained by:
sum.sim <- sum(bern.sim)   # equals 201. Just tells how many 1's out of 1000 with a prob. of 0.2

# now generate  n = 1 from a binomial distribution of m = 100  and pi = 200
n.obs <- 1 
binom.size <- 100
pi.true <- 0.2
binom.sim <- rbinom(n.obs,binom.size,pi.true)
binom.sim  # equals 19. (however value changes when ran multiple times)

# now generate n = 10000 and m = 100. pi = 0.2
n.obs <- 10000
binom.size <- 100
pi.true <- 0.2
binom.sim <- rbinom(n.obs,binom.size,pi.true)

# a binomial r.v. with parameter pi and size m has E[Y] = m*pi and Var[Y]= m*pi(1-pi). Examine if
# this supported
mean.true.binom <- binom.size * pi.true
var.true.binom <- binom.size * pi.true * (1 - pi.true)
mean.sim.binom <- mean(binom.sim)
var.sim.binom <- var(binom.sim)
c(mean.true.binom, mean.sim.binom) # 20.000, 19.956
c(var.true.binom, var.sim.binom)   # 16.000, 16.039

# produce a histogram of bern.sim and binom.sim

# plot 2 rows and 1 column
par(mfrow = c(2,1))

hist(bern.sim, col = 'yellow', xlab = 'Y', prob = TRUE,
     main= 'Histogram of bern.sim. Blue line indicates expected value')
abline(v = mean.true.bern, col ='blue', lwd = 2)

hist(binom.sim, col='yellow', xlab= "Z", prob = TRUE,
     main = "Histogram of binom.sim. Blue line indicates expected value")
abline(v=mean.true.binom, col='blue', lwd = 2)