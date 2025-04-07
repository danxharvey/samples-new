##########################
# SAMPLING INTRODUCTION #
#########################

# In order to mimic election polling we have an urn with 
# red and blue beads.  We want to know the spread between the
# beads.  You can select and bead and replace for $0.10 per bead
# The prize is $25
# In part 2 of the competition, the smallest margin of error wins
install.packages("tidyverse")
install.packages("dslabs")
library(tidyverse)
library(dslabs)
ds_theme_set()
take_poll(25)     # Random sample (p = blue beads)

# We want to predict the portion of blue beads
# blue = p
# red = 1-p
# spread = p - (1-p) = 2p-1
# Number of beads in the urn = the population
# Beads drawn (25) = the sample
# Number of blue beads is the parameter of interest
# Each time we take_poll(25) we get a different answer for p

# Define random variable X
# X = 1 for blue bead (what we want), or 0 for red
# Xbar is sum of Xs divided by N (number in sample)
# n*Xbar = X1 + X2 + X3 + ... + Xn
# As long as samples are returned to the urn for constant n

# When forecasting elections, estimate p is most accurate
# closest to election time as opinion will change the least

# Expected value of X (total blue in sample)
# E(N*Xbar) = N x p
# E(Xbar)= p      # N cancels out either side of equation

# Using standard error to deduce accuracy of our sample
# (1-0)*sqrt(p(1-p)) = sqrt( p(1-p) )
# As we divide by N to get Xbar we can also say that
# SE(Xbar) = sqrt( (p(1-p))/N )
# As N increase the Standard Error decreases
# Larger sample size means less standard error

# SE(Xbar) of the spread = 2*sqrt(p(1-p)/N)

# For exmample
# p = 0.51   (51% blue, 49% red)
# sample size = 1000
# SE = 0.015 or 1.5%
# This gives us a high confidence

# Expected value of the sum of draws E(S) = Np
# Standard error of this is SE(S) = sqrt(Np(1-p))

# EXERCISES
# N represents the number of people polled
N <- 25
# Create a variable p that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length=100)
# Create a variable se that contains the standard error of each sample average
se <- sqrt(p*(1-p)/N)
# Plot p on the x-axis and se on the y-axis
plot(p, se)


# N represents the number of people polled
N <- 25
# p represents the proportion of Democratic voters
p <- 0.45
# Calculate the standard error of the spread
# Print this value to the console.
2*sqrt(p*(1-p)/N)


######################################
# CENTRAL LIMIT THEORUM IN PRACTICE #
#####################################

# Distribution for a sum of draws is approximately normal
# Dividing a normally distributed random var by known constant
# Implies distribution of Xbar is approximately normal

# Suppose we are forecasting an election and want to know if we are
# within 1 percentage point of p, that we made a good forecast
# We're basically asking for
# Pr(|Xbar-p| <= 0.01)    # Distance Xbar is from p

# It's the same as asking
# Pr(Xbar <= p+0.01) - Pr(Xbar <= p-0.01)
# Using the subtraction trick we learnt in the last module
# convert Xbar to standard normal random variable Z
# Z = (Xbar-E(Xbar)) / SE(Xbar)

# Probability that Xbar is within 0.01 of p becomes

# Pr(Z <= 0.01/sqrt(p(1-p)/N) - Pr(Z <= -0.01/sqrt(p(1-p)/N))

# The CLT still works if Xbar is used instead of p.
# This is called a PLUG-IN ESTIMATE
# The wee hat means estimate SE(X)hat like in Sian in Welsh
# SE(Xbar)hat = sqrt(Xbar(1-Xbar)/N)

# Using our 12 blue and 13 red sample earlier
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
# use pnorm
pnorm(0.01/se) - pnorm(-0.01/se)

# The estimate is not very good, we're roughly 8% chance of being right
# What the CLT can do though is tell us what size N needs to be
# in order to get a very good estimate

# MARGIN OF ERROR
# This is 2 * SE(X)
# Works out to about 95% chance of being with 2 SE of p
pnorm(2) - pnorm (-2)

# If we had used a sample size of 2000
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/2000)    # ~ 1% error (2% margin error)
# use pnorm
pnorm(0.01/se) - pnorm(-0.01/se)    # 63% Chance of being within 1%


# MONTE CARLO SIMULATION FOR CORROBORATION

# We could write a simulation as in previous module but we don't
# know p.  We could pick a value or values of p and run tests
# Example let's set p to 0.45 and simulate a poll of 1000 people
p <- 0.45
n <- 1000
X <- sample(c(0, 1), size = n, replace=TRUE, prob=c(1-p, p))
xhat <- mean(X)

# Now enter into a monte carlo
b <- 10000
xhat <- replicate(b, {
          X <- sample(c(0, 1), size = N, replace=TRUE, prob=c(1-p, p))
          mean(X)
        })
mean(xhat)    # approx 0.45 as expected
sd(xhat)      # approx 1.5% as expected

# A histogram and Q-Q plot confirms the normal approximation
library(gridExtra)
p1 <- data.frame(xhat=xhat) %>%
        ggplot(aes(xhat)) +
        geom_histogram(binwidth=0.005, color="black")
p2 <- data.frame(xhat=xhat) %>%
        ggplot(aes(sample=xhat)) +
        stat_qq(dparams=list(mean=mean(xhat), sd=sd(xhat))) +
        geom_abline() +
        ylab("xhat") +
        xlab("Theoretical Normal")
grid.arrange(p1, p2, nrow = 1)

# We could never run this in real life because we don't know p or n
# but we can experiment to prove the theory


# THE SPREAD
# The spread = 2p-1
# As we are using plug-in estimates we can change this to
# 2Xbar - 1
# 2SE(X)hat       # we drop the -1 as it has no impact to variability

# So for our first example with p=0.48, n=25 and margin of error 0.2
# Our estimate of spread is 2p-1 = 0.04
# margin of error is 0.4 or 40%
# Not very useful

# KEY NOTES
# The spread between two outcomes with probabilities
# p and 1−p is  2p−1
# The expected value of the spread is  2X¯−1 
# The standard error of the spread is  2SE^(X¯)

# The margin of error of the spread is 2 times the margin of
# error of X¯ 


# WHY NOT JUST RUN A VERY LARGE POLL?
#
# Note for realistic values of p, between 0.35 and 0.65 for elections
# theory tells us that the maximum margin of error is about 3%
# with a 100,000 sample size
n <- 100000
p <- seq(0.35, 0.65, length=100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/n))  # p goes into x
data.frame(p=p, SE=SE) %>%
    ggplot(aes(p, SE)) +
    geom_line()

# So why not poll 100,000 people??
# Polling is quite expensive
# Theory has its limits:
# Polling is more complex than picking beads from an urn
# People might lie
# With phone polls, you can't ask people without phones
# They might not vote the same
# We don't know who is or isn't within our sample population
# How do we know if we are chatting to an active voter
# Are we reaching all possible voters?

# All the above lead to bias
# Historically we know that polls are biased although it is small
# About 1-2%


# EXERCISES
# Write a function called take_sample that takes p and N as arguements and returns the average value of a randomly sampled population.
take_sample <- function (p, N){
  vote <- sample(c(0, 1), size = N, replace=TRUE, prob=c(1-p, p))
  mean(vote)
}
# Use the set.seed function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define p as the proportion of Democrats in the population being polled
p <- 0.45
# Define N as the number of people polled
N <- 100
# Call the take_sample function to determine the sample average of N randomly selected people from a population containing a proportion of Democrats equal to p. Print this value to the console.
take_sample(p, N)


# The variable B specifies the number of times we want the sample to be replicated
B <- 10000
# Create an objected called errors that replicates subtracting the result of the take_sample function from p for B replications
errors <- replicate(B, {
            x <- p - take_sample(p, N)
          })
# Calculate the mean of the errors. Print this value to the console.
mean(errors)


# Define p as the proportion of Democrats in the population being polled
p <- 0.45
# Define N as the number of people polled
N <- 100
# The variable B specifies the number of times we want the sample to be replicated
B <- 10000
# Use the set.seed function to make sure your answer matches the expected result after random sampling
set.seed(1)
# We generated errors by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))
# Calculate the mean of the absolute value of each simulated error. Print this value to the console.
mean(abs(errors))


# Define p as the proportion of Democrats in the population being polled
p <- 0.45
# Define N as the number of people polled
N <- 100
# The variable B specifies the number of times we want the sample to be replicated
B <- 10000
# Use the set.seed function to make sure your answer matches the expected result after random sampling
set.seed(1)
# We generated errors by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))
# Calculate the standard deviation of errors
sqrt(mean(errors^2))


# Define p as the expected value equal to 0.45
p <- 0.45
# Define N as the sample size
N <- 100
# Calculate the standard error
sqrt(p*(1-p)/N)


# Define p as a proportion of Democratic voters to simulate
p <- 0.45
# Define N as the sample size
N <- 100
# Use the set.seed function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define X as a random sample of N voters with a probability of picking a Democrat (1) equal to p
X <- sample(c(0, 1), N, replace=TRUE, prob=c(1-p, p))
# Define X_bar as the average sampled proportion
X_bar <- mean(X)
# Calculate the standard error of the estimate. Print the result to the console.
sqrt(X_bar*(1-X_bar)/N)


# Define p as the proportion of Democrats in the population being polled
p <- 0.45
# Define N as the number of people polled
N <- 100
# The variable B specifies the number of times we want the sample to be replicated
B <- 10000
# Use the set.seed function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Generate errors by subtracting the estimate from the actual proportion of Democratic voters
errors <- replicate(B, p - take_sample(p, N))
# Generate a qq-plot of errors with a qq-line showing a normal distribution
qqnorm(errors)
qqline(y=errors)


# Define p as the proportion of Democrats in the population being polled
p <- 0.45
# Define N as the number of people polled
N <- 100
# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.
1 - pnorm(0.5, p, sqrt(p*(1-p)/N))


# Define N as the number of people polled
N <-100
# Define X_hat as the sample average
X_hat <- 0.51
# Define se_hat as the standard error of the sample average
se_hat <- sqrt(X_hat*(1-X_hat)/N)
# Calculate the probability that the error is 0.01 or larger
1 - (pnorm(0.01/se_hat) - pnorm(-0.01/se_hat))
# Pay more attention to the question --- OR LARGER



#########################
# CONFIDENCE INTERVALS #
########################

# ggplot uses geom_smooth() to show confidence intervals
data("nhtemp")
data.frame(year=as.numeric(time(nhtemp))
          ,temperatures=as.numeric(nhtemp)) %>%
      ggplot(aes(year, temperatures)) +
      geom_point() +
      geom_smooth() +
      ggtitle("Average Yearly Temperatures in New Haven")
# The shaded area is created using the concept of confidence intervals

# To get 95% confidence we want p to be in the range
# [Xbar - 2SEhat(Xbar), Xbar + 2SEhat(Xbar)]

# The start and end of this range are random variables
# Every time that we take a sample, they change

# We can illustrate this by running monte carlo twice
p <- 0.45
N <- 1000
x <- sample(c(0, 1), N, replace=TRUE, prob=c(1-p, p))
X_hat <- mean(x)
SE_hat <- sqrt(X_hat*(1-X_hat)/N)
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)

# To find the probability that p is in this interval we use
# Pr( Xbar - 2SEhat(Xbar) <= p <= Xbar + 2SEhat(Xbar) )

# Becomes
# Pr( -2 <= Xbar-p/SEhat(Xbar) <= +2)

# The term in the middle is an approximately random normal variable
# with mean 0 and standard error 1, or Z

# Pr( -2 <= Z <= 2)
# Probability of a standard normal variable being between -2 and 2

# If we want a different confidence interval we simply solve for Z
# where this formula holds true
# Pr( -z <= Z <= +z) = 0.99

# calculate z to solve for 99% confidence interval
z <- qnorm(0.995)
# demonstrating that qnorm gives the z value for a given probability
pnorm(qnorm(0.995))
# demonstrating symmetry of 1-qnorm
pnorm(qnorm(1-0.995))
# demonstrating that this z value gives correct probability for interval
pnorm(z) - pnorm(-z)

# To get exact number for 95% confidence interval
qnorm(0.975)  # 1.959964

# To prove q with matchematics
# 1 - (1-q)/2 - (1-q)/2  =  1 - (1-q) = q



# MONTE CARLO FOR CONFIDENCE INTERVALS
p <- 0.45
N <- 1000
B <- 10000
inside <- replicate (B, {
    x <- sample(c(0, 1), N, replace=TRUE, prob=c(1-p, p))
    X_hat <- mean(x)
    SE_hat <- sqrt(X_hat*(1-X_hat)/N)
    between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)
})
mean(inside)

# IMPORTANT NOTES
# The 95% confidence intervals are random, p is not random
# 95% refers to the probability that the inteval falls on p
# It is technically incorrect to say p falls into the interval


# POWER
# Pollsters do not become successful for predicting accurate intervals
# They become successful for predicting a win

# For our N=25 sample we can show
N <- 25
X_hat <- 0.48
(2*X_hat -1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/sqrt(N))
# Huge interval including zero, they would have to declare no idea

# The problem is that we have a small sample size
# In statistics this is called a lack of power
# To increase the power we would increase the sample size and 
# lower our standard error

# The power can be thought of as the probability of detecting a
# spread different than zero (i.e. an indication of who might win)


# P-VALUES
# Suppose we just want to know whether there are more red or blue beads
# We are asking if (2p-1) > 0
N <- 100
blue <- 52
spread = 0.04   # 4% spread

# The null hypothesis is the skeptic's hypothesis
# The spread is zero because we know chance exists
# 2Xbar - 1 = 0.04
# p-value means how likely is it to see a value this large when
# the null hyposthesis is true?
# Pr( |Xbar - 0.5| ) > 0.02       # 52 - 2 = 50
# Same as asking the spread is 4 or more
# Null hypothesis is that the spread is 0 and p = 0.5

# Under the null hyposthesis we know that
# sqrt(N) * (Xbar-0.5) / sqrt(p(1-p))
# is standard normal
# We've taken the random variable and divided it by its SE after
# subtracting its expected value

# We can reduce that equation to this one in order to compute
# a p-value
# Pr(sqrt(N) * |Xbar-0.5| / 0.5) > Z        # Z is standard normal
N <- 100
z <- sqrt(N)*0.02/0.5
1 - (pnorm(z) - pnorm(-z))  # 0.6891565
# The p-value is 69% or 0.69
# This means that there's a 69% chance of seeing more than 52 blue
# if the null hypothesis is true.  Not a strong argument to say that
# there are definitely 52 beads in the urn

# Further, if a 95% confidence interval of the spread does not include
# zero, we know that the p-value must be less than 0.05 (1 - 95%)

# We prefer working with confidence intervals over p-values as it
# gives us an idea of the size of the estimate.
# The p-value simply reports a probability and says nothing about
# the significance of the finding in the context of the problem.

# Suppose we want to find the p-value of an observation 2 standard
# deviations larger than the mean. This means we are looking for
# anything with |z| >= 2
# We reject the null hypothesis if outside of +/- 2sd

# The right tail (more than 2sd) can be found with
1-pnorm(2)
# As we want both tails of the distribution (two tailed p-value)
# we know that our desired value is just
2*(1-pnorm(2))

# Recall that, by default, pnorm() gives the CDF for a normal
# distribution with a mean of  μ=0  and standard deviation of  σ=1 

# To find p-values for a given z-score z in a normal distribution
# with mean mu and standard deviation sigma
# use 2*(1-pnorm(z, mu, sigma)) instead.

# EXERCISES
# Load the data
data(polls_us_election_2016)
# Generate an object polls that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls <- polls_us_election_2016 %>% filter(enddate >= '2016-10-31' & state == "U.S.")
# How many rows does polls contain? Print this value to the console.
nrow(polls)
# Assign the sample size of the first poll in polls to a variable called N. Print this value to the console.
N <- polls$samplesize[1]
N
# For the first poll in polls, assign the estimated percentage of Clinton voters to a variable called X_hat. Print this value to the console.
X_hat <- polls$rawpoll_clinton[1]/100
X_hat
# Calculate the standard error of X_hat and save it to a variable called se_hat. Print this value to the console.
se_hat <- sqrt((X_hat*(1-X_hat))/N )
se_hat
# Use qnorm to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called ci.
z <- qnorm(0.975)
ci <- c(X_hat - z*se_hat, X_hat + z*se_hat)


# The polls object that filtered all the data by date and nation has already been loaded. Examine it using the head function.
head(polls)
# Create a new object called pollster_results that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.
pollster_results <- mutate(polls, X_hat = rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/samplesize), lower= X_hat - qnorm(0.975)*se_hat, upper = X_hat + qnorm(0.975)*se_hat) %>%
  select(pollster,enddate, X_hat, se_hat, lower, upper)


# The pollster_results object has already been loaded. Examine it using the head function.
head(pollster_results)
# Add a logical variable called hit that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average hit result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called avg_hit.
avg_hit <- mutate(pollster_results, hit = ifelse(0.482>=lower & 0.482<=upper, 1, 0)) %>%
              summarize(mean(hit))


# Add a statement to this line of code that will add a new column named d_hat to polls. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.")  %>% mutate(d_hat = (abs(rawpoll_clinton - rawpoll_trump)/100))
# Assign the sample size of the first poll in polls to a variable called N. Print this value to the console.
N <- polls$samplesize[1]
N
# Assign the difference d_hat of the first poll in polls to a variable called d_hat. Print this value to the console.
d_hat <- polls$d_hat[1]
d_hat
# Assign proportion of votes for Clinton to the variable X_hat.
X_hat <- (d_hat+1)/2
# Calculate the standard error of the spread and save it to a variable called se_hat. Print this value to the console.
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
se_hat
# Use qnorm to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called ci.
z <- qnorm(0.975)
ci <- c(d_hat - z*se_hat, d_hat + z*se_hat)


# The subset polls data with d_hat already calculated has been loaded. Examine it using the head function.
head(polls)
# Create a new object called pollster_results that contains columns for pollster name, end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.
pollster_results <- mutate(polls, X_hat = (d_hat+1)/2, se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize), lower = d_hat - qnorm(0.975)*se_hat, upper = d_hat + qnorm(0.975)*se_hat) %>% 
                        select(pollster, enddate, d_hat, lower, upper)


# The pollster_results object has already been loaded. Examine it using the head function.
head(pollster_results)
# Add a logical variable called hit that indicates whether the actual value (0.021) exists within the confidence interval of each poll. Summarize the average hit result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called avg_hit.
avg_hit <- mutate(pollster_results, hit = ifelse(0.021>=lower & 0.021<=upper, 1, 0)) %>% summarize(mean(hit))



# The polls object has already been loaded. Examine it using the head function.
head(polls)
# Add variable called error to the object polls that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster.
polls <- mutate(polls, error = abs(((2*rawpoll_clinton)-1) - 0.021)) %>%
  ggplot(aes(x = pollster, y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# USE DPLYR FILTER FOR POLLSTERS WITH MULTIPLE POLLS
# The polls object has already been loaded. Examine it using the head function.
head(polls)
# Add variable called error to the object polls that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster, but only for pollsters who took 5 or more polls.
polls <- mutate(polls, error = abs(((2*rawpoll_clinton)-1) - 0.021)) %>% group_by(pollster) %>% filter(n() >= 5) %>%
  ggplot(aes(x = pollster, y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#####################
# POLL AGGREGATORS #
####################

# During the 2012 electioN Obama won by 3.9%.  A week before Nate
# Silver was 90% sure, everyone thought he was insane.  What did
# he see to be so confident?

# Use monte carlo for the 12 polls taken the week before the election
# Use the actual sample sizes as data and construct 95% confidence
# intervals.  We'll use the actual outcome as the spread (d=0.039)
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2    # Proportion p based on the spread for Obama

# Generate confidence intervals
confidence_intervals <- sapply(Ns, function(N) {
            X <- sample(c(0, 1), N, replace=TRUE, prob=c(1-p, p))
            X_hat <- mean(X)
            SE_hat <- sqrt(X_hat*(1-X_hat)/N)
            2*c(X_hat, X_hat-2*SE_hat, X_hat+2*SE_hat)-1
        })

# Generate a data frame containing the results
polls <- data.frame(poll=1:ncol(confidence_intervals),
                      t(confidence_intervals),
                      sample_size=Ns)
# Assign column names
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")

# Results
polls
# All 12 polls contain the zero range and the 95% confidence intervals
# fall on top of the mean zero meaning that it really does look like
# a tossup

# However, they are missing a key insight
# By aggregating the data into one large sample size we can make
# an even more precise estimate of the 95% confidence interval
# We construct a weighted estimate of the total spread
d_hat <- polls %>% summarize(avg = sum(estimate*sample_size)/sum(sample_size)) %>%
                    .$avg

# Now we can construct an estimate for the proportion voting
# for Obama
p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
moe       # margin of error

# d_hat and moe give us an estimate of the spread and margin of
# error for that spread
round(d_hat*100, 1)
round(moe*100, 1)
# 3% +/- 1.8%
# This includes the actual result but criticalls DOES NOT include
# zero, which would represent a tie.

# Whilst this method is quite accurate, the actual data science
# of predicting election results is far more complicated and involves
# the concept of statistical modelling


# MULTI-LEVEL MODELS
# What we saw above is an example of multi-level models


# POLL DATA AND POLLSTAR BIAS
# This section uses actual polling data used by fivethirtyeight
# for the actual US 2016 presidential election
# This is included as part of the dslabs package
library(dslabs)
data("polls_us_election_2016")
names(polls_us_election_2016)

# First we filter data, note, anything less than grade B quality
# is removed.  Ungraded remains as it might be good data
polls <- polls_us_election_2016 %>% filter(state == "U.S." & 
            enddate >= "2016-10-31" &
            (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
        mutate(spread = (rawpoll_clinton - rawpoll_trump)/100)
# we will use p = clinton, 1-p = trump
# spread = 2p-1 = d = difference
# Expected value = election night spread
# Standard error - 2*sqrt(p*(1-p)/N)

# Using our urn model we can calculate an aggravated d_hat
d_hat <- polls %>% summarize(avg = sum(spread*samplesize)/sum(samplesize)) %>%
  .$avg

# We can then get a expected p for the spread and margin of error
p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
moe       # margin of error

# The actual result had a 2.1% spread which is outside of the 95%
# confidence interval

# A histogram of spreads shows us
polls %>% ggplot(aes(spread)) +
          geom_histogram(color="black", binwidth = 0.01)

# This shows us that the spreads are not normally distributed
# and the standard error appears to be larger than 0.0066
# The theory is not quite working here

# If we look again at the data we can see that several pollsters
# are involved and some are taking several polls per week
polls %>% group_by(pollster) %>% summarize(n())

# Let's filter by pollsters who are polling more than 5 times
polls %>% group_by(pollster) %>%
    filter(n() >= 6) %>%
    ggplot(aes(pollster, spread)) +
    geom_point() +
    theme(axis.text.x = element_text(angle=90, hjust=1))

# The table of spreads
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se=2*sqrt(p_hat*(1-p_hat)/median(samplesize)))
# Results between 0.0183 and 0.0333
# This appear to be right according and consistent with the plots
# However, the theory does not take account that different pollsters
# will produce difference results - 5% Clinton win v 4% Trump win

# Fivethirtyeight call this "house effect" but the official term
# is "pollster bias"

# Instead of using the simplistic urn model, we will develop
# data-driven models to produce better estimates and a better
# confidence interval


# DATA DRIVEN MODELS
# For each pollster let's collect their last reported result
# before the election
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
      filter(enddate == max(enddate)) %>%
      ungroup()

one_poll_per_pollster %>% ggplot(aes(spread)) + 
    geom_histogram(color="black", binwidth = 0.01)

# Now let's redefine the urn model to be not so directly related
# to the beads
# Instead, the urn contains poll results from different pollsters
# The expected value of the urn is the actual spread
# and d still equals 2p-1
# The urn values are now continuous numbers between -1 and 1
# The SE formula no longer stands as the values are not discrete
# The SE will now include pollster-to-pollster variability
# it also includes the sample variability from the polling
# This standard deviation is now an unknown parameter

# In summary, we now have 2 unknown parameters:
# Expected value = d
# Standard deviation = sigma

# The task is to now estimate d and provide inference for it
# Because we are estimating from X1...Xn as a random sample
# The Central Limit Theorum still works for these average of values
# due to the fact that it is an average of independent random variables
# For a large sample the X_bar will be approximately normal
# with E(X) = d and SD = sigma/sqrt(N) if large N is large enough

# Although we don't know sigma the theory tells us that we can
# estimate the urn model sigma, the unobserved sigma, with the 
# sample SD from:

# Sample standard deviation
# s = 1/(N-1) * Summation(i=1toN) of (Xi - Xbar)squared

# R has a function to calcualte the sample SD for us
sd(one_poll_per_pollster$spread)

# We are now ready to create a confidence interval based on our new
# data driven model using the CLT
results <- one_poll_per_pollster %>%
    summarize(avg=mean(spread), se=sd(spread)/sqrt(length(spread))) %>%
    mutate(start=avg-1.96*se, end=avg+1.96*se)
round(results*100, 1)

# Our new Confidence interval is larger, includes the result
# and does not include zero.

# We can now be confident that Clinton will win the popular vote
# (which she did) however we are not ready to give a probability
# of her chances of winning.  To do that we must learn
# Bayesian Statistics


# Load the dslabs package and data contained in heights
library(dslabs)
data(heights)
# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>% .$height
# Calculate the mean and sd
mean(x)
sd(x)

# The vector of all male heights in our population x has already been loaded for you. You can examine the first six elements using head.
head(x)
# Use the set.seed function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define N as the number of people measured
N <- 50
# Define X as a random sample from our population x
X <- sample(x, N, replace=TRUE)
# Calculate the mean and sd
mean(X)
sd(X)


# The vector of all male heights in our population x has already been loaded for you. You can examine the first six elements using head.
head(x)
# Use the set.seed function to make sure your answer matches the expected result after random sampling
set.seed(1)
N <- 50
# Define X as a random sample from our population x
X <- sample(x, N, replace = TRUE)
# Define se as the standard error of the estimate. Print this value to the console.
se <- sd(X)/sqrt(N)
se
# Construct a 95% confidence interval for the population average based on our sample. Save the lower and then the upper confidence interval to a variable called ci.
X_hat <- mean(X)
ci <- c(X_hat - qnorm(0.975)*se, X_hat + qnorm(0.975)*se)


# Define mu as the population average
mu <- mean(x)
# Use the set.seed function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Define N as the number of people measured
N <- 50
# Define B as the number of times to run the model
B <- 10000
# Define an object res that contains a logical vector for simulated intervals that contain mu
res <- replicate (B, {
  X <- sample(x, N, replace = TRUE)
  between(mu, mean(X) - qnorm(0.975)*(sd(X)/sqrt(N)), mean(X) + qnorm(0.975)*(sd(X)/sqrt(N)))
})
# Calculate the proportion of results in res that include mu. Print this value to the console.
mean(res)


# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")
# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 
# Make a boxplot with points of the spread for each pollster
polls %>% group_by(pollster) %>%
      ggplot(aes(pollster, spread)) +
      geom_boxplot() +
      geom_point()


# The polls data have already been loaded for you. Use the head function to examine them.
head(polls)
# Create an object called sigma that contains a column for pollster and a column for s, the standard deviation of the spread
sigma <- polls %>% group_by(pollster) %>%
  summarize(s=sd(spread))
# Print the contents of sigma to the console
sigma


# The polls data have already been loaded for you. Use the head function to examine them.
head(polls)
# Create an object called res that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>%
  summarize(avg=mean(spread), s=sd(spread), num=n())
# Store the difference between the larger average and the smaller in a variable called estimate. Print this value to the console.
estimate <- max(res$avg)-min(res$avg)
# Store the standard error of the estimates as a variable called se_hat. Print this value to the console.
se_hat <- sqrt((estimate*(1-estimate)/1))
se_hat<-sqrt(res$s[1]^2/res$num[1]+res$s[2]^2/res$num[2])
# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called 'ci'.
ci <- c(estimate - qnorm(0.975)*se_hat, estimate + qnorm(0.975)*se_hat)
ci


# We made an object res to summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 
# The variables estimate and se_hat contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])
# Calculate the p-value
2*(1 - pnorm(estimate/se_hat, 0, 1))


# We compute statistic called the t-statistic by dividing our
# estimate of b2???b1 by its estimated standard error:
# (Y2bar???Y1bar)/sqrt(s2^2/N2 + S1^2/N1)


# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()
# Create an object called var that contains columns for the pollster, mean spread, and standard deviation. Print the contents of this object to the console.
var <- polls %>% group_by(pollster) %>%
    summarize(avg=mean(spread), s=sd(spread))
var



########################
# BAYESIAN STATISTICS #
#######################

# In the urn model p is not random so doesn't make sense to talk
# about probability
# With Bayesian statistics we do assume that it is random

# Forecasters also use models to discuss variability at different
# levels - e.g. sampling and pollster-to-pollster variability

# One of the most successful approaches ever used is the
# Hierarchical Model, best explained by Bayesian statistics


# Let's use a cystic fibrosis test as an introductory example
# Suppose a test has an accuracy of 99%
# Bayes Theory:
# Probability of +ve test given you have it = 0.99
# Probability of -ve test given you don't have it = 0.99
# Prob(+ | D=1) = 0.99, Prob(- | D=0) = 0.99

# Suppose we test a random person and they are test positive
# Prob(D=1 | +) ??    Prob they have CF given they test +ve

# CF has a 1 in 3900 occurrence rate
# Prob(D=1) = 1/3900      # 0.00025

# Pr(A|B) = Pr(A and B) / Pr(B)
#         = Pr(B|A)*Pr(A)/Pr(B)

# In our example
# Pr(D=1|+) = Pr(+|D=1)*Pr(D=1)/Pr(+)
#   = Pr(+|D=1)*Pr(D=1) / (Pr(+|D=1)*Pr(D=1) + Pr(+|D=0)*Pr(D=0))

# Using what know

# Pr(D=1|+) = 0.99*0.00025 / (0.99*0.00025 + 0.01*0.99975)
# = 0.02 = 2%

# Whilst this may seem counter-intuitive given the 99% test accuracy
# We have to factor in the random chance that the person chosen
# at random actually has the disease.
# This is Bayesian thinking

# Let's see this proven with monte carlo
prev <- 1/3900
N <- 100000
outcome <- sample(c("Disease", "Healthy"), N, replace=TRUE, prob=c(prev, 1-prev))
N_D <- sum(outcome == "Disease")
N_H <- N - N_D
# This comes out around 23.  This makes the probability that we
# will see some false positives on the test quite high

# Let's analyse this:
accuracy <- 0.99
test <- vector("character", N)    # Set up empty vector
test[outcome=="Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob=c(accuracy, 1-accuracy))
test[outcome=="Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob=c(accuracy, 1-accuracy))

# The outputs
table(outcome, test)
# If we look at +Disease / total of + columns ~~ 2%
# This is what Bayes Theorum tells us it should be


##################################################
# THE HIERARCHICAL MODEL OF BAYESIAN STATISTICS #
#################################################

# Using a baseball example we can see the useless of
# Hierarchical Models in Bayesian Statistics

# Example, Jose Inglesias in his rookie year had a batting
# average of .450 after his first 20 bats. This was phenomenal
# No batter finished a season higher than .400 since 1941

# So far the techniques we've learnt are referred to as frequentist
# statistics and the best we can do if offer a confidence interval

# We can think of batting outcomes as binomial with a success
# rate of p (p being a hit)

# So with a success rate of 0.450 and 20 bats the standard error
# can be computed as:
# sqrt(p(1-p)/N) = 0.111
# With a 95% confidence interval of 0.228 to 0.672
# This has 2 problems
# 1) It's very large and not very useful
# 2) It's centred at .450 which implies this guy will break records

# Intuitively we know that Jose is extremely unlikely to break this
# record because we are implicitly using hierarchical models that
# factors in years of information

# Let's quantify this intuition by looking at batting averages
# for all players with more than 500 bats from 2010-2012
# We note that the average player had 0.275 and sd 0.027
# We see immediately that Jose is over 6SD from the average
# Is he lucky or the best batter seen in the last 50 years?
# How much is luck and how much is talent?
# If we believe it's luck we should cash in and trade him to a 
# team who believes it's down to talent

# The hierarchical model provides a mathematical description
# of how we see the 0.45 result

# First we pick a player with an intrinsic ability who matches
# our plots.  We see this player has a batting average that is
# normally distributed with mean 0.275 and SE 0.027

# The second level of variability is luck, no matter how good you
# are there is always an element of luck

# If we add up all these successes and failures as 0s and 1s then
# the CLT tells us that the observed average Y has a normal dist.
# with expected value p and a standard error sqrt(p(1-p)/N)
# Using the tilda ~ to represent the distribution of something
# p ~ N(mu, tau) describes randomness in picking a player
# p is normally distributed with mean mu and SE of tau

# Now to factor in the next level, observed batting average Y
# Y|p ~ N(p, sigma) describes the randomness in the performance
# of this player

# In this example we know
# mu = 0.275
# tau = 0.027
# sigma squared = p(1-p)/N

# These 2 levels form our hierarchical model with 2 variables
# The first being player-to-player variability
# The second being variability due to luck when batting

# In the Bayesian framework the first level is called the prior
# distribution and the second the sampling distribution

# For Jose, we want to predict his innate batting ability in the
# form of his true batting average p.  Our hierarchical model is:
# p ~ N(0.275, 0.027)
# Y|p ~ N(p, 0.111)

# We can now compute a posterior distribution to summarise our
# prediction of p

# The is a continuous version of Bayes' rule to derive a posterior
# probability function for p assuming that we have observed Y
# equals little y (because the normal distribution is also continuous)

# E(p|y)  = B*mu + (1-B)*Y
#         = mu + (1-B)*(Y-mu)
# B = simga squared / (sigma squared + tau squared)

# mu is the weighted average for all baseball players, and
# Y which is what we have observed for Jose
# If B = 1 then we say Jose is average and we predict mu
# If B = 0 then we say the past doesn't count, Jose is 0.450 as seen
# Looking at the formula for B, B will be closer to 1 when
# sigma is large.  Sigma is large when we have a big standard error
# meaning that B approaches 1 when we don't trust our observed data
# Conversely, if sigma is very small then we really trust our
# observed data and we'll ignore the past data and predict Y

# This weighted average is known as SHRINKING because it shrinks
# the observed average Y towards the known prior average mu

# For Jose we can now fill in these numbers and see what happens
# B = 0.111^2 / (0.111^2 + 0.027^2) = 0.944
# This means we don't particularly trust our observed data

# E(p|Y=0.450)  = B*0.275 + (1-B)*0.450
#               = 0.994*0.275 + (1-0.994)*0.450
#               = 0.285

# We can calculate a standard error
# SE(p|y)^2 = 1/(1/sigma^2 + 1/tau^2)
#           = 1/(1/0.111^2 + 1/0.027^2)
#           = 0.00069
# Take square root to get standard deviation (SE)
# SD = 0.026

# We started with a frequentist 95% confidence interval as
# 0.450 +/- 0.220

# Our Bayesian approach is known as an EMPIRICAL BAYESIAN approach
# We can construct a 95% credible interval
# This is a region centred on the expected value p with 95% chance
# of actually occurring
# E(p|Y) +/- 2SE(p|y)
# E(p|Y) = 0.285 +/ 0.052
# This gives us a lower average and a much smaller range

# His batting average after April came in at 0.293
# The Bayesian approach was far more accurate


# EXERCISES
# Define Pr_1 as the probability of the first son dying of SIDS (incorrectly calculated at 1 in 73 million in court)
Pr_1 <- 1/8500
# Define Pr_2 as the probability of the second son dying of SIDS
Pr_2 <- 1/100
# Calculate the probability of both sons dying of SIDS. Print this value to the console.
(1/100)*(1/8500)


# Define Pr_1 as the probability of the first son dying of SIDS
Pr_1 <- 1/8500
# Define Pr_2 as the probability of the second son dying of SIDS
Pr_2 <- 1/100
# Define Pr_B as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2
# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000
# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50
# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
pr_AB = Pr_BA*Pr_A/Pr_B
pr_AB
# This takes into account that it is rare for mothers to murder their children


# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)
# Create an object polls that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
# Examine the polls object using the head function
head(polls)
# Create an object called results that has two columns containing the average spread (avg) and the standard error (se). Print the results to the console.
results <- polls %>% summarize(avg=mean(spread), se=sd(spread)/sqrt(length(spread)))
results


# The results object has already been loaded. Examine the values stored: avg and se of the spread
results
# Define mu and tau
mu <- 0
tau <- 0.01
# Define a variable called sigma that contains the standard error in the object results
sigma <- results$se 
# Define a variable called Y that contains the average in the object results
Y <- results$avg
# Define a variable B using sigma and tau. Print this value to the console.
B <- (sigma^2) / (sigma^2 + tau^2)
# Calculate the expected value of the posterior distribution
B*mu+(1-B)*Y


# Here are the variables we have defined
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
# Compute the standard error of the posterior distribution. Print this value to the console.
sqrt(1/(1/sigma^2 + 1/tau^2))


# Here are the variables we have defined in previous exercises
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
# Uses -1 to 1 as a continuous variable
ci <- B*mu + (1-B)*Y + c(-1,1)*qnorm(0.975)*sqrt( 1/ (1/sigma^2 + 1/tau^2))


# Assign the expected value of the posterior distribution to the variable exp_value
exp_value <- B*mu + (1-B)*Y 
# Assign the standard error of the posterior distribution to the variable se
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
# Using the pnorm function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0, exp_value, se)


# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg
# Define a variable taus as different values of tau
taus <- seq(0.005, 0.05, len = 100)
# Create a function called p_calc that generates B and calculates the probability of the spread being less than 0
p_calc <- function(tau) {
  B <- (sigma^2/(sigma^2+tau^2))
  EV <- B*mu + (1-B)*Y
  s <- sqrt(1/(1/sigma^2 + 1/tau^2))
  pnorm(0, EV, s)
}
# Create a vector called ps by applying the function p_calc across values in taus
ps <- sapply(taus, p_calc)
# Plot taus on the x-axis and ps on the y-axis
plot(taus, ps)


#########################
# ELECTION FORECASTING #
########################

# Saying that Obama has a 91% chance of winning the election is
# a probabilistic statement about the parameter d
# Fivethirtyeight gave Clinton an 81.4% chance of winning the
# popular vote.  They used a Bayesian approach like this:

# d ~ N(mu, tau) describes our best guess had we not seen polling data
# Xbar|d ~ N(d, sigma) describes randomness due to sampling and pollster effect
# where d is the spread and is assumed to be Normal

# We'll set mu to 0 - this means we have no idea who will win
# For tau, we'll use 0.035 which is historical spread of 3.5%

# Now we can use formulas for the posterior distribution
# to report the probability of d being bigger than zero
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2/(sigma^2+tau^2)

posterior_mean <- B*mu+(1-B)*Y
posterior_se <- sqrt(1/(1/sigma^2 + 1/tau^2))

# We use the fact that this posterior distribution is approx
# Normal to make a probability statement.  We can report a
# credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# Probability that d is bigger than zero
1 - pnorm(0, posterior_mean, posterior_se)

# This is almost 100% probability that Clinton will win the 
# popular vote.  Why is this?

# After elections are over we can look at the difference between
# the actual results and the pollster's predictions
# An important observation is that our model does not account
# for is that it is common to see what is called general bias
# that affects many pollsters the same

# Whist we cannot know the bias before the election we can add
# a variance to our model to account for this variability

# EXTRA CODE BLOCK
library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)


###########################################
# MATHEMATICAL REPRESENTATIONS OF MODELS #
##########################################

# If we assume no general bias we observe
# E(X) = d, SE = 2*sqrt(p*(1-p)/N)
# Mathematically:  Xj = d + Epsilon_j   # sqiggly Greek E
#       j is the index of the different polls
#       Epsilon explains the error from poll to poll
# If d was 2.1 and N was 2000, we could simulate 6 data points
J <- 6
N <- 2000
d <- 0.21
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))

# Now supposed we have 6 data points from 5 pollsters. We need to
# add 2 new indices.  One for pollster, one for poll each pollster
# understakes. We use X_ij with i=pollster, j=jth poll from pollster
# X_i,j = d + E_i,j
# To simulate data we now have to use a loop by using sapply
I <- 5
J <- 6
N <- 2000
d <- 0.21
p <- (d+1)/2
X <- sapply(1:I, function(i){
        d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
      })

# We need to add another term for the pollster affect to describe
# variability between pollsters - h_i for the house effect between
# pollsters
# X_i,j = d + h_i + E_i,j
I <- 5
J <- 6
N <- 2000
d <- 0.21
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)         # 0.025 historical bias percentage
X <- sapply(1:I, function(i){
  d + h[i] + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})
# Note that h[i] is specific to each pollster

# To see the house effect we would have to have to collect data
# and compare it to the average of all polls each year
# Now we add another term b to account for the general bias
# X_i,j = d + b + h_i + E+i,j
# where b has expected value 0 and standard error about 2.5%
# We do not observe b every year, it is a historical value for
# all years.  Because we know this we now have the implication:

# Xbar = d + b + 1/N summation from i=1 toN of X_i
# SD(Xbar) = sqrt(sigma^2/N + Sigma_b^2)
# Because b is in every measurement, a constant, the average does
# not reduce its variance

# If we now redo the Bayesian calculation taking this variation
# into account we get a result much closer to the Fivethirtyeight
# result
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + 0.025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu+(1-B)*Y
posterior_se <- sqrt(1/(1/sigma^2 + 1/tau^2))

# Clinton's new chance of winning given as:
1 - pnorm(0, posterior_mean, posterior_se)
# 81.7% much lower than 99.999% because we are including
# the general bias figure


##########################################
# PREDICTING THE ELECTORAL COLLEGE VOTE #
#########################################

# Here are the top 5 states ranked by electoral vote
results_us_election_2016 %>% arrange(desc(electoral_votes)) %>%
    top_n(5, electoral_votes)

# With some minor exceptions that we don't discuss the electoral
# vote is won all or nothing by simple majority.  A candidate
# will take all votes for that state 100%
# The idea behind this is to avoid a few large states having
# too much power and dominating the US election

# To predict the electoral college vote we start by aggragating
# the results from polls
results <- polls_us_election_2016 %>%
      filter(state!="U.S." & 
               !grepl("CD", state) &
               enddate >= "2016-10-31" &
               (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
      mutate(spread=(rawpoll_clinton-rawpoll_trump)/100) %>%
      group_by(state) %>%
      summarize(avg=mean(spread), sd=sd(spread), n=n()) %>%
      mutate(state=as.character(state))
# This gives us the 10 closest races according to the polls
results %>% arrange(abs(avg))
# These are called the battleground states

# We are going to use left_join to add number of electoral votes
results <- left_join(results, results_us_election_2016, by="state")

# Some states have no polls as the winner is pretty much known
results_us_election_2016 %>% filter(!state %in% results$state)

# This code assigns a SD to the states that had just one poll by substituting
# the missing value by the median of the SD of all other states
results <- results %>% mutate(sd=ifelse(is.na(sd), median(results$sd, na.rm=TRUE), sd))

# We're going to use monte carlo to generate outcomes from simulated states
# Then we're going to use this to make probability statements
# For each state we apply the Bayesian approach
# We will assume we know nothing about prior history for each state so
# mean = 0 and a SD of 2%
mu <- 0
tau <- 0.02
results %>% mutate(sigma=sd/sqrt(n),
                  B = sigma^2/(sd^2+tau^2),
                  posterior_mean = B*mu+(1-B)*avg,
                  posterior_se = sqrt(1/(1/sigma^2 + 1/tau^2))) %>%
              arrange(abs(posterior_mean))

# Now we perform 10,000 monte carlo simulations
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

# Here is a histogram of the outcomes
data.frame(clinton_EV) %>% 
    ggplot(aes(clinton_EV)) +
    geom_histogram(binwidth = 1) +
    geom_vline(xintercept = 269)
# We know that a similar result was obtained by the Princeton Election Consortium
# and that this result is way off.  So what happened?

# Note that the model we just showed ignored the general bias.  The bias
# was not as big in 2016 but was still around 1-2%

# Because the election was close in several big states and the large number
# of polls made the standard error small, by ignoring the general bias the
# pollsters were over-confident. Fivethirtyeight models the data in a more
# sophisticated way and reported closer results

# We're going to assume larger state bias and will set sigma b at 0.03
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/n + bias_sd^2),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

# Here is a histogram of the outcomes
data.frame(clinton_EV) %>% 
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

# Fivethirtyeight includes many other features that we do not including
# model variability with distributions that have higher probabilities for
# extreme events compared to what the Normal distributions give us.
# They ended up giving a prediction probability of 71%


############################
# FORECASTING AN ELECTION #
###########################

# Forecasters like to make predictions weeks ahead of the election and
# update the predictions with each new poll. An important question that
# forecasters must ask is, how informative are polls taken several weeks
# before an election?
# Here we study variability across time and to make sure that the variability
# we observe is not due to the pollster effect, we will use just one poll
one_pollster <- polls_us_election_2016 %>%
                  filter(pollster == "Ipsos" & state == "U.S.") %>%
                  mutate(spread = (rawpoll_clinton - rawpoll_trump)/100)

# Since there's no pollster effect perhaps the theoretical standard error
# will match the empirical standard error.  In fact we see that the
# empirical SE is slightly higher:
se <- one_pollster %>% summarize(empirical = sd(spread),
      theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# Furthermore the distribution of the data does not look normal as the 
# theory would predict
one_pollster %>% ggplot(aes(spread)) +
        geom_histogram(binwidth=0.01, color="black")

# This variation makes a strong case that the extra variability is coming
# from variation over time not accounted for by the assumption that
# p is fixed across time
# Part conferences, etc.. give parties peaks and troughs

# With this code we see the trend across time for several pollsters
polls_us_election_2016 %>%
    filter(state == "U.S." & enddate >= "2016-07-01") %>%
    group_by(pollster) %>%
    filter(n()>=10) %>%
    ungroup() %>%
    mutate(spread=(rawpoll_clinton-rawpoll_trump)/100) %>%
    ggplot(aes(enddate, spread)) +
    geom_smooth(method="loess", span=0.1) +
    geom_point(aes(color=pollster), show.legend = FALSE, alpha=0.6)

# This implies that we need to introduce a bias term for time
# Y_ijt = d + b + h_j + b_t + epsilon_ijt

# The SD of b_t will depend on time as the closer we get to election day
# the smaller the variation will become

# Pollsters also try to estimate trends f(t)
# The blues lines in the plot above are estimates of F(t)
# The model now becomes
# Y_ijt = d + b + h_j + b_t + f(t) + epsilon_ijt
# This f(t) is not for the difference but for each candidate
# Updated plot to show this:
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton=rawpoll_clinton, Trump=rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate=factor(candidate, levels=c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n()>=10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color=candidate)) +
  geom_point(show.legend = FALSE, alpha=0.4) +
  geom_smooth(method="loess", span=0.15) +
  scale_y_continuous(limits=c(30, 50))


# EXERCISES
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")
# Create a table called polls that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
# Create an object called cis that has the columns indicated in the instructions
cis<- polls %>% mutate(X_hat = (spread+1)/2, se = 2*sqrt(X_hat * (1-X_hat)/samplesize), lower = spread - qnorm(0.975)*se, upper = spread + qnorm(0.975)*se)%>% select(state, startdate, enddate, pollster, grade, spread, lower, upper)


# Add the actual results to the cis data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
# Create an object called p_hits that summarizes the proportion of confidence intervals that contain the actual value.
# Print this object to the console.
p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% summarize(proportion_hits = mean(hit))
p_hits


# The cis data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
# Create an object called p_hits that summarizes the proportion of hits for each state that has more than 5 polls.
p_hits <- ci_data %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) %>% mutate(proportion_hits = mean(hit)) %>% group_by(state) %>% filter(n() > 5) %>% summarize(proportion_hits=mean(hit), n=n()) %>% arrange(desc(proportion_hits))


# The p_hits data have already been loaded for you. Use the head function to examine it.
head(p_hits)
# Make a barplot of the proportion of hits for each state
p_hits %>% arrange(proportion_hits) %>%
  ggplot(aes(state, proportion_hits)) +
  geom_bar(stat="identity") +
  coord_flip()


# The cis data have already been loaded. Examine it using the head function.
head(cis)
# Create an object called errors that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))
# Examine the last 6 rows of errors
tail(errors, 6)


# Create an object called errors that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))
# Create an object called p_hits that summarizes the proportion of hits for each state that has 5 or more polls
p_hits <- errors %>% group_by(state) %>% filter(n()>=5) %>% summarize(proportion_hits = mean(hit), n = n()) %>% arrange(proportion_hits)
# Make a barplot of the proportion of hits for each state
p_hits %>% ggplot(aes(state, proportion_hits)) +
  geom_bar(stat="identity") +
  coord_flip()


# The errors data have already been loaded. Examine them using the head function.
head(errors)
# Generate a histogram of the error
hist(errors$error)
# Calculate the median of the errors. Print this value to the console.
median(errors$error)


# The errors data have already been loaded. Examine them using the head function.
head(errors)
# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c("A+", "A", "A-", "B+")) %>%
  arrange(state, error) %>%
  ggplot(aes(state, error)) +
  geom_boxplot() +
  geom_point()


# The errors data have already been loaded. Examine them using the head function.
head(errors)
# Create a boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher
errors %>% filter(grade %in% c("A+", "A", "A-", "B+")) %>%
  group_by(state) %>%
  filter(n()>=5) %>%
  ungroup() %>%
  arrange(state, error) %>%
  ggplot(aes(state, error)) +
  geom_boxplot() +
  geom_point()



#######################
# THE T DISTRIBUTION #
######################

# Previously we made use of the CLT with sample sizes as small as 15
# Because we're also estimating a second parameter sigma further variability
# is introduced into our confidence interval this results in a interval
# that is over-confident.  For very large sample sizes this variability is
# negligible.

# In general for sample sizes less than 30 we need to be cautious about
# using the CLT

# However if the population is Normal then we actually have a mathematical
# theory to tell us how much bigger we need to make the intervals to account
# for the estimation of sigma.
# Using this theory we can construct confidence intervals for any urn but
# only if the data is known to follow a Normal distribution
# So for the 0, 1 data of the urn models we previously used,
# THIS DEFINITELY DOES NOT APPLY

# The statistic on which this theory is based was seen earlier, we call it
# Z, the CLT tells us that Z is approximately Normally distributed

# Z = (Xbar - d) / (sigma/sqrt(N))
# with mean 0 SD 1

# In practice we don't know sigma so we use s
# Z = (Xbar - d) / (s/sqrt(N))
# The s as variability is estimated from data

# Z follows a t-distribution with N-1 degrees of freedom
# The degrees of freedom is a parameter that controls the variability
# via what are known as fatter tails

# For pollster data if we are ready to assume that the pollster effect data
# is normally distributed then we can use this theory

# All we do is use a t-distribution instead of a normal distribution
# for estimating d.
# We simply change the 1.96 to a quantile coming from the t-distribution
z <- qt(0.975, nrow(one_poll_per_pollster)-1)
one_poll_per_pollster %>%
    summarize(avg=mean(spread), moe=z*sd(spread)/sqrt(length(spread))) %>%
    mutate(start=avg-moe, end=avg+moe)

# The interval is bigger because the quantile from the t-distribution
# is larger than the quantile from the Normal distribution
qt(0.975, 14)     # n = 15 - 1
qnorm(0.975)
# This is one of the theories that Fivethirtyeight use to produce more
# accurate polling estimates

# EXERCISES
# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'. 
1 - pt(2, 3) + pt(-2, 3)


# Generate a vector df that contains a sequence of numbers from 3 to 50
df <- 3:50
# Make a function called pt_func that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func <- function(df) {
  1-pt(2, df) + pt(-2, df)
}
# Generate a vector probs that uses the pt_func function to calculate the probabilities
probs <- sapply(df, FUN = pt_func)
# Plot df on the x-axis and probs on the y-axis
plot(df, probs)


# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate x, a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height mu, the sample size N, 
# and the number of times the simulation should run B
mu <- mean(x)
N <- 15
B <- 10000

# Use the set.seed function to make sure your answer matches the expected
# result after random sampling
set.seed(1)
# Generate a logical vector res that contains the results of the
# simulations
res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qnorm(0.975)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})
# Calculate the proportion of times the simulation produced values
# within the 95% confidence interval. Print this value to the console.
mean(res)


# The vector of filtered heights x has already been loaded for you.
# Calculate the mean.
mu <- mean(x)
# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000
# Generate a logical vector res that contains the results of the
# simulations using the t-distribution
res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qt(0.975, N-1)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})
# Calculate the proportion of times the simulation produced values
# within the 95% confidence interval. Print this value to the console.
mean(res)



######################
# ASSOCIATION TESTS #
#####################

# So far we have not discussed statistical tests for a variety of datatypes
# specifically inference for binary, categorical or ordinal data

# A 2014 study in Netherlands concluding they suffered gender bias
# in awarding research funding
data("research_funding_rates")
research_funding_rates

# Calculate totals for successful and unsuccessful applicants
totals <- research_funding_rates %>% select(-discipline) %>%
      summarize_all(funs(sum)) %>%
      summarize(yes_men = awards_men,
                no_men = applications_men - awards_men,
                yes_women = awards_women,
                no_women = applications_women - awards_women)

totals %>% summarize(percent_men = yes_men/(yes_men+no_men),
                    percent_women = yes_women/(yes_women+no_women))
# But could this be just random chance?

# We learn the fisher test through the lady drinking tea example to
# quantify this question
# The story goes that a lady, Muriel Bristol, could tell whether the
# milk was added to tea before or after it was poured

# R.A. Fisher gave her a test, 4 cups of tea with the milk poured in twice
# before the tea, and twice after the tea.

# The null hypothesis is that she was guessing.  He concluded that the 
# picks were random and independent.
# The question we are asking is:
# If she gets 3 or more correct, what are the chances that she is actually
# just guessing

# Just as before we can think of this as an urn where there are
# 4 blue beads - correct answers, and
# 4 red beads - incorrect anwers, 8 beads in total
# There are 4 milk before tea, and 4 milk after tea

# We can use combinatorics to figure out each probility
# Probability of picking 3 right (out of 4)
# (4)(4)   (8)
# (3)(1) / (4)    =  16/70

# Probability of picking 4 right (out of 4)
# (4)(4)   (8)
# (4)(0) / (4)    =  1/70

# Probably of getting 3 or more under the null hypothesis is approx 0.24
# This is the p-value
# The procedure that produces the p-value is called The Fisher Exact Test
# and uses the hypergeomtric distribution
# FYI - In the story, she did it every time (1/70 chance)

# Create the data
tab <- matrix(c(3, 1, 1, 3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab
# These are known as two by two tables.  They show, for each of the 4
# combinations one can get with a pair of binary variables, the observed
# counts for each of these pairs

# The function fisher.test performs the inference for us
fisher.test(tab, alternative = "greater")
# We see that the p-value is what we previously calculated

# In the tea drinking example the number of teas was a fixed number
# 4 before milk and 4 after milk.  The lady knew this.  As this is the 
# case then the sum of the rows and columns of the 2x2 table is also fixed
# This defines a constraint on the way that we can fill the table and is
# what allows us to use the hypergeometric distribution.

# In general this will not be the case and we have another distribution,
# the chi-squared distribution which we will now describe.
# Imagine we have 2823 individuals, some are men, some are women,
# some get funded, some don't.  There you have 2 binary variables

# We can compute the overall funding rate using the following code,
# This follows on from the number calculated earlier
funding_rate <- totals %>%
      summarize(percent_total = 
                  (yes_men + yes_women) / 
                    (yes_men+no_men+yes_women+no_women)) %>%
      .$percent_total
funding_rate    # 16.54$

# The question we ask if whether we will see a difference between men and
# women as big as the one we see if the funding was assigned at random using
# the funding_rate.  The chi-squared test answers this question

# Step 1 is to create a two by two table using this code
two_by_two <- tibble(awarded=c("no", "yes"),
                     men=c(totals$no_men, totals$yes_men),
                     women=c(totals$no_women, totals$yes_women))
two_by_two      

# Step 2 is to compare this table of observed values to the values of a 
# two by two table you expect to see from the funding_rate value
tibble(awarded=c("no", "yes"),
       men=c(totals$no_men, totals$yes_men) *
         c(1-funding_rate, funding_rate),
       women=c(totals$no_women, totals$yes_women) *
         c(1-funding_rate, funding_rate))

# We see that more men and women were awarded funding than you'd expect
# However under the null hypothesis, the observation is a random variable
# The chi-squared test tells us how likely it is to see a deviation like
# this or larger.
# The test uses an asymptotic result, similar to the CLT, related to the
# sums of independent binary outcomes
two_by_two %>% select(-awarded) %>% chisq.test    # p-value = 0.05091

# This tells us that the chance of seeing the observed funding under the 
# null hypothesis of random funding is 0.051, or 5%

# An informative summary statistic associated with two by two tables
# is the odds ratio.  Define two ratios:
# X <- 1 for man, 0 otherwise
# Y <- 1 if funded, 0 otherwise

# The odds of getting funded if you're a man is defined as follows:
# Pr(Y=1|X=1) / Pr(Y=0/X=1)
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) / 
              (two_by_two$men[1] / sum(two_by_two$men))

# The odds of getting funded if you're a woman is defined as follows:
# Pr(Y=1|X=0) / Pr(Y=0/X=0)
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) / 
  (two_by_two$women[1] / sum(two_by_two$women))

# The odds ratio is the ratio of these odds.  I.e. how many time larger
# for men than for women
odds_men / odds_women #   In this case it is about 1.23

# A NOTE OF CAUTION RE: P-VALUES
# Reporting only p-values is not an appropriate way to report the results
# of data analysis

# Some studies over-emphasise the p-values with large sample sizes and
# report impressively small p-values yet when one looks closely at the
# results we realise that the odds ratios are quite modest, barely bigger
# than one.  In this case, the difference may not be practically or
# scientifically significant

# The relationship between the p-value and the odds ratio is not one to one
# It depends on the sample size
two_by_two %>% select(-awarded) %>%
                  mutate(men=men*10, women=women*10) %>%
                  chisq.test    # p-value = 2.625e-10
# The odds ratio remains the same but the p-value becomes far smaller

# It is more appropriate to calculate confidence invervals however
# calculating confidence intervals for the odds ratio is not mathematically
# straight forward.  Unlike other statistics, the odd ratio is a ratio of
# ratios, therefore there is no way to use the CLT

# One approach is to use generalise linear models but these are far too
# advanced for this course but you can read about them in this book:
# McCullough & Nelder, 1989


# EXERCISES

# The errors data have already been loaded. Examine them using the head function.
head(errors)
# Generate an object called totals that contains the numbers of good and bad predictions for polls rated A- and C-
totals <- errors %>%
  filter(grade %in% c("A-", "C-")) %>%
  group_by(grade,hit) %>%
  summarize(num = n()) %>%
  spread(grade, num)
# Print the proportion of hits for grade A- polls to the console
totals[[2,3]]/sum(totals[[3]])
# Print the proportion of hits for grade C- polls to the console
totals[[2,2]]/sum(totals[[2]])


# The totals data have already been loaded. Examine them using the head function.
head(totals)
# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test <- totals %>% select(-hit) %>% chisq.test
# Print the p-value of the chi-squared test to the console
chisq_test$p.value


# The totals data have already been loaded. Examine them using the head function.
head(totals)
# Generate a variable called odds_C that contains the odds of getting the prediction right for grade C- polls
odds_C <- (totals[[2,2]] / sum(totals[[2]])) / 
  (totals[[1,2]] / sum(totals[[2]]))
# Generate a variable called odds_A that contains the odds of getting the prediction right for grade A- polls
odds_A <- (totals[[2,3]] / sum(totals[[3]])) / 
  (totals[[1,3]] / sum(totals[[3]]))
# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A / odds_C


#####################################################
# COMPREHENSIVE ASSESSMENT - BREXIT - YAAYYYY  :/  #
####################################################

# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)
head(brexit_polls)

# Define actual remain proportion and spread
p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

# Q1
N <- 1500
RemainVoters <- N*p           # Number of remain voters
se_remain <- sqrt(p*(1-p)/N)  # SE(X)
se_remain*1500                # SE as number of voters
E_X <- p                      # Expected value of X
d                             # expected value of d
SE(d) <- 2 * se_remain        # Standard error of the spread

# Q2
brexit_polls <- brexit_polls %>% mutate(x_hat = (spread+1)/2)
m_d <- mean(brexit_polls$spread)  # 1 avg of spreads
sd_d <- sd(brexit_polls$spread)   # 2 sd of observed spreads
ed <- (mean_spreads+1)/2          # 3 average of x_hat
sd_ed <- sd(brexit_polls$x_hat)   # 4 sd of s_hat

# Q3
X_hat <- brexit_polls$remain[1]
se_hat <- sqrt((X_hat*(1-X_hat))/brexit_polls$samplesize[1])
# Use qnorm to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called ci.
z <- qnorm(0.975)
ci <- c(X_hat - z*se_hat, X_hat + z*se_hat)


##############
# SECTION 2 #
#############

# suggested libraries
library(tidyverse)
# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>% mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

# Q4
# Filter for polls ending in June
june_polls <- brexit_polls %>%
                filter(enddate >= '2016-06-01') %>%
                mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
                    se_d = 2*se_x_hat,
                    lower = spread - qnorm(0.975)*se_d,
                    upper = spread + qnorm(0.975)*se_d,
                    hit = ifelse(-0.038>=lower & -0.038<=upper, 1, 0))

# Q5
june_polls %>% group_by(pollster) %>%
    summarize(polls=n(), prop_hits = mean(hit)) %>%
    arrange(desc(prop_hits))

# Q6
june_polls %>% ggplot(aes(poll_type, spread)) +
    geom_boxplot() +
    geom_point()

# Q7
combined_by_type <- june_polls %>%
                      group_by(poll_type) %>%
                      summarize(N = sum(samplesize),
                      spread = sum(spread*samplesize)/N,
                      p_hat = (spread + 1)/2)

combined_by_type <- combined_by_type %>%
                    mutate(SE_phat = sqrt(p_hat*(1-p_hat)/N),
                      SE_spread = 2 * SE_phat)

combined_by_type <- combined_by_type %>%
          mutate(lower = spread-qnorm(0.975)*SE_spread,
                  upper = spread+qnorm(0.975)*SE_spread)


##############
# SECTION 2 #
#############

# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

# Q9
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)
# Prepare two by two - by the way this needs to be transposed
brexit_chi <- brexit_hit %>% group_by(poll_type) %>%
              summarize(Right = sum(hit),Wrong = n()-sum(hit))
# Calculate chi-squared results
brexit_chi %>% select(-poll_type) %>% chisq.test

# Q10
# Calculate odds manually due to table being wrong way around        
odds_o <- (48/85) / (37/85)
odds_t <- (10/42) / (32/42)
odds_o/odds_t  

# Q11
brexit_polls %>%
  ggplot(aes(enddate, spread, color = poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() +
  geom_hline(aes(yintercept = -.038))

# Q12
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long %>%
  ggplot(aes(enddate, proportion, color = vote)) +
  geom_smooth(method = "loess", span = 0.3) +
  geom_point()