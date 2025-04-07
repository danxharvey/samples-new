# Simple notation
# Probability of x occurring is Pr(x)
# if x was greater than 5 about 40% of the time
# Pr(x>5) = 0.4


#############################
# RANDOM NUMBER GENERATORS #
#############################

# Create an urn containing beads using the rep function
# set.seed(1) ensures repeatability of random results
beads <- rep(c("red", "blue"), times = c(2, 3)) # Vectors must match
# Pick one at random
sample(beads, 1)


############################
# MONTE CARLO SIMULATIONS #
###########################

# Facilitating repeating the above over and over again
# Repeat above example 10,000 times
b <- 10000
events <- replicate(b, sample(beads, 1))  # replicate is not rep above
# View results
tab <- table(events)
# View proportions
prop.table(tab)   # Results give good approximation of 0.6 and 0.4


# BACK TO THE URN
sample(beads, 5)  # This is sampled without replacement
sample(beads, 6)  # Error, not enough beads

# If we use the replace argument we can perform monte carlo
events <- sample(beads, b, replace=TRUE)
prop.table(table(events))


#########################################
# INDEPENDENT & NON-INDEPENDENT EVENTS #
########################################

# Events are independent if they do not affect the outcome of subsequent
# events.  Coin tosses are perfect examples of this
# Card games are not-independent - draw a card leaves 51 cards, not 52

# Example picking a bead from the urn is similar
x <- sample(beads, 5)
x[1]    # /5
x[2:5]  # The probability is now /4 not /5

# When events are not independent it is useful to use conditional probability
# Card games are such
# Probability that the second card is a king, given the first is a king
# Pr(card2 = king | card1 = king) = 3/51      # | = "given"
# 4 kings in 52 cards becomes 3 kings in 51 cards

# If 2 events are independent then
# Pr(A|B) = Pr(A)

# To calculate conditional probability we use multiplication
# Pr(A|B) = Pr(A) * Pr(B|A)     # Prob A * Prob B given A happened

# Using blackjack probability of drawing 21 (ace + 10/face)
# 4/52 * 16/51 ~ 0.02

# We can use induction to expand this out for 3 events
# Pr(A and B and C) = Pr(A)Pr(B|A)Pr(C| A and B)    # Non-independent
# Pr(A and B and C) = Pr(A)Pr(B)Pr(C)               # Independent
# You must be totally sure you have independence!

# Real life example (loosely based)
# A court case happened where a lawyer did not understand this
# A suspect was described as having a beard and moustache
# The prosecution brought in an expert to argue that because
# about 1/10 men have beards and 1/5 men have moustaches
# The odds of the suspect having both was 2% and quite unlikely
# He incorrectly assumed independence

# The conditional probability of a man having a moustache conditional
# on them having a beard is quite high, about 95%
# So the correct results was 1/10 * 95% = 9.5% placing reasonable doubt
# on the expert


##################################
# COMBINATIONS AND PERMUTATIONS #
#################################

# What are the chances of getting a flush in poker?
# Discrete probability teaches us how to do this mathematically

# Introducing expand.grid() and paste()
number <- "Three"
suit <- "Hearts"
paste(number, suit)
# It also works on pairs of vectors performing the
# operation element-wise
paste(letters[1:5], as.character(1:5))

# expand.grid gives us all the combinations of two lists
# cartesian product in SQL
expand.grid(pants=c("blue", "black"), shirt=c("white", "gray", "plaid"))

# Generating a deck of cards
suits <- c("Diamonds", "Hearts", "Clubs", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven"
             , "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number, deck$suit)

# Check probability of getting a king
kings <- paste("King", suits)
mean(deck %in% kings)

# Probability of next card being a king?
# We introduce permutations() and combinations()

# PERMUTATIONS
# Permutations() computes for size n, all the ways we can select r items
# For example, here are all the ways that we can select 2 numbers
# from a list of 1, 2, 3, 4, 5
install.packages("gtools")
library(gtools)
permutations(5, 2)    # size n, items r
# The order matters -> 1,3 is different to 3,1

# This works with vectors too, imagine picking 5 random phone numbers
# 10 numbers, pick 7, pool is from 0 through 9
all_phone_numbers <- permutations(10, 7, v=0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

# So for dealing cards
hands <- permutations(52, 2, v = deck)  # pull 2 from deck (size 52)
first_card <- hands[, 1]
second_card <- hands[, 2]

# To get 2 kings
sum(first_card %in% kings)  # First cad is a king = 1/13
sum(first_card %in% kings & second_card %in% kings) / +
  sum(first_card %in% kings)  # 0.0588... = 3/51
# This code is eqivalent to using mean to calculate proportions
mean(first_card %in% kings & second_card %in% kings) / +
  mean(first_card %in% kings)  # 0.0588... = 3/51
# This is equivalent to the written probability
# Pr(B|A) = Pr(A and B) / Pr(A)


# COMBINATIONS
# We use combinations when the order does not matter
combinations(3, 2)  # Less entries (duplicates discarded as no order)
permutations(3, 2)  # More entries (order matters)
# Think of combinations as SELECT DISTINCT

# Calcuate the probability of a 21 in blackjack
# Ace & Face is the same as Face & Ace
# We define a new vector
aces <- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number=facecard, suit=suits)
facecard <- paste(facecard$number, facecard$suit)
hands <- combinations(52, 2, v=deck)

# How often to we get an ace and facecard?
# As order does not matter it is a simple count and average
mean(hands[, 1] %in% aces & hands[,2] %in% facecard)

# We can also use a monte carlo simulation
hand <- sample(deck, 2)
hand

# Build this up for b = 10,000
b <- 10000
results <- replicate(b, {     
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) |
    (hand[1] %in% facecard & hand[2] %in% aces)
})  
mean(results)


#########################
# THE BIRTHDAY PROBLEM #
########################

# What are the chances of 2 people having the same birthday
# in a class of 50

# A sample of 50 random birthdays can be returned using sample()
n <- 50
bdays <- sample(1:365, n, replace=TRUE)   # Bdays = days of year

# How do we know if the same birthday appears?
# duplicated() returns TRUE the second time a value appears
duplicated(bdays)   # Returns vector of TRUE/FALSE
any(duplicated(bdays))  # Returns boolean string

# To estimate the probability we're going to run monte carlo
# Using replicate() for 10,000 runs
b <- 10000
results <- replicate(b, {
  bdays <- sample(1:365, n, replace=TRUE)
  any(duplicated(bdays))
})
mean(results)   # The answer is surprisingly high, about 97%-98%


#################
# USING SAPPLY #
################

# Using the above birthday example, what size is needed to be
# greater than 50%?  greater than 75%?
# We can quickly create a lookup table and a function

compute_prob <- function(n, b=10000){
  same_day <- replicate(b, {
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

# Let's apply this function for groups up to 60 in size
# Define a vector
n <- seq(1:60)    # n <- 1:60 most efficient

# Background - R performs arithmetic on vectors extremely efficiently
x <- 1:10
y <- 1:10
sqrt(x)
x*y

# Not all functions work this way. Ours will not work this way.
# It expects an argument
compute_prob(n)
#Gives zero, no good

# To do what we want, we use sapply()
# Permits element-wise operations on any function
sapply(x, sqrt)     # Simple example from above

# For our function
prob <- sapply(n, compute_prob)
# Now we can make a plot to see the probabilty by size of group
plot(n, prob)
# This is great, uses monte carlo and a function to approximate

# We can get the exact answer using maths, and it's faster
# We calcualte probability of it NOT happening instead of it happening
# Pr(person 1 has unique birthday = 1)
# Pr(person 2 unique | person 1 unique) = 364/365
# Pr(person 3 unique | person 1&2 unique) = 363/365
# and so on for 365-n+1 person
# Let's write a function for the exact prob

exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365
  1 - prod(prob_unique)
}
# Now calculate group prob of duplicate birthday using sapply
eprob <- sapply(n, exact_prob)

# If we plot it and compare to the monte carlo
plot(n, prob)
lines(n, eprob, color="red")


# HOW MANY MONTE CARLO EXPERIMENTS ARE ENOUGH?
# This requires advanced statistical training and is very difficult
# to answer.  For our simple examples 10,000 was more than enough
# For other problems, it may be too much or too little
# However we can check the stability of the estimate
# Take our birthday example using 22 people
b <- 10^seq(1, 5, len=100)
compute_prob <- function(b, n=22){
  same_day <- replicate(b, {
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
# This computes the simulation for n=22 but the monte carlo varies
# We can calculate the probably for each monte carlo variation
# and plot the results
prob <- sapply(b, compute_prob)
plot(log10(b), prob, type="l")
# The graph is awesome and eventually stabilises
# When it stabilises we can say maybe this is a good number
# to use as a number of monte carlo experiments


##################
# ADDITION RULE #
#################

# Earlier we saw how to get a natural 21 in blackjack
# We can try this again using the addition rule
# Pr(A or B) = Pr(A) + Pr(B) - Pr(A and B)
# The result is the same


###########################
# THE MONTY HALL PROBLEM #
##########################

# Classic switcheroo problem
# 3 doors:  1 prize, 2 goats
# Are you better off to stick or twist after first choice?

# Initially you have 1 in 3 chance of winning a prize
# If you subsequently stick your chance of winning is 2 in 3
# Why?
# Using Monte Carlo:

# Code for sticking to 1st door
b <- 10000
stick <- replicate(b, {
  doors <- as.character(1:3)                  # Doors 1:3
  prize <- sample(c("car", "goat", "goat"))   # Random fill
  prize_door <- doors[prize == "car"]         # Assign prize
  my_pick <- sample(doors, 1)                 # Pick 1 door
  # You are then shown not your door, and not the prize
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  stick == prize_door
})
mean(stick)     # Comes out to be a third

# So, if you haven't won you know that 1 goat is eliminated
b <- 10000
switch <- replicate(b, {
  doors <- as.character(1:3)                  # Doors 1:3
  prize <- sample(c("car", "goat", "goat"))   # Random fill
  prize_door <- doors[prize == "car"]         # Assign prize
  my_pick <- sample(doors, 1)                 # Pick 1 door
  # You are then shown not your door, and not the prize
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  switch <- doors[!doors %in% c(my_pick, show)]
  switch == prize_door
})
mean(switch)     # Comes out to be a third


###########################
# CONTINUOUS PROBABILITY #
##########################

# Using heights as an example it is more useful to measure intervals
# than assigning specific probabilities to individual heights
# This is called the Cumulative Distribution Function
# The empirical CDF or eCDF is a basic summary of lists of numeric values
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% .$height

# Now we can define the eCDF as:
F <- function(a) mean(x<=a) # Count x <= a and divide by n

# These are just lists, let's introduce some probability
# What are the chances of picking a student > 70.5 inches of height
# Every student has an equal chance of being picked
# So for this example
1 - F(70)   # 1 - probability x <= 70 as per the function

# We can use this to calculate any probability
# Chances of a student being between 60 and 70 inches of height
F(70) - F(60)   # F(A) - F(B) where F is probability

# The CDF defines a probability of picking a height at random
# from our vector of heights x


###################################
# CDF OF THE NORMAL DISTRIBUTION #
##################################

# In R we can use the function pnorm() to achieve this
# F(a) = pnorm(a, avg, s)   # avg = average, s = standard deviation

# If we use this then we don't need the entire distribution
# to calculate, just the mean and sd. Prob of being taller than 70.5
1 - pnorm(70.5, mean(x), sd(x))

# Normal distribution is defined for continuous variables
# It is not defined for discrete variables
# As data scientists working with data, most of it is discrete
# We could consider our adult data categorical meaning that each
# reported height is unique.  The probability distribution would
# then be defined by proportion of student recording those exact heights
# Here is what a plot of that would look like
plot(prop.table(table(x)), xlab="a = Height in inches", ylab="Pr(x=a)")

# In this the data is not useful.. someone converts 177cm to inches
# records this as 69.68.... whereas most round up to 70 inches
# This gives the probably of being 70 inches x times greater
# giving false conclusions.  It makes more sense to define intervals
# It does not make sense to ask Pr(70) but does make sense to ask
# Pr(69.99 < x < 70.01).  It useful if the approximation includes
# an integer number.  Probability between 69.5 and 70.5 would catch
# all students who rounded their height to 70 inches

# Using the actual data
mean(x<=68.5) - mean(x<=67.5)
mean(x<=69.5) - mean(x<=68.5)
mean(x<=70.5) - mean(x<=69.5)
# Now use the approximation
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))
# The results are similar so are quite useful

# However, using data that does not include an integer
mean(x<=70.9) - mean(x<=70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))
# The results are not accurate
# This situation is called discretisation

# Although heights are continuous in nature the reported values
# tend to be more discrete, due to rounding
# As long as we are aware of this reality and know how to deal with
# it then the normal approximation can be a very good tool.


#####################
# CATEGORICAL DATA #
####################

# Example is rolling a die. Categories of 1 to 6
# F4 = Pr(x<=4) = Pr(x=1) + Pr(x=2) + Pr(x=3) + Pr(x=4)
# F4 = 4/6

# In contrast for continuous distributions the probability of a single
# distribution is not defined.  However there is a theoretical
# definition that has a similar interpretation and is to do with the 
# Probability Density (integral calculus)
# F(a) = Pr(x<=a)= Integral(-inf to a) of f(x)dx
# This is the area under the curve giving Pr(x<=a) (normal dist)
# To get the probability density function in R, you use dnorm()
# This is useful for fitting data to models where pre-defined
# functions are not available



####################################################################
# RUN MONTE CARLO DISTRIBUTIONS ON NORMALLY DISTRIBUTED VARIABLES #
###################################################################

# R provides a function rnorm() taking:
# size
# average, default to 0
# standard deviation, default to 1

# Example of generating data that looks like our reported heights
x <- heights %>% filter(sex=="Male") %>% .$height
n <- length(n)
avg <- mean(x)
s <- sd(x)
sim_heights <- rnorm(n, avg, s)
ds_theme_set()
data.frame(sim_heights=sim_heights) %>%
  ggplot(aes(sim_heights)) +
  geom_histogram(color="black", binwidth = 2)
# Looks normal because r generates normal dist looking data
# rnorm() allows us to generate data that mimics naturally occurring
# events

# Generate a distribution of 800 samples and run a 10,000 monte carlo
# simulation to see the probability of someone being over 7ft tall

# Keep max height from simulated data
b <- 10000
tallest <- replicate(b, {
  simulated_data <-rnorm(800, avg, s)
  max(simulated_data)
})
# Find out what percentage had the tallest over 7ft
mean(tallest >= 84)   # 7ft in inches


###################################
# OTHER CONTINUOUS DISTRIBUTIONS #
##################################

# Other theoretical distributions include
# student-t, chi-squared, exponential, gamma and the beta distribution
# R has functions for all of these allowing to perform monte carlo

# R has a convenient notation for allowing us to remember the functions
# d for density
# q for quantile
# p for probability density function
# r for random

# We have seen for example the normal distribution functions
# dnorm, qnorm, pnorm and rnorm
x <- seq(-4, 4, length.out = 100)
data.frame(x, f=dnorm(x)) %>% ggplot(aes(x, f)) + geom_line()

# For student-t distribution for example we can use
# dt, qt, pt, rt


#####################
# RANDOM VARIABLES #
####################

# We've already generated random variables with our beads in an urn
beads <- rep(c("blue", "red"), times=c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)   # blue = 1, red = 0

# Being able to quantify the uncertainty introduced by randomness
# is one of the most important jobs of a data scientist
# Statistical inference offers a framework for doing this
# as well as several tools

# In epidemiological studies we assume that the subjects in our
# study are a random sample from the population of interest
# taken from a larger population in general

# Casino games are a great training tool for understanding
# such models

# Consider if a small casino hires you to determine if a roulette
# wheel would be a profitable venture
# Assume 1000 will play and bet on red or black
# Wheel has 18 black, 18 red and 2 green
color <- rep(c("black", "red", "green"), c(18, 18, 2))
# There are 1000 draws, or plays at the game, each is independent
n <- 1000
# If red the player gets $1, if not the casino gets $1
x <- sample(ifelse((color=="red"), -1, 1), n, replace=TRUE)
x[1:10]

# However, because we KNOW the number of reds, blacks and greens
# we can just define the probabilities.
# We call this a SAMPLING MODEL
x <- sample(c(-1, 1), n, replace=TRUE, prob=c(9/19, 10/19))
s <- sum(x)
# s changes every time because it is a random variable
# The probability distribution of this is important because it tells
# us the probability of the observed value falling in any given interval

# To answer the probability of us losing money (as the casino)
# we're asking what is the probability that s < 0
# We can define a CDF to answer any question on profit/loss
# F(a) = Pr(s<=a)

# We can estimate the distribution of s with a monte carlo
# We want to estimate for 1000 people playing and do it 10,000 times
n <- 1000
b <- 10000

s <- replicate(b, {
  x <- sample(c(-1, 1), n, replace=TRUE, prob=c(9/19, 10/19))
  sum(x)
})
# How often was s <= 0
mean(s<=0)

# A histogram would show the approximation to be normal
ds_theme_set()
data.frame(s) %>%
  ggplot(aes(s)) +
  geom_histogram(color="black", binwidth = 2)
# A Q-Q plot would show that this is a near match


# Adding a normal density with the mean and sd
S <- seq(min(s), max(s), length=100)
normal_density <- data.frame(s=S, f=dnorm(S, mean(s), sd(s)))
data.frame(s=s) %>% ggplot(aes(s, ..density..)) +
  geom_histogram(color="black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data=normal_density, mapping=aes(S, f), color="blue")
# This matches very well
# The average and standard deviation here have special names
# They are referred to as the expected value and standard error
# of the random variable s

# We can show that from our example earlier statistical theory
# gives us a way to derive the distribution of a random variable
# (s+n)/2 follows a binomial distribution and therefore we do not
# need to run monte carlo simulations to know the distribution of s

# Binomial distribution can be found on the internet or wikipedia
# https://en.wikipedia.org/wiki/Binomial_distribution

# We will now introduce an incredibly useful approximation that can
# be applied generally to sums of averages of draws from any urn
# The Central Limit Theorum

# IMPORTANT DISTINCTION TO REMEMBER
# DISTRIBUTIONS v PROBABILITY DISTRIBUTIONS

# A list of numbers 1 to x has a distribution and we can define
# F(a) as the probability that that proportion is <= a
# When this distribution is approximately normal we can define
# a mean and sd that are defined with the operation of the list

# A random variable x has a distribution function. To define this
# we do not need a list of numbers as it is a theoretical concept
# In this case to define F(a) we define it as a function that will
# answer the question being asked. There is no list of numbers

# However if x is defined by drawing from an urn then there is a list
# and the list of numbers inside the urn. In this case the 
# distribution of that list is the probability distribution of that
# list and the average and sd of that list are known as the
# expected value and standard errors of the random variable

# Monte carlo generates list and distributions and if large enough
# is a very good approximation of a distribution and the
# average and standard deviation of this list will approximate the
# expected value and standard error of the random variable


# NOTATION FOR RANDOM VARIABLES

# Capital letters are used for random variables
# Lower case letters are used for observed values
# X <= x    X is value of die roll, x is actual value we see



##############################
# THE CENTRAL LIMIT THEORUM #
#############################

# The CLT tells us that when the number of draws is large, the sum
# of the independent draws is approximately normal

# Average and standardard deviation when approximated are known as
# Expected value and standard error

# In statistics books the expected value of X, E of X = mu
# E(X) = mu (greek letter)

# Using our casino example, we know that we have 20 "wins" for the 
# casino and 18 for the punter.
# E(X) = (20-18)/38 = ~0.05       # $1 and -$1 respectively
# This is saying if we play over and over the casino wins
# 5 cents per game on average
# We can confirm this with monte carlo over 1 million runs
b <- 10^6
x <- sample(c(-1, 1), b, replace=TRUE, prob=c(9/19, 10/19))
mean(x)

# In general if we have 2 outcomes with p and 1-p
# the average is ap + b(1-p)

# FIRST USEFUL FACT
# Expected sum of draws = number of draws x average of numbers in urn

# The standard error (SE) gives us an idea of the size of the error
# associated with E(X) known as SE[X]

# If our draws are independent, as important assumption then SE[X]
# sqrt(number of draws) x sd of the numbers in the urn
# SE[X] = abs(b-a) * sqrt(p(1-p))
# in our casino example
# SE[X] = abs(1--1) * sqrt((9/19)/(10/19)) = 0.998614
#
# This makes sense to have E(X) = 0.05 and SE[X] ~ 1 as we either
# win $1 or lose $1, with a $1 profit  slightly favoured

# When 1000 people bet on red
n <- 1000
sqrt(n)*2*sqrt(90)/19 #~32  E(X) = 50, SE(X] = 32

# This still doesn't answer the question, how likely is the casino
# to lose money

# As we know the mean and standard error we can use the pnorm function
# to approximate the answer
pnorm(0, 52, 32)  # ~ 5%
# This agrees with the monte carlo simulation we ran earlier


#############################
# AVERAGES AND PROPORTIONS #
############################

# Useful mathematical results listed here

# The first, is that the expected value of a sum of random variables
# is the sum of the expected values of the individual random variables
# If they are all drawn from the same urn then they have the same value
# E(X) = n * mu


# The second, the expected value of random variables times a
# non-random constant is the expected value times that non-random
# constant
# E(aX) = a * E(X)


# The third, the square of the standard error of the sum of
# independent random variables is the sum of the square of the
# standard error of each random variable.
# SE[x1+x2+...+xn] = sqrt(SE[x1]^2+...+SE[xn]^2)
# NOTE
# Variance[X1] = SE[X1] squared


# The fourth, he standard error of random variables times a
# non-random constant is the standard error times a non-random
# constant
# SE[aX] = a * SE[X]


# The fifth and final, If x is a normally distributed random variable
# then if a and b are non-random constants, then a times X plus b
# is also a normally distributed random variable.
# i.e. If X is normally distributed then so is aX + b


# LAW OF LARGE NUMBERS (LAW OF AVERAGES)
# As the number of draws grows larger, the SE approaches zero
# until the number of draws is so large that the average of draws
# converges on the averages of the urn

# HOW LARGE IS LARGE IN THE CLT?
# The CLT works when n is large
# How large is large enough?

# In many circumstances 30 is enough, or 10, but these are not
# general rules

# When the probability of success is small then the number of draws
# will need to be large

# When the probability of success is very low, say number of lottery
# winners the CLT does not apply.  In these instances the Poisson
# distribution is more appropriate
# https://en.wikipedia.org/wiki/Poisson_distribution



#############################################
# THE BIG SHORT - INTEREST RATES EXPLAINED #
############################################

# The sampling models we have seen are also used by banks to
# determine interest rates

# Historically in any given year approximately 2% will default on loans

# Example: Bank Loan interest rates
# Gives out 1000 loans for 180,000 value each loan
# Defaults cost 200,000

# We can create a sampling model for this scenario
n <- 1000
loss <- -200000
p <- 0.02
defaults <- sample(c(0, 1), n, prob=c(1-p, p), replace=TRUE)
sum(defaults*loss)

# Create a monte carlo to get a distribution for this random variable
b <- 10000
losses <- replicate(b, {
  defaults <- sample(c(0, 1), n, prob=c(1-p, p), replace=TRUE)
  sum(defaults*loss)
})
mean(losses)  # approximately $4m

# Graphically
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth=0.6, col="black")


# We didn't really need the monte carlo because the CLT tells us that
# as it is the sum of independent draws the distribution is
# approximately normal with mean and sd given as:
m <- n*(p*loss + (1-p)*0)
s <- sqrt(n)*abs(loss)*sqrt(p*(1-p))

# In order to break even we need to add an amount x to each loan
# to guarantee that on average we break even
# lp + p(1-p) = 0
-loss*p + x*(1-p) # ~2% to break even

# Say the bank wants its chance of losing money to be 1%
# Pr(S<=0) = 0.01

# We're going to use a mathematical trick to just calculate x
# Add and subtract the same quantities to both sides of event S
# less than zero so that the probability does not change
# to end up with a standard normal random variable on the left
# which then permits us to write down an equation with only x as
# as unknown
# Pr(S <= 0)
# Pr(S-E[S] / SE[S]  <=  -E[S] / SE[S])
# The left term is now a standard normal random variable z
# Pr(z <=  -E[S]/SE[S])
# Now we add in our known formulae

# Pr(z <= -{lp+x(1-p)}n / (x-l)sqrt(np(1-p))) = 0.01
# The term on the left is now a standard normal random variable
# with MEAN = 0 and SD = 1
# This means that the term on the right must be equal to qnorm(0.01)
# which is -2.326348

# Pr(Z<=z) = 0.01

# This now leaves x as unknown so we have
# -{lp+x(1-p)}N / (x-l)sqrt(Np(1-p)) = z

# Solving for x
# x = -l * (n*p - z * sqrt(n*p*(1-p))/(N*(1-p)*z*sqrt*n*p*(1-p)))
l <- loss
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p))) / ( n*(1-p) + z*sqrt(n*p*(1-p)))
x   # ~ 3% ( / 180,000)
exp_profit <- loss*p + x*(1-p)  # 2124.198 profit per loan
# This is an expected profit of $2m

# We can check this with monte carlo
b <- 10000
profit <- replicate(b, {
  draws <- sample(c(x, loss), n, prob=c(1-p, p), replace=TRUE)
  sum(draws)
})
mean(profit)      #  approx $2m
mean(profit < 0)  #  approx 1% chance of losign money


################################
# GLOBAL FINANCIAL CRASH 2007 #
###############################

# Code: Expected value with higher default rate and interest rate

p <- .04                            # Probability of default
loss_per_foreclosure <- -200000     # Loss on default
r <- 0.05                           # Interest rate
x <- r*180000                       # profit per loand
loss_per_foreclosure*p + x*(1-p)    # expected profit (640)

# Equations: Probability of losing money
# We can define our desired probability of losing money (z) as:
# Pr(S<0)=Pr(Z<−E[S]SE[S])=Pr(Z<z) 
# If μ is the expected value of the urn (one loan)
# and σ is the standard deviation of the urn (one loan)
# then  E[S]=nμ  and  SE[S]=n−−√σ .

# As in the previous video, we define the probability of losing
# money as z=0.01. In the first equation, we can see that:
# z=−E[S]SE[S] 
# It follows that:
# z=−nμn√σ=−n√μσ 
# To find the value of n for which z is less than or equal to our
# desired value, we take z≤−n√μσ and solve for n:
# n≥z2σ2μ2 

# Code: Calculating number of loans for desired probability of
# losing money. The number of loans required is:
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n   # number of loans required (22,163 loans)
n*(loss_per_foreclosure*p + x * (1-p))
# exp. profit over n loans is approximately $14m

# Code: Monte Carlo simulation with known default probability
# This Monte Carlo simulation estimates the expected profit given
# a known probability of default p=0.04
# Note that your results will differ from the video because the seed
# is not set.
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # approximately $14m


# Code: Monte Carlo simulation with unknown default probability
# This Monte Carlo simulation estimates the expected profit given
# an unknown probability of default 0.03≤p≤0.05
# modeling the situation where an event changes the probability
# of default for all borrowers simultaneously. Note that your
# results will differ from the video because the seed is not set.
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE) 
  sum(draws)
})
mean(profit)                # expected profit
mean(profit < 0)            # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million

# The Central Limit Theorem states that the sum of independent draws
# of a random variable follows a normal distribution. However, when
# the draws are not independent, this assumption does not hold.

# This is one of the reasons that the global financial crash of 2007
# happened.  A lot of financial experts assumed independence when
# this was not true
