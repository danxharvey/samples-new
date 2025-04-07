#################################################
# MOTIVATING EXAMPLE OF BASEBALL - MONEYBALL!! #
################################################

# The official term is Sabermetrics, coined by Bill James
# The introduction of statistics into professional sport
# Baseball 1990s

# BASEBALL EXPLAINED

# The goal of a baseball game is to score more runs (points) than the 
# other team

# Each team has 9 batters who have an opportunity to hit a ball with a
# bat in a predetermined order.

# Each time a batter has an opportunity to bat, we call it a plate 
# appearance (PA)

# The PA ends with a binary outcome: the batter either makes an out
# (failure) and returns to the bench or the batter doesn't (success) 
# and can run around the bases, and potentially score a run (reach all
# 4 bases)

# We are simplifying a bit, but there are five ways a batter can
# succeed (not make an out):

    # Bases on balls (BB): the pitcher fails to throw the ball
    # through a predefined area considered to be hittable 
    # (the strike zone), so the batter is permitted to go to first
    # base

    # Single: the batter hits the ball and gets to first base

    # Double (2B): the batter hits the ball and gets to second base

    # Triple (3B): the batter hits the ball and gets to third base

    # Home Run (HR): the batter hits the ball and goes all the way
    # home and scores a run

# Historically, the batting average has been considered the most
# important offensive statistic. To define this average, we define a
# hit (H) and an at bat (AB). Singles, doubles, triples and home runs
# are hits. The fifth way to be successful, a walk (BB), is not a hit
# An AB is the number of times you either get a hit or make an out; 
# BBs are excluded. The batting average is simply H/AB and is
# considered the main measure of a success rate.


# Data covers 1961-2001 because in the example we are back in 2002
# getting ready to build a team and 1961 is the year that the number
# of games played per year changed from 154 to 162

# When comparing between 2 variables the visualisation of choice
# is a SCATTERPLOT

# Scatterplott for home runs vs wins
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()
# Scatterplot of home runs vs wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Scatterplot for stolen bases vs wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Scatterplot for balls on bases vs wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)
# Balls on base shows a strong correlation, stolen bases does not

# Scatterplot for at bats vs runs
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Scatterplot of win rate vs fielding errors
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(E_per_game = E/G, R_per_game = R/G) %>%
  ggplot(aes(E_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Scatterplot of triples vs doubles (3rd base vs 2nd base runs)
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(three_per_game = X3B/G, two_per_game = X2B/G) %>%
  ggplot(aes(three_per_game, two_per_game)) + 
  geom_point(alpha = 0.5)


# INTRODUCTING CORRELATION
# To describe this we use Galton's dataset of heights for fathers
# sons.  Galton discovered regression and correlation

# Galton tried to predict sons' heights based on fathers' heights
# create the dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# The mean and standard errors are insufficient for describing an
# important characteristic of the data: the trend that the taller
# the father, the taller the son
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# The correlation coefficient is an informative summary of how two
# variables move together that can be used to predict one variable
# using the other
# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)


# The correlation coefficient is defined for a list of pairs
# (x1,y1),...,(xn,yn)  as the product of the standardized values:
# (xi?????x??x)(yi?????y??y)

# i-th entry of x is (x_i - mu_i) / sigma_i SDs away from the avg
# Similar for the y_i

# If x and y are unrelated then the product of x_i and y_i = 0
# If both positve or negative then it equals 1
# If one is +ve and -ve then it equals -1
# Unrelated variables tend to 0
# Related variables tend to -1 or 1

# The correlation coefficient essentially conveys how two variables 
# move together

# The correlation coefficient is always between -1 and 1

# Rho (Greek p is the correlation value)
rho <- mean(scale(x)*scale(y))
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)


# Sample correlation is a random variable
# In most data science applications we do not observe the entire
# population. We take a sample of a population
# As with the averagea and SD, the sample correlation is the most
# commonly used estimate of the population correlation

# This implies that the correlation that we compute and use as a 
# summary is a random variable

# As an example let's assume that the 179 pairs of fathers and sons
# is our entire population. A less fortunate geneticist can only
# afford to take a random sample of 25 pairs the sampe correlation
# can be computed as follows
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R

# We can run a monte carlo to simulate the distribution of this
# random variable R
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

# expected value and standard error
mean(R)     # mean of R is 0.5
sd(R)       # SD is relatively high for its size, SD=0.147

# When interpreting correlations, it is important to remember that
# correlations derived from samples are estimates containing
# uncertainty and may have a large standard error

# Because the sample correlation is an average of independent draws
# the central limit theorem applies. 

# For a large enough sample we know that the sample size N we know
# that the distribution of these Rs is approximately normal. The
# expected value we know is the population correlation. The SD
# is somewhat more difficult to derive

# R ~ N(rho, sqrt( (1-r^2)/(N-2) )

# In our sample with N-25, it does not appear large enough to be
# a good approximation, as we see in the QQ plot
# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))


# EXERCISES ON CORRELATION THEORY
# What is the correlation coefficient between runs per game and
# number of at bats per game
Teams %>% filter(yearID %in% 1961:2001) %>% 
      mutate(runs_per_game = R/G, at_bats_per_game =AB/G) %>%
      summarise(r=cor(runs_per_game, at_bats_per_game)) %>%
      pull(r)

# What is the correlation coefficient between runs per game and
# number of errors per game
Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(wins_per_game = W/G, errors_per_game = E/G) %>%
  summarise(r=cor(wins_per_game, errors_per_game)) %>%
  pull(r)

# What is the correlation coefficient between third base and
# second base per game
Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(third_per_game = X3B/G, second_per_game = X2B/G) %>%
  summarise(r=cor(third_per_game, second_per_game)) %>%
  pull(r)


# Alternate code
library(Lahman)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
cor(Teams_small$R/Teams_small$G, Teams_small$AB/Teams_small$G)
cor(Teams_small$W/Teams_small$G, Teams_small$E/Teams_small$G)
cor(Teams_small$X2B/Teams_small$G, Teams_small$X3B/Teams_small$G)


########################################
# ANSCOMBE'S QUARTET - STRATIFICATION #
#######################################

# Correlation is not always a good summary of the relationship between two
# variables. # A famous example used to illustrate this are the following
# for artificial data sets, referred to as Anscombe's quartet.
# All of these pairs have a correlation of 0.82 (all graphs different)
# Correlation is only meaningful in a particular context.
# To help us understand when it is that correlation is meaningful as a
# summary statistic, we'll try to predict the son's height 
# using the father's height.

# In the heights example we  know that the mean height for son's is 70.5
# and would be the number that minimising the chance of error in a prediction
# However, what if we are told that the father is 72 inches?  Do we still
# predict 70.5 inches?  The father is 1.14 SD taller than average.  Should
# we now predict that the son is 1.14 SD taller than the average too?

# It turns out that this would be an overestimate.  To see why, we look 
# at all the son's who are about 72 inches tall.  We do this by
# STRATIFYING the father's height, we call this a conditional average
# since we are computing the average son height given that the father is
# 72 inches (or whatever height we want to analyse)
# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

# The problem is that this will lead to a far smaller sample size and 
# would give larger standard errors which won't be useful for prediction

# We round the fathers heights to 72 and say we use heights approximately
# 72 inches.  We can use the following code to find the average son heights
# In this case, 71.82
conditional_avg <- galton_heights %>% filter(round(father) == 72) %>%
      summarize(avg = mean(son)) %>% .$avg   # pull(avg) also works here
conditional_avg
# Updated dataset is 70.5.  This is smaller than the SD proportion we
# expected in both cases

# Stratification followed by boxplots lets us see the distribution of
# each group
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
    ggplot(aes(father_strata, son)) +
    geom_boxplot() +
    geom_point()

# The means increase with each strata as you'd expect. They appear to
# follow a linear relationship as seen in this code:
galton_heights %>% mutate(father = round(father)) %>%
    group_by(father) %>%
    summarize(son_conditional_avg = mean(son)) %>%
    ggplot(aes(father, son_conditional_avg)) +
    geom_point()
# The slope of the line appears to be about 0.5 which happens to be the 
# actual correlation value

# To see this plot the standardised father v son heights against each other
# with a line equal to the correlation
r <- galton_heights %>% summarize(r = cor(father, son)) %>% .$r
galton_heights %>% mutate(father = round(father)) %>%
    group_by(father) %>% summarize(son = mean(son)) %>%
    mutate(z_father = scale(father), z_son = scale(son)) %>%
    ggplot(aes(z_father, z_son)) +
    geom_point() +
    geom_abline(intercept=0, slope=r)
# This line is what we call the regression line

# The regression line for two variables x and y tells us:
# for every standard deviation sigma_x increase above the average, mu_x
# y grows rho standard deviations sigma_y above the average mu_y
# The formula for the relation line is:
# (y_i - mu_y)/sigma_y  =  rho * (x_i - mu_x)/sigma_x
# For negative correlation we expect a reduction

# It is because when correlation is positive and lower than one that we
# predice something closer to the mean that we call it regression
# I.e. the son regresses to the average height

# If we write this in standard notion of y = b + mx then we see that the 
# regression line becomes:

# slope m = rho*sigma_y/sigma_x
# intercept b = mu_y - (mu_x*slope)

# If we standardise the variable to mean 0 and SD 1 the regression line
# has intercept 0 and slope rho


# We can compute the intercept and slope using the formulas we just learned
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r*s_y/s_x
b <- mu_y - m*mu_x

galton_heights %>% ggplot(aes(father, son)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = b, slope = m)

# If we plot the data in standard units as discussed
galton_heights %>% ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)


# IN SUMMARY:

# We started using conditional means but we didn't trust the sample size
# We saw that the conditional means appeared to follow a linear path
# We calculated the regression line using ALL of the data
# We calculated a much more stable slope for the population

# Are we justified in using the regression line to predict?
# Galton gives us the answer

# Correlation and the regression line are widely used summary statistics
# These are often misused or misinterpreted

# The main way that we motivate a case to use of correlation involve
# what we call the bivariate distribution

# When a pair of random variables is approximated by a
# bivariate normal distribution, the scatterplot looks like ovals
# like american footballs, then can be thin when they have high correlation
# or all the way up to circle shaped when they have no correlation.

# A more technical way to define a bivariate distribution is to say that
# it is defined for pairs, x and y, with paired values.  They are
# bivariately distributed if the following happens:

# if X and Y are normally distributed random variables and for any group
# of X, X=x, Y is approximately normal in that group, then the pair is
# approximately bivariate normal

# When we fix X in this way then we refer to the resulting distribution
# of the y's in the group  as the conditional distribution of Y given X=x
# Written as:
# f_y|X=x is the conditional distribution and
# E(Y|X=x) is the conditional expected value


# Here we stratify the son height by the standardised father height
# to see if it holds, it appears to hold
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)


# Galton showed using mathematical statistics that when two variables
# follow a bivariate normal distribution, then for any given x the
# expected value of the y in pairs for which x is set at the value
# E(Y|X=x) = mu_y + rho*((X-mu_x)/sigma_x)*sigma_y
# Note this is a line with slope rho*(sigma_y/sigma_x)
# and intercept mu_y - m*mu_x
# and therefore this is the same as the regression line and can be written:
# (E(Y|X=x)-mu_y)/sigma_y = rho*((x-mu_x)/sigma_x


# IN SUMMARY

# If our data is approximately bivariate then the conditional expectation
# is given by the regression line, which is the best prediction of y
# given that we know the value of x


# VARIANCE

# The theory we have seen tells us that the standard deviation of
# the conditional distribution is:
# SD(Y|X=x) = sigma_y * sqrt(1-rho^2)
# Var(Y|X=x) = sigma_y^2 * 


# This means that the correlation and amount of variance explained
# are related to each other
# The data explains statement is only makes sense when the data is 
# approximated by a bivariate normal distribution


# THERE ARE 2 REGRESSION LINES!
# There are two different regression lines depending on whether we are
# taking the expectation of Y given X or taking the expectation of
# X given Y.  You must compute, you cannot simply take the inverse
# P(Y|X=x) is not the inverse of P(X|Y=y)

# compute a regression line to predict the son's height from the
# father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

# compute a regression line to predict the father's height from the
# son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y

# the variation explained can be calculated as ??2×100


# EXERCISES
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mean_mothers <- mean(female_heights$mother)
mean_daughters <- mean(female_heights$daughter)
sd_m <- sd(female_heights$mother)
sd_d <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)

m3 <- r*sd_d/sd_m
b3 <- mean_daughters - m3*mean_mothers
delta <- (sd_d/sd_m)*r*1
var_height <- (r^2)*100     # % of daughter's height explained by mother's height
# Mother is 60 inches, expected value of daughter's height
# E(Y|X=x) = mu_y + rho*((X-mu_x)/sigma_x)*sigma_y
height_daughter <- mean_daughters + r*((60-mean_mothers)/sd_m)*sd_d


################
# CONFOUNDING #
###############
# Back to the baseball example
# Although it may appear that BB cause runs, it is actually the HR that
# cause most of these runs. We say that BB are confounded with HR.

# Association is not causation!  Most pitchers are scared of home runs
# hitters and so pitch more base on balls to home run hitters

# Regression can help us account for confounding.

# find regression line for predicting runs from BBs
library(tidyverse)
library(Lahman)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))


################################################
# STRATIFICATION AND MULTIVARITATE REGRESSION #
###############################################

# To determine if Bases on Balls (BB) is still useful for creating
# runs, a first approach is to keep home runs fixed at a certain value
# and then examine the relationship between runs and BBs

# Just as we did for stratifying father's heights earlier we can stratify
# home runs by say the nearest 10th
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB/G,
         Runs_per_game = R/G) %>%
  filter(HR_strata >= 0.4 & HR_strata <= 1.2)

# Make a scatterplot for each strata
dat %>% ggplot(aes(BB_per_game, Runs_per_game)) +
    geom_point(alpha=0.5) +
    geom_smooth(method="lm") +
    facet_wrap(~HR_strata)

# When we calculated this regression line it was 0.735, but once we
# stratify by home runs we see that the slopes are reduced, the values are
# closer to the value of the slope for singles (0.449) which is more
# consistent with our intuition, since both get to first base they should
# have the same predictive power
dat %>% group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, Runs_per_game)*sd(Runs_per_game)/sd(BB_per_game))

# Although we understand that BB are caused by home runs, and not the other
# way around, we can still check to see if after stratifying by BB we still
# see a home run effect or not, or if it goes up or down
# We use the same code but swap home runs for BBs
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR/G,
         Runs_per_game = R/G) %>%
  filter(BB_strata >= 2.8 & BB_strata <= 3.9)

# Make a scatterplot for each strata
dat %>% ggplot(aes(HR_per_game, Runs_per_game)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm") +
  facet_wrap(~BB_strata)

# And the slope data is given as
dat %>% group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, Runs_per_game)*sd(Runs_per_game)/sd(HR_per_game))
# Pretty constant aroudn 1.5, 1.6 or 1.7 - do not change much from original
# slope value of 1.84

# It seems that if we stratify by home runs we have an approximately
# bivariate normal distribution for runs versus bases on balls
# Similarly if we stratify by bases on balls, we have an approximately
# bivariate normal distribution for runs versus home runs

# This seems like a lot of hard work to do this for every regression line
# We are essentially using this model
# E[R|BB=x_1, HR=x_2] = beta_0 + beta_1(x_2)x_1 + beta+2(x_1)x_2

# If we take random variability into account the estimated slopes by strata
# don't appear to change that much, if these slopes are in fact the same
# this implies that the function beta1 of x2 and beta2 of x1 are constant
# which in turn implies that the expectation of runs condition on home runs
# and bases on balls can be written as a simpler model
# E[R|BB=x_1, HR=x_2] = beta_0 + beta_1 x_1 + beta_2 x_2

# This is implies that if the home runs are fixed then we observe a 
# linear relationship between runs and BB, and that the slope of that 
# relationship does not depend on the number of home runs, only that the 
# slope changes as the home runs increases. The same is true if we swap
# home runs and BBs.

# In this analysis, referred to as multivariate regression, we say that
# the bases on balls slope beta_1 is adjusted for the home run effect.

# If this model is correct then confounding has been accounted for.
# To estimate beta_1 and beta_2 we need to learn about linear models
# and least square estimate


# LINEAR MODELS
# "Linear" here does not refer to lines, but rather to the fact that the
# conditional expectation is a linear combination of known quantities.

# In Galton's model, we assume Y (son's height) is a linear combination 
# of a constant and X (father's height) plus random noise. We further
# assume that ??i are independent from each other, have expected value 0
# and the standard deviation ?? which does not depend on i.

# Note that if we further assume that ?? is normally distributed, then the
# model is exactly the same one we derived earlier by assuming bivariate
# normal data.

# We can subtract the mean from X to make ??0 more interpretable.
# Y_i = beta_0 + beta_1(x_i - X_bar) + epsilon_i, i = 1 to N

lm(son ~ father, data = galton_heights)
# Call:
# lm(formula = son ~ father, data = galton_heights)
# Coefficients:
# (Intercept)   father  
# 35.71         0.50  
 #The coefficient for "father" gives the predicted increase in son's height
# for each increase of 1 unit in the father's height. 
# In this case, it means that for every inch we increase the father's
# height, the son's predicted height increases by 0.5 inches.


# We want the intercept term for our model to be more interpretable, so
# we run the same model as before but now we subtract the mean of
# fathers' heights from each individual father's height to create a new
# variable centered at zero.
galton_heights <- galton_heights %>%
  mutate(father_centered=father - mean(father))
# We run a linear model using this centered fathers' height variable.
lm(son ~ father_centered, data = galton_heights)
# Call:
# lm(formula = son ~ father_centered, data = galton_heights)
# Coefficients:
# (Intercept)    father_centered  
# 70.45          0.50  
# Because the fathers' heights (the independent variable) have been 
# centered on their mean, the intercept represents the height of the son
# of a father of average height. In this case, that means that the height 
# of a son of a father of average height is 70.45 inches.

# If we had not centered fathers' heights to its mean, then the intercept
# would represent the height of a son when a father's height is zero.


# Suppose we fit a multivariate regression model for expected runs based
# on BB and HR:
# E[R|BB=x1,HR=x2]=??0+??1x1+??2x2 
# Suppose we fix  BB=x1 . Then we observe a linear relationship between
# runs and HR with intercept of: ??0+??1x1
# If x1 is fixed, then ??1x1 is fixed and acts as the intercept for 
# this regression model. This is the basis of stratificaton.  


# Which of the following are assumptions for the errors ??i in a linear
# regression model? - Answer is all:
  # The ??i are independent of each other
  # The ??i have expected value 0
  # The variance of ??i is a constant


###########################
# LEAST SQUARES ESTIMATE #
##########################

# For linear models to be useful we have to estimate the unknown parameters
# the betas, the standard approach in science is to find the values that
# that minimise the distance of the fitted model to the data. To quantify
# this we use the least squares equation

# For Galton's data we would write something like this:
# RSS = Summation i=1 to n {Y_i - (beta_0+beta_1*x_i)}^2
# RSS = Residual Sum of Squares
# The values that minimise the RSS are the LSE
# They are denoted as beta_0_hat and beta_1_hat
rss <- function(beta0, beta1, data){
          resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
          return(sum(resid^2))
        }

# So far any pair of values we get an RSS and a 3-dimensional plot with
# beta0, bet1 and RSS being the x, y and z

# In this example we are going to look at a 2-dimensional plot by keeping
# beta0 fixed at 25 and look at the RSS as a function on beta1
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1=beta1, rss=sapply(beta1, rss, beta0=25))
results %>% ggplot(aes(beta1, rss)) + 
            geom_line() + 
            geom_line(aes(beta1, rss), col=2)
# We can see a clear minimum at around 0.65 making this our LSE
# However, this minimum is for beta1 when beta0 is fixed at 25
# We don't know if (25, 0.65) minimises the equation across all pairs

# Trial and error is not practical here so we will use calculus
# We'll take partial derivatives, set them to zero and solve for 
# beta0 and beta1
# These calculations can get quite complex if we have many paraemeters
# Fortunately R has functions to do this for us


# In R, we can use the lm() function to find the LSE
# To fit the following model where Y_i is the son's height
# Y_i = beta0 + beta1*x_i + epsilon_i   # y = son, x = father, eps = error
fit <- lm(son~father, data=galton_heights)
fit
# This gives us the LSE as the outputs of R (Intercept[1], father[2])

# The way that lm() works is as follows:
# Value to predict is first before the ~
# Value to use to predict comes after the ~
# The intercept is added automatically

# There is more information than shows and we can use summary() to extract
# more of this information
summary(fit)

# To understand some of the information contained in this summary we need
# to remember that these the LSE are random variables.  Mathematical
# statistics gives us some idea of the distribution of these random variables

# The LSE are derived from the data Y1 to YN which applies that they are
# random.  We can see this using a monte carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
          sample_n(galton_heights, N, replace=TRUE) %>%
          lm(son~father, data = .) %>% .$coef
        })

lse <- data.frame(beta0=lse[1,], beta1=lse[2,])
# This gives us several estimates of the regression slope

# To see the variability of the estimates
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta0)) + geom_histogram(binwidth = 5, color="black")
p2 <- lse %>% ggplot(aes(beta1)) + geom_histogram(binwidth = 0.1, color="black")
grid.arrange(p1, p2, ncol=2)
# These look normal as the CLT applies here as well

# To see the standard error of one of our estimates
sample_n(galton_heights, N, replace=TRUE) %>% 
      lm(son~father, data=.) %>% summary    # 9.6099 and 0.1385
# Note how close they are to the monte carlo
lse %>% summarize(se0 = sd(beta0), se1 = sd(beta1))   # 8.5647 and 0.1241

# The t-statistic that we see is not actually based on the CLT but rather
# on the assumptions that the epsilons (errors) follow a normal distribution
# Mathematical theory tells us that the LSE / Standard error follow a
# t-distribution with N-P degrees of freedom, with p equal to the number
# of parameters in our model (in this case, 2)
# beta0_hat / SE_hat(beta0_hat)
# beta1_hat / SE_hat(beta1_hat)

# The 2 p-values are testing the null hypothesis that both beta0 and beta1
# are 0.

# Note if N is large enough then the CLT works and the t-distribution
# becomes almost the same as a normal distribution

# So if you either you assume that the errors are normal and use the 
# t-distribution, or if you assume that N is large enough to use the CLT
# then you can construct confidence intervals for your parameters

# Hypothesis testing for regression models is used very commonly in 
# epidemiology and economically to make statements such as 
# the effect of A and B was statistically significant after adjusting
# for X, Y and Z

# It's very important to note that several assumptions have to hold true
# for these statements to also hold true

# Although interpretation is not straight-forward, it is also useful to
# know that the LSE can be strongly correlated, which can be seen using
# this code:
lse %>% summarize(cor(beta0, beta1))
# However, the correlation depends on how the predictors are defined 
# or transformed.
# Here we standardize the father heights, which changes xi to  xi???xbar
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})
# Observe what happens to the correlation in this case
cor(lse[1,], lse[2,]) 


# PREDICTED VARIABLES ARE RANDOM VARIABLES

# Once we fit our model we can obtain predictions of y by plugging the
# estimates into the regression model
# For son Y and father X heights
# Yhat = beta0hat + beta1hat*x

# We're just plugging beta into the equation
# If we plot Yhat versus x we will see the regression line

# The following code plots confidence intervals around the predicted Yhat
# ggplot layer:
# geom_smooth(method="lm")
galton_heights %>% ggplot(aes(son, father)) +
    geom_point() +
    geom_smooth(method="lm")

# The function predict() takes an lm object as input and returns
# these predictions with this code
galton_heights %>%
    mutate(Y_hat = predict(lm(son~father, data=.))) %>%
    ggplot(aes(father, Y_hat)) +
    geom_line()


# If requested the standard errors and other information from which
# we can construct confidence intervals can be obtained from the
# predict function. You can see it by running this code
fit <- galton_heights %>% lm(son~father, data=.)
Y_hat <- predict(fit, se.fit=TRUE)
names(Y_hat)


# ASSESSMENT LEAST SQUARES ESTIMATE LSE
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)
# In a model for sons' heights vs fathers' heights, what
# is the least squares estimate (LSE) for ??1 if we assume ??^0 is 36?
# Alter code for beta0 = 36

# The least squares estimates for the parameters ??0,??1,.,??n minimize  # correct
# the residual sum of squares.


# Create a lm for runs per game based on home runs and bases on balls
library(Lahman)
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game = R/G,
        HR_per_game = HR / G,
        BB_per_game = BB / G)

dat %>% lm(R_per_game ~ HR_per_game+BB_per_game, data=.) %>% .$coef


# We run a Monte Carlo simulation where we repeatedly take samples of 
# N = 100 from the Galton heights data and compute the regression 
# slope coefficients for each sample:
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

# What does the central limit theorem tell us about the variables
# beta_0 and beta_1?
    # They are approximately normally distributed
    # The expected value of each is the true value of ??0 and ??1
    #  (assuming the Galton heights data is a complete population)


# In an earlier video, we ran the following linear model and looked
# at a summary of the results.
mod <- lm(son ~ father, data = galton_heights)
summary(mod)
lm(formula = son ~ father, data = galton_heights)

# What null hypothesis is the second p-value (the one in the father row)
# testing?
# Ans: ??1=0, where ??1 is the coefficient for the variable "father"
# The p-value for "father" tests the null hypothesis that ??1=0
# ie, the fathers' heights are not associated with the sons' heights,
    # where ??1 is the coefficient for the variable father.


# Which R code(s) below would properly plot the predictions and confidence
# intervals for our linear model of sons' heights?
# Option 1: True
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

# Alternative code: True
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% 
          bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))


# Q7 and Q8
# Define female_heights, a set of mother and daughter heights sampled
# from GaltonFamilies, as follows:
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# Fit a linear regression model predicting the mothers' heights using
# daughters' heights.
fit <- female_heights %>% lm(mother~daughter, data=.)
fit$coef[2]
fit$coef[1]

# Predict mothers' heights using the model.
# What is the predicted height of the first mother in the dataset?
fit <- female_heights %>% lm(mother~daughter, data=.)
X_hat <- predict(fit, se.fit=TRUE)
X_hat$fit[1]
# smoother code
predict(fit)[1]
# What is the actual height of the first mother?
female_heights$mother[1]



# We have shown how BB and singles have similar predictive power for scoring 
# runs. Another way to compare the usefulness of these baseball metrics
# is by assessing how stable they are across the years. Because we have
# to pick players based on their previous performances, we will prefer 
# metrics that are more stable. In these exercises, we will compare the
# stability of singles and BBs.

# Before we get started, we want to generate two tables: one for 2002 
# and another for the average of 1999-2001 seasons. We want to define
# per plate appearance statistics, keeping only players with more than
# 100 plate appearances. Here is how we create the 2002 table, 
# including each player's singles rate and BB rate for the 2002 season:
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)


# Now compute a similar table but with rates computed over 1999-2001. 
# Keep only rows from 1999-2001 where players have 100 or more plate 
# appearances, calculate each player's single rate and BB rate per
# season, then calculate the average single rate (mean_singles) and
# average BB rate (mean_bb) per player over those three seasons.
bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

# How many players had a mean single > 0.2 for 1999-2001
bat_01 %>%
    group_by(playerID) %>%
    summarize(mean_bb = mean(bb), mean_singles = mean(singles)) %>%
    count(mean_singles > 0.2)
# How many players had a mean bb > 0.2 for 1999-2001
bat_01 %>%
  group_by(playerID) %>%
  summarize(mean_bb = mean(bb), mean_singles = mean(singles)) %>%
  count(mean_bb > 0.2)

# Alternative code
bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
  sum(bat_99_01$mean_singles > 0.2)
  # sum(bat_99_01$mean_bb > 0.2)

  
# Combine your two tables with inner join
batcomb <- inner_join(bat_02, bat_99_01, by="playerID")
batcomb %>% summarize(r = cor(singles, mean_singles))
batcomb %>% summarize(r = cor(bb, mean_bb))
# Initially forgot to join on the averages of 1999:2001


# Make scatterplots to see if they are bivariate normal
ds_theme_set()
# mean_singles versus singles
batcomb %>%
  ggplot(aes(singles, mean_singles)) + 
  geom_point(alpha = 0.5)
# mean_bb versus bb
batcomb %>%
  ggplot(aes(bb, mean_bb)) + 
  geom_point(alpha = 0.5)
# Both are bivariate normal


# Fit a linear model to predict 2002 singles given 1999-2001 mean_singles
fit_s <- batcomb %>% lm(singles~mean_singles, data=.)
fit_b <- batcomb %>% lm(bb~mean_bb, data=.)
fit_s$coef[2]
fit_b$coef[2]


#############################
# ADVANCED DPLYR - TIBBLES #
############################

# Whilst performing the above calculations to return regression lines
# and confidence intervals using the lm() function, we should learn that
# the lm() function does not recognise the gruopo_by() function, this means
# that we cannot use the lm() function to return the estimated slope using
# the group_by function()
# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

# use lm to get estimated slopes - lm does not work with grouped tibbles
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

# This is expected as the lm() function is not part of the tidyverse
# group_by() is a part of the tidyverse, it retrns a group_by tibble
# This happens because a tibble is part of the tidyverse

# Let's look at the output of a group_by call
dat %>% group_by(HR) %>% head  # Returns a tibble

# To see the class of the object
dat %>% group_by(HR) %>% class
# tbl is pronounced tibble in this case
# It is a type of data frame, think of it as a modern data frame
# group_by() and summarize() always return a tibble
# grouped tibble seen as "grouped_df", or "tbl_df"

# Note that manipulation verbs such as select, filter, mutate and arrange
# don't necessarily return tibbles, they preserve the class of the input


# TIBBLES - DIFFERENCES FROM DATA FRAMES
# Here are some of the main difference between tibbles and data frames

# The print method for a tibble is much more readable than data frames
# Standard data frame is unreadable due to many rows and columns that wrap
library(Lahman)
Teams

# As a tibble, when it wraps it just gives a summary of col names
as_tibble(Teams) 


# If you subset columns of a data fram you may get back an object
# that is not a data frame.  Here is an example:
class(Teams[,20])             # Returns integer
# With a tibble this is not the case:
class(as_tibble(Teams[,20]))  # Returns tibble
# This is useful in the tidyverse because functions require
# data frames as inputs

# What about when you want to access the vector that defines a 
# column in the original dataframe, you use the accessor $ sign
class(as_tibble(Teams)$HR)    # Returns integer

# A tibble will give you a warning if you try to access a column
# that does not exist.  A data frame will not do this, it will
# retun null
Teams$hr              # Column is HR, returns null, hard to debug
as_tibble(Teams)$hr   # Tibble gives warning 


# Third difference
# Dataframes need vectors of number strings or boolean
# Tibbles can have complex entries such as lists or functions
# We create tibbles with the tibble() function
tibble(id=c(1, 2, 3), func=c(mean, median, sd))

# Tibbles can be grouped, a grouped tibble, returned by the function
# group_by.  The tibble stores information about which rows are with
# each group and this is what the summarize() function uses to work
# with tibbles

# The lm function we saw earlier, which is not part of the tidyverse,
# does not know how to deal with group tibbles.  The object is 
# basically converted to a regular dataframe and then the function
# runs ignoring the groups, this is why we only get one pair of 
# estimates as we see here
dat %>% group_by(HR) %>% lm(R~BB, data=.)

# To make these non-tidyverse functions integrate better, we use
# a new function do()


# DO() FUNCTION

# As the pipe %>% is an essential part of the tidyverse, this is
# why the inputs and outputs are consistent, to ensure the data
# is passed correctly for functions to chain together

# The do() function serves as a bridge between R and the tidyverse
# It understands tibbles and ALWAYS returns a DATAFRAME

# To find our HR strata data
dat %>% group_by(HR) %>% do(fit = lm(R~BB, data=.))
# NOTE:
  # Column name is defined by us
  # If we don't then it returns actual output of lm
      # which will generate an error since do() expects a dataframe
  # It returns the strata data and the output as a function


# We could build the code differently to get a useful output for
# lm using a function
get_slope <- function(data){
  fit <- lm(R~BB, data=data)
  data.frame(slope = fit$coefficients[2],
                  se = summary(fit)$coefficients[2])
}
# and THEN use do() to get an output, note we don't have to name
# the column since we are already getting a data frame returned
# by the function
dat %>% group_by(HR) %>% do(get_slope(.))    # . is for data

# NOTE if we name the column then we get a complex tibble
dat %>% group_by(HR) %>% do(slope = get_slope(.))
# This is not very useful


# One more feature of do().  If the dataframe being returned has
# more than 1 row then these will be concatenated appropriately
# Here is an example that returns both estimated parameters
get_lse <- function(data){
  fit <- lm(R~BB, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients,
             se = summary(fit)$coefficients[,2])
  }
dat %>% group_by(HR) %>% do(get_lse(.))

# If this seems a little too complicated then you're not alone
# It is, and now we learn about  broom package which was created to 
# facilitate the use of model fitting functions such as lm()
# with the tidyverse


# THE BROOM PACKAGE
# The original task in the previous video was to provide an
# estimate and confidence intervals for the slop estimate of each
# strata. The broom package will make this quite easy

# The broom package has 3 main functions all of which extract
# information from the object returned by the function lm()
# and return it in a tidyverse friendly data frame

# The functions are tidy, glance and augment

# TIDY returns estimates and related information as a dataframe
library(broom)
fit <- lm(R~BB, data=dat)
tidy(fit)
# We can add other important summaries like confidence intervals
# using extra arguments
tidy(fit, conf.int = TRUE)
# As the output is a dataframe(tibble) we can immediately use it
# with do
dat %>% group_by(HR) %>% do(tidy(lm(R~BB, data=.), conf.int=TRUE))
# We can then refine our selection
dat %>% group_by(HR) %>% 
  do(tidy(lm(R~BB, data=.), conf.int=TRUE)) %>%
  filter(term=="BB") %>%
  select(HR, estimate, conf.low, conf.high)

# This then makes it easy to plut into ggplot
dat %>% group_by(HR) %>% 
  do(tidy(lm(R~BB, data=.), conf.int=TRUE)) %>%
  filter(term=="BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y=estimate, ymin=conf.low, ymax=conf.high)) +
  geom_errorbar() +
  geom_point()


# Back to our question of determining if slopes change. The plot we
# just made using do and broom shows that the confidence intevals overlap
# which provides a nice visual confirmation that our assumption that
# the slopes do not change with home run strata, is a relatively safe
# assumption

# Earlier we mentioned two other functions of the broom package
# glance - model specific outcomes
# augment - observation specific outcomes

# Here we see the model fit summary that glance returns
glance(fit)
augment(fit)
# These are covered in the textbook and a later video (augment)


# EXERCISES
# You want to take the tibble dat, which we used in the video on the
# do() function, and run the linear model R ~ BB for each strata of HR.
# Then you want to add three new columns to your grouped tibble: the
# coefficient, standard error, and p-value for the BB term in the model.
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

# What additional code could you write to accomplish your goal?
dat %>% 
  group_by(HR) %>% 
  do(get_slope(.))


# You want to know whether the relationship between home runs and 
# runs per game varies by baseball league. You create the following 
# dataset:
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
        mutate(HR = HR/G, R = R/G) %>%
        select(lgID, HR, BB, R) 

# What code would help you quickly answer this question?
dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 




# We have investigated the relationship between fathers' heights and sons' 
# heights. But what about other parent-child relationships? Does one parent's
# height have a stronger association with child height? How does the child's
# gender affect this relationship in heights? Are any differences that we
# observe statistically significant?

# The galton dataset is a sample of one male and one female child from each
# family in the GaltonFamilies dataset. The pair column denotes whether the
# pair is father and daughter, father and son, mother and daughter, or mother
# and son.

# Create the galton dataset using the code below:
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))
galton


# Group by pair and summarize the number of observations in each group.
galton %>% group_by(pair) %>% count
# Alt code - galton %>% group_by(pair) %>% summarize(n=n())


# Calculate the correlation coefficients for fathers and daughters,
# fathers and sons, mothers and daughters and mothers and sons.
# Which pair has the strongest correlation in heights?
# Which pair has the weakest correlation in heights?
galton %>% group_by(pair) %>% 
  summarize(cor = cor(childHeight, parentHeight)) # %>%
  # filter(cor == max(cor))
  # filter (cor == min(cor))


# Use lm() and the broom package to fit regression lines for each
# parent-child pair type. Compute the least squares estimates, 
# standard errors, confidence intervals and p-values for the
# parentHeight coefficient for each pair.
fit <- galton %>% group_by(pair) %>%
  do(tidy(lm(childHeight~parentHeight, data=.), conf.int=TRUE)) %>%
  filter(term == "parentHeight", pair == "father_daughter") %>%
  pull(estimate)
# Estimate of father~daughter coefficient
# 0.345
# For every 1 inch increase in mother's height, how many inches does
# typical son's height increase
fit <- galton %>% group_by(pair) %>%
  do(tidy(lm(childHeight~parentHeight, data=.), conf.int=TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_son") %>%
  pull(estimate)
# 0.381 ~ coefficient means for every 1 inch increase of mother,
# The son's height will increase by this much



# Which sets of parent-child heights are significantly correlated
# at a p-value cut off of .05?
# ALL pairs as p-value is < 0.05 for each
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight" & p.value < .05)
# Which of the following statements are true?
# Answers:
  # All of the confidence intervals overlap each other
  # The confidence intervals involving mothers' heights are larger
      # than the confidence intervals involving fathers' heights
  # The data are consistent with inheritance of height being 
      # independent of the child's gender
  # The data are consistent with inheritance of height being
      # independent of the parent's gender

# Incorrect options (not chosen):
  # The confidence intervals involving daughters' heights are 
      # larger than the confidence intervals involving sons' heights
  # At least one confidence interval covers zero




#################################################
# BUILD A BETTER OFFENSIVE METRIC FOR BASEBALL #
################################################

# To continue the baseball example we can expand the lm() to take 
# two variables like this
fit <- Teams %>% filter(yearID %in% 1961:2001) %>%
    mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
    lm(R ~ BB + HR, data=.)
# We can add a tidy function to see a nice summary
tidy(fit, conf.int = TRUE)
# When we did the model with just one variable we saw the 
# estimated slopes at 0.735 (BB) and 1.844 (HR)
# Under the multivariate model seen above both of these slopes
# go down

# To construct a model to pick players we will add more variables
# and take a leap of faith to say that all variables are independently
# normal. if this holds true then we see the following model:
# Y_i = beta0 + beta1x_i_1 + beta2x_i_2 + beta3x_i_3 + beta4x_i_4
# + beta5x_i_5 + epsilon_i
# where 1 = BB, 2 = Singles, 3 = Doubles, 4 = Tripes, 5 = HRs

# Using lm() we can find the LSE for these parameters
fit <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  lm(R ~ BB + singles + doubles + triples + HR, data=.)
# Using tidy to see a formatted answer
coefs <- tidy(fit, conf.int = TRUE)

# To see how well this works at the team level let's use it to predict
# 2002 data against the observed values (note our filter cuts off at 2001)
Teams %>% filter(yearID %in% 2002) %>%
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  mutate(R_hat = predict(fit, newdata=.)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()
# We see that this is quite accurate and better than just using batting
# average or home runs

# Specifically to define a metric for player A we would imagine a team
# made up of player A and use our fitted regression model to predict
# how many runs this team would produce
# -2.769 + 0.371*BB + 0.519*singles + 0.771*doubles + 1.240*triples
# + 1.443*HR
# We plug our values in beta (estimated coefficients) into regression formula
# However we have more work to do as this is team level

# We need to adjust for amount of game time played - we can use
# per-plate-appearance rate, to do this we calculate the average number
# pf per-plate-appearances per game
pa_per_game <- Batting %>% filter(yearID %in% 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
  .$pa_per_game %>% mean

# Now, we're ready to use our metric.
# We're going to compute the per-plate-appearance rates for players
# available in 2002. But we're going to use data from 1999-2001.
# Because remember, we are picking players in 2002. We don't know what 
# happened yet. We'll filter players with few appearances
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))


# The player-specific predicted runs computer here can be interpreted as
# the number of runs we would predict a team to score if this team
# was made up of just that player, if that player batted every single time.
# The distribution shows that there's wide variability across players,
# as we can see here.
players %>% ggplot(aes(R_hat)) +
  geom_histogram(binwidth = 0.5, color="black")
# plot player-specific predicted runs (same plot different method)
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5,
      color = I("black"))

# As we are pretending to be the Oakland A's with a 2002 budget of $40m
# we must calculate salary
# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players - well known players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# We see high metrics with low salaries, these are young players
# who have not yet re-negotiated salaries
# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()



######################################
# LINEAR PROGRAMMING - CODE EXAMPLE #
#####################################

# A way to actually pick the players for the team can be done using 
# what computer scientists call linear programming. Although we don't go
# into this topic in detail in this course, we include the code anyway:
library(reshape2)
library(lpSolve)
players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 

# This algorithm chooses these 9 players:
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

# We note that these players all have above average BB and HR rates
# while the same is not true for singles.
my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))


# ON BASE PLUS SLUGGING (OPS)
# Common metric that became used by ESPN and other networks
# Sabermetricians realised they needed to weight the formula
# The on-base-percentage plus slugging percentage (OPS) metric is:
# BB/PA + (Singles+2*Doubles+3*Triples+4*HR)/AB

# Whilst sabermetricians were probably not using regression a plot
# shows that this is a highly correlated statistic


##############################################
# THE SOPHOMORE SLUMP OR REGRESSION FALLACY #
#############################################

# Regression can bring about errors in reasoning, especially when
# interpreting individual observations.
# The example showed in the video demonstrates that the
# "sophomore slump" observed in the data is caused by regressing to
# the mean.  Top players appear worse, bottom players appear better

# This refers to the common 2nd year, 2nd album, 2nd whatever slump
# that we see in many walks of life.  Is it statistically true?
# Fox Sports asked will MLBs tremendous rookie class of 2015
# suffer a sophomore slump?
# Note pitchers are filtered out as they do not receive batting awards
# Focus to use is batting average for rookie and sophomore seasons
# Only using players that won rookie of the year award, each year

# The code to create a table with player ID, their names, and their 
# most played position:
library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

# The code to create a table with only the ROY award winners and add
# their batting statistics:
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

# The code to keep only the rookie and sophomore seasons and remove 
# players who did not play sophomore seasons:
ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

# The code to use the spread function to have one column for the
# rookie and sophomore years batting averages:
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY

# The code to calculate the proportion of players who have a lower batting average their sophomore year:
mean(ROY$sophomore - ROY$rookie <= 0)
# 68.6% appear to regress

# The code to do the similar analysis on all players that played the 
# 2013 and 2014 seasons and batted more than 130 times 
# (minimum to win Rookie of the Year):
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years

# The code to see what happens to the worst performers of 2013:
arrange(two_years, `2013`)

# The code to see  the correlation for performance in two separate years:
qplot(`2013`, `2014`, data = two_years)
summarize(two_years, cor(`2013`,`2014`))  # cor = 0.46
# The correlation is present but not highly correlated
# The data looks like a bivariate normal distribution which means
# that if we were to predict the 2014 batting average, call it y
# for a given player that had a 2013 batting average of x
# we would use the regression formula like this:
# (Y-0.255)/0.032 = 0.46*( (X-0.261)/0.023 )

# As the correlation is not perfect, regression tells us that on
# average we would expect high performers from 2013 to do a little
# worse in 2014.  This is regression to the mean.  It is not a jinx
# It is just chance

# The Rookies of the Year are selected from the top values of X so it
# is expected that their Y will regress to the mean


#############################
# MEASUREMENT ERROR MODELS #
############################

# Up until all our regression examples have been applied to 2 or more
# random variables.  We assume that all pairs are bivariate normal
# and use this to motivate a linear model. This approach covers
# most of real life examples were linear regression is used

# The other major application comes from measurement error models
# Here, it is common to have nonrandom covariates, such as time.
# Randomness is introduced via measurement error rather than
# sampling or natural variability

# To understand this we are going to motivate an example taken from
# physics.  In the 16th century, Gallileo tried to measure the velocity
# of a falling object.
library(dslabs)
falling_object <- rfalling_object()
# An assistant climbs the Tower of Pisa and drops a ball whilst several
# other assistants record the position at different times.  The
# assistants hand the data to Gallileo and this is what he sees when
# he makes a ggplot
falling_object %>% ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")

# Gallileo does not know the formula but by looking at the plot
# he deduces that the formula should follow a parabola
# f(x) = beta0 + beta1*x + beta2*x^2

# The data does not fall exactly on the parabolic curve but Gallileo
# knows that this is due to measurement error
# To account for this we write this model
# Y_i = beta0 + beta1*x + beta2*x^2 + epsilon_i, i = 1 to n
# Y = distance ball dropped in meters
# x_i = time in seconds
# epsilon = measurement error, assumed to be random, independent
      # has the same distribution and has no bias = exp. value = 0

# Note that this is a linear model because it is a linear combination
# of known quantities X and X squared, with unknown quantities, betas
# The X are fixed quantities, it is just time, we are not conditioning

# To pose a new physical theory Gallileo needs actual numbers rather
# than unknown parameters. The LSE seems like a reasonable approach
# LSE calculations do not require the errors to be approximately normal
# The lm() function will find the betas that minimise the
# Residual Sum of Square (RSS) which is what we want
# We can use this code to obtain our betas
fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)
tidy(fit)

# The broom function augment lets us easily check if the estimated
# parabola fits the function
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")
# The curve goes right through the points

# The trajectory of a falling object is given as:
# d = h0 +v0*t - 0.5*9.8t^2
# h0 = starting height
# v0 = starting velocty

# We know that the Tower of Pisa is approximately 56.67m high
# and that the initial velocty is zero
# These known constants are consistent with the parameters that we estimated
# and we can see this with the tidy function
tidy(fit, conf.int = TRUE)

# The height of the Tower of Pisa is within the confidence interval
# for beta0 (intercept).  The initial velocity is in the conf. interval
# for beta1 - note that the p-value is larger than 0.05 which means that
# we wouldn't reject the null hypothesis that the initial velocity is zero
# Finally the accelaration constant is within the conf. interval for 
# (-0.5*9.8)t^2 = 4.9t^2



# EXERCISES
# Use the Teams data frame from the Lahman package. Fit a multivariate linear 
# regression model to obtain the effects of BB and HR on Runs (R) in 1971. Use
# the tidy() function in the broom package to obtain the results in a
# data frame.
fit <- Teams %>% filter(yearID %in% 1971) %>%
  lm(R~BB+HR, data=.)
tidy(fit)


# Repeat the above exercise to find the effects of BB and HR on runs (R) for
# every year from 1961 to 2018 using do() and the broom package.
# Make a scatterplot of the estimate for the effect of BB on runs over time 
# and add a trend line with confidence intervals.
# Fill in the blank to complete the statement:
# "The effect on BB on runs has <<select>> over time"
  # decreased
  # increased - This is correct answer
  # remained the same
Teams %>% filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R~BB+HR, data=.))) %>%
  filter(term=="BB") %>%  
  ungroup() %>%
  ggplot(aes(yearID, y=estimate)) +
  geom_point() +
  geom_smooth(method="lm")

# Tidier code - Can be used in next question
res <- Teams %>% filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%   do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 

res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")



# Fit a linear model on the results from Question 10 to determine the effect
# of year on the impact of BB.
ans <- res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .)

# Answer code is improved for a specific value
# For each additional year, by what value does the impact of BB on runs change?
res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(estimate)
# What is the p-value for this effect?
res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(p.value)




#####################################
# FINAL ASSESSMENT - LINEAR MODELS #
####################################

# Load data and create new dataframe with attendance
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)


# Use linear models to answer the following 3-part question about Teams_small.
# Use runs (R) per game to predict average attendance.
# For every 1 run scored per game, average attendance increases by how much?
fit <- Teams_small %>%
    mutate(rpg = R/G) %>%
    lm(avg_attendance~rpg, data = .)
summary(fit)

# Use home runs (HR) per game to predict average attendance.
# For every 1 home run hit per game, average attendance increases by how much?
fit2 <- Teams_small %>%
  mutate(hrg = HR/G) %>%
  lm(avg_attendance~hrg, data = .)
summary(fit2)


# Use number of wins to predict average attendance; do not normalize for number
# of games.
# For every game won in a season, how much does average attendance increase?
fit3 <- Teams_small %>%
  lm(avg_attendance~W, data = .)
summary(fit3)
# Suppose a team wins zero games - take the intercept value


# Use year to predict average attendance.
# How much does average attendance increase each year?
fit4 <- Teams_small %>%
  lm(avg_attendance~yearID, data = .)
summary(fit4) %>% .$coeff %>% .[2]


# Game wins, runs per game and home runs per game are positivelycorrelated
# with attendance. We saw in the course material that runs per game and
# home runs per game are correlated with each other.
# Are wins and runs per game or wins and home runs per game correlated?
# What is the correlation coefficient for wins and runs per game?
Teams_small %>%
  mutate(rpg = R/G) %>%
  summarize(r = cor(rpg, W))
# What is the correlation coefficient for wins and home runs per game?
Teams_small %>%
  mutate(hrg = HR/G) %>%
  summarize(r = cor(hrg, W))


# Stratify Teams_small by wins: divide number of wins by 10 and then round
# to the nearest integer. Keep only strata 5 through 10, which have 20
# or more data points.
strata <- Teams_small %>%
  mutate(W_strata = round(W/10), rpg=R/G, hrg=HR/G) %>%
  filter(W_strata >= 5 & W_strata <= 10)

# Use the stratified dataset to answer this three-part question.
# How many observations are in the 8 win strata?
# (Note that due to division and rounding, these teams have 75-85 wins.)
sum(strata$W_strata == 8)

# Calculate the slope of the regression line predicting average attendance
# given runs per game for each of the win strata.
# Which win stratum has the largest regression line slope?
get_slope_rpg <- function(data){
  fit <- lm(avg_attendance~rpg, data=data)
  data.frame(slope=fit$coefficients[1])
}
strata %>% group_by(W_strata) %>% do(get_slope_rpg(.))

# Calculate the slope of the regression line predicting average attendance 
# given HR per game for each of the win strata.
# Which win stratum has the largest regression line slope?
get_slope_hrg <- function(data){
  fit <- lm(avg_attendance~hrg, data=data)
  data.frame(slope=fit$coefficients[2])
}
strata %>% group_by(W_strata) %>% do(get_slope_hrg(.))


# More efficient code is this:
# calculate slope of regression line after stratifying by R per game
strata %>% group_by(W_strata) %>%
  summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance)/sd(R/G))

# calculate slope of regression line after stratifying by HR per game
strata %>% group_by(W_strata) %>%
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))



# Fit a multivariate regression determining the effects of runs per game, home
# runs per game, wins, and year on average attendance. Use the original 
# Teams_small wins column, not the win strata from question 3.

# What is the estimate of the effect of runs per game on average attendance?
# What is the estimate of the effect of home runs per game on average attendance?
# What is the estimate of the effect of number of wins in a season on
# average attendance?
fit <- Teams_small %>%
  mutate(rpg = R/G, hrg = HR/G) %>%
  lm(avg_attendance ~ rpg+hrg+W+yearID, data=.)
# We can add a tidy function to see a nice summary
tidy(fit, conf.int = TRUE)    # Use the estimate value


# Use the multivariate regression model from Question 4. Suppose a team 
# averaged 5 runs per game, 1.2 home runs per game, and won 80 games in a season.
Teams_small %>%
  mutate(rpg = 5, hrg = 1.2, W = 80, yearID=1960) %>%
  mutate(R_hat = predict(fit, newdata = .))
# This code worke but elegant answer code below

# What would this team's average attendance be in 2002?
predict(fit, data.frame(R_per_game = 5, 
              HR_per_game = 1.2, 
              W = 80, 
              yearID = 2002))

# What would this team's average attendance be in 1960?
predict(fit, data.frame(R_per_game = 5, 
                        HR_per_game = 1.2, 
                        W = 80, 
                        yearID = 2002))



# Use your model from Question 4 to predict average attendance
# for teams in 2002 in the original Teams data frame.
# What is the correlation between the predicted attendance 
# and actual attendance? (model defined above as fit)
Teams2002 <- Teams %>% filter(yearID =="2002") %>% 
    mutate(avg_attendance = attendance/G, 
           hrg = HR / G, 
           rpg = R / G)

# Create estimates and find correlation
estimates <- predict(fit, Teams2002)
cor <- cor(estimates, Teams2002$avg_attendance)




###############################################
# CONFOUNDING - CAUSATION IS NOT CORRELATION #
##############################################

# Association/correlation is not causation.
# p-hacking is a topic of much discussion because it is a
# problem in scientific publications. Because publishers tend to reward
# statistically significant results over negative results, there is an
# incentive to report significant results.

# spurious correlations, or p-hacking are also called data dredging,
# data phishing or data snooping. In the US they call this cherry picking

# There are plenty of examples such as margine and divorce with cor = 0.93
# Monte carlo simulations of random variables with 1,000,000 runs can also
# give a high correlation value due to number of repeat experiments

# An example of this would be to look through many results and pick the one
# that supports the result that you want to defend

# P-hacking is a subject of much discussion because it is problematic in
# scientific publications, because publishers tend to reward statistically
# significant results over negative results

# In epidemiology in the social sciences for example,
# researchers may look for associations between an average outcome
# and several exposures, and report only the one exposure
# that resulted in a small p-value.
# Furthermore, they might try fitting several different models
# to adjust for confounding and pick the one
# model that yields the smallest p-value.
# In experimental disciplines, an experiment
# might be repeated more than once, and only the one
# that results in a small p-value are reported.
# This does not necessarily happen due to unethical behavior,
# but rather to statistical ignorance or wishful thinking.
# In advanced statistics courses, you'll learn
# methods to adjust for what is called the multiple comparison problem.

# generate the Monte Carlo simulation
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))

# calculate correlation between X,Y for each group
res <- sim_data %>% 
  group_by(group) %>% 
  summarize(r = cor(x, y)) %>% 
  arrange(desc(r))
res

# plot points from the group with maximum correlation
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(x, y)) +
  geom_point() + 
  geom_smooth(method = "lm")

# histogram of correlation in Monte Carlo simulations
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")

# linear regression on group with maximum correlation
library(broom)
sim_data %>% 
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(y ~ x, data = .)))


############################################
# CORRELATION IS NOT CAUSATION - OUTLIERS #
###########################################

# Correlation can be caused by outliers, as shown in this code

# simulate independent X, Y and standardize all except entry 23
set.seed(1985)
x <- rnorm(100,100,1)
y <- rnorm(100,84,1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

# plot shows the outlier
qplot(x, y, alpha = 0.5)

# outlier makes it appear there is correlation
cor(x,y)
# Without element 23
cor(x[-23], y[-23])

# With spearman correlation we can use rank instead to plot the elements
qplot(rank(x), rank(y))
cor(rank(x), rank(y))

# Spearman correlation with cor function by adding method = "spearman"
# Spearman computes the correlation based on the ranks of the values
cor(x, y, method = "spearman")

# You can learn more about methods of robust fitting for linear models in this book
# Robust Statistics: Edition 2
# Peter J. Huber, Elvizio M. Ronchetti



##############################################################
# CORRELATION IS NOT CAUSATION - REVERSING CAUSE AND EFFECT #
#############################################################

# An example of this is claiming that tutoring makes students perform worse
# because they score lower than peers who are not scored
# Here the tutoring is not causing the low test, but the low test causes a 
# student to require tutoring

# A form of this claim was actually made in an op ed in the New York Times,
# titled "Parental Involvement is Overrated". Consider this quote from the article.
# "When we examine whether regular help with homework had a positive impact on
# children's academic performance, we were quite startled by what we found.
# Regardless of family social class, racial, or ethnic background, or child's grade
# level, consistent homework help almost never improved test scores or grades.
# Even more surprising to us was that when parents regularly helped with homework,
# kids usually performed worse."

# We can create an example of cause and effect reversal using the father and son
# heights set in the Galton data, when father and son were reversed in the
# regression, the model was technically correct. The estimates and p-values were
# obtained correctly as well. What was incorrect was the interpretation of the
# model.

# we do get a statistically significant result. You can see that with this simple code.
# This model fits the data very well. However, if we look at the mathematical 
# formulation of the model, it could easily be incorrectly interpreted as to suggest
# that the son being tall caused the father to be tall. But given what we know about
# genetics and biology, we know it's the other way around.

# Cause and effect reversal using son heights to predict father heights
library(HistData)
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>% 
  do(tidy(lm(father ~ son, data = .)))
# The model is technically correct.
# The estimates and p-value were obtained correctly as well.
# What is wrong here is simply the interpretation.
# Tall sons likely have tall fathers, but don't cause fathers to be tall


###############################################
# CORRELATION IS NOT CAUSATION - CONFOUNDERS #
##############################################

# Confounders are perhaps the most common reason for associations to be misinterpreted
# If X and Y are correlated, we call Z a confounder if changes in Z causes changes
# in both X and Y.

# In the baseball example we saw that home runs was a confounder that resulted in 
# higher correlation than expected when studying the relationship between bases on
# balls and runs. In some cases we can use linear models to account for confounders, as we did in
# the baseball example/

# Incorrect interpretations due to confounders are ubiquitous in the lay press
# They are sometimes hard to detect.

# We examined admission data from UC Berkeley majors from 1973 that showed that
# more men were being admitted than women. 44% men were admitted compared to
# 30% women. Here's the data:
library(dslabs)
data(admissions)      # UC Berkeley admissions data
  admissions

# percent men and women accepted
admissions %>% group_by(gender) %>% 
  summarize(percentage = 
              round(sum(admitted*applicants)/sum(applicants),1))

# test whether gender and admission are independent
# Chi-squared test clearly rejects null hypothesis that admissions
# and gender are independent. The p-value is very small
admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))

# percent admissions by major gives a paradoxical result
# 4 out of the 6 majors favour women
admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)

# The paradox is that when analysing admissions v gender suggests
# dependence, but this seems to disappear when analysing by major

# Why does this happen?  It can happen when an uncounted confounder
# is present and driving most of the variability.
# We can see this with 3 variables
# X = 1 for male, 0 otherwise
# Y = 1 for admitted, 0 otherwise
# Z is related to selectiveness of entering the major

# A gender bias claim would say that Pr(Y=1|X=x) being higher when
# X = 1 (male)

# However Z is an important confounder because the more selective
# a major the lower Y, the probability of entry, will be

# plot total percent admitted to major versus percent women applicants
admissions %>% 
  group_by(major) %>% 
  summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
            percent_women_applicants = sum(applicants * (gender=="women")) /
              sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

# plot number of applicants admitted and not
admissions %>%
  mutate(yes = round(admitted/100*applicants), no = applicants - yes) %>%
  select(-applicants, -admitted) %>%
  gather(admission, number_of_students, -c("major", "gender")) %>%
  ggplot(aes(gender, number_of_students, fill = admission)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(. ~ major)
# Same but stacked
admissions %>% 
  mutate(percent_admitted = admitted * applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

# condition on major (confounder Z) and then look at differences
# The effect goes away
admissions %>% ggplot(aes(major, admitted,
                        col = gender, size = applicants)) + 
              geom_point()

# If we first stratify by major, compute the difference and then
# average the difference by major we see that the percent difference
# is actually quite small
admissions %>%  group_by(gender) %>% 
  summarize(average = mean(admitted))

# This is actually something called Simpson's Paradox


######################
# SIMPSON'S PARADOX #
#####################

# We call it a paradox because we see the correlation sign of the
# correlatin flip when compute it on the entire population versus
# on a specific strata

# We can use a very illustrative, simulated example

# Picture a scatterplot negatively correlated (top left to bottom right)
# It has correlation of -0.608

# Once we stratify by Z, the unknown confounder, we see the negative
# correlation split into 5 banded groups, retaining the shape of the 
# negative correlation but all are positively correlated through the 
# Z stratas. This is simpson's paradox



# ASSESSMENT - COUNFOUNDING
library(dslabs)
data("research_funding_rates")
research_funding_rates

# Construct two by two tables
two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)

# Calculate percentages of men and women admitted
totals %>% 
  mutate(men = round(men/sum(men)*100, 1),
      women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  # pull(men)
  pull(women)


# Run a chi-squared test on the two-by-two table to determine whether
# the difference in the two success rates is significant. (You
# can use tidy() to turn the output of chisq.test() into a data
# frame as well.)
# What is the p-value of the difference in funding rate?
two_by_two %>% select(-awarded) %>% 
    chisq.test() %>% 
    tidy() %>% 
    pull(p.value)


# There may be an association between gender and funding. But can we 
# infer causation here? Is gender bias causing this observed difference?
# The response to the original paper claims that what we see here is
# similar to the UC Berkeley admissions example. Specifically they 
# state that this "could be a prime example of Simpson's paradox; if
# a higher percentage of women apply for grants in more competitive
# scientific disciplines, then an analysis across all disciplines 
# could incorrectly show 'evidence' of gender inequality."

# To settle this dispute, use this dataset with number of applications, 
# awards, and success rate for each gender:
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

# To check if this is a case of Simpson's paradox, plot the
# success rates versus disciplines, which have been ordered by
# overall success, with colors to denote the genders and size 
# to denote the number of applications.
dat %>% 
  ggplot(aes(discipline, success, size = applications, color = gender)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point()

