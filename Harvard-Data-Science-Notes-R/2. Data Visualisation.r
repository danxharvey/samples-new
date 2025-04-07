##################################
# INTRODUCTION TO DISTRIBUTIONS #
#################################

# Learning to summarise lists or vectors
# These become distributions

# We will work with two data types:
# categoricals (e.g. sex):
#             ordinals (can be ordered)
#             non-ordinals (not ordered)
# numericals: continuous (any values e.g. height)
#             discrete (finite e.g. population size)

# Example
# number of packs of cigarettes smoked per day = ordinal
# number of cigarettes smoked = numerical

# Working with a distribution
library(dslabs)
data(heights)
names(heights)    # Name of columns in dataset
x <- heights$height # Assign all heights to x 
x <- c(x)         # Assign to a vector
length(unique(x)) # Number of unique values
tab <- table(x)   # Gives frequency of each unique value
tab <- table(heights$height)
sum(tab==1)       # See how many values are reported once

# Proportions of categories in dataset
prop.table(table(heights$sex))

# CDF - Cumulative Distribution Function
# Proportion of values <= a for all a
# F(a) = Pr(x<=a)

# Normal Distribution
# Create bins (e.g. 64.1, 64.3 both considered 64)

# Smooth Density Plots
# Key principle is understanding the concept that your sample
# is considered to just be part of a much larger sample
# E.g. male heights becomes a subset of global male heights
# Has affect of making smaller bins to smooth histogram
# Curve is calculated by frequency not count
# Smoothness of curves controlled within ggplot function
# Smooth density based on assumptions and choices you make
# Norm Dist is based on fact alone
# Choose smoothness that you can justify

# Area Under Curve (AUC) = 1
# Bands are just proportions of this
# Proprotion of selected bands = proportion of area in AUC

########################
# NORMAL DISTRIBUTION #
#######################

# AKA Bell Curve or Gaussian Distribution
# average <- sum(x) / length(x)
# sd <- sqrt( sum( (x-average)^2) / length(x))

index <- heights$sex == "Male"  # Defines male only dataset
x <- heights$height[index]      # Only looks at Male
average <- mean(x)
sd <- sd(x)
c(average=average, sd=sd)       # display formatted results

# Standard units
# Tells how far a value is away from the mean, in s.deviations
# z = (x - average) / sd        # z = 0 = average height
z <- scale(x)   # gives standard units

# How many men are within 2 sd of the average?
mean(abs(z) < 1)    # Take absolute to work both sides of avg

# Proportion of men between 2 heights?
x <- heights$height[heights$sex == "Male"]
mean(x>69 & x<=72)

# Using pnorm to work with CDF
# Proportion of men between 69 and 72 inches
x <- heights$height[heights$sex=="Male"]
avg <- mean(x)
stdev <- sd(x)
pnorm(72, avg, stdev) - pnorm(69, avg, stdev)
# This is quite accurate, only 3% difference

# It is not so accurate at the tails or extremes though
x <- heights$height[heights$sex == "Male"]
exact <- mean(x > 79 & x <= 81)
avg <- mean(x)
stdev <- sd(x)
approx <- pnorm(81, avg, stdev) - pnorm(79, avg, stdev)
exact/approx    # 60% difference

# With the given parameters, what % are over 7ft?
avg <- 69
stdev <- 3
height <- 84
p<- 1 - pnorm(height, avg, stdev)   # 1 - all under 7ft
# NUmber of men in 1 billion over 7ft (287)
round(p*1000000000)

##########
# QNORM #
#########

# In heights example, 50% of data below 69.5
mean(x <= 69.5)     # 69 bucket goes 68.5 -> 69.5
# if p =0.5 then q = 69.5     # q = quantiles

# If the quantiles for the data match the quantiles for the
# normal distribution it must be because the data matches
# the normal distribution

# Set up quantiles
p <- seq(0.05, 0.95, 0.05)
x <- heights$height[heights$sex == "Male"]
observed_quantiles <- quantile(x, p)

# Obtain theoretical normal distribution quantiles
theoretical_quantiles <- qnorm(p, mean=mean(x), sd=sd(x))

# Plot them to see if they match
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# To make code simpler we can use standard units
p <- seq(0.05, 0.95, 0.05)
x <- heights$height[heights$sex == "Male"]
z <- scale(x)   # gives standard units
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)
# also known as q-q plots

################
# PERCENTILES #
###############

# Percentiles are just quantiles defined as 1%
# 25th percentiles is <= 25%
# Median = median = 50%   # In Norm Dist these are same
# 25/50/75 percentiles are called quartiles


#############
# BOXPLOTS #
############

# Using the murders dataset we can see that the norm dist
# is not appropriate.  So we use box plots instead
# 25th - 75th percentiles are inter-quartile range
# Outside of these are outliers shown as dots
# Line for the median

# Define the vectors
male <- heights$height[heights$sex=="Male"]
female <- heights$height[heights$sex=="Female"]
length(male)
length(female)
female_percentiles <- quantile(female, seq(0.1, 0.9, 0.2))
male_percentiles <- quantile(male, seq(0.1, 0.9, 0.2))
df <- data.frame(female=c(female_percentiles), male=c(male_percentiles))


# Using Galton's data
install.packages("HistData")
library(HistData)
data(Galton)
x <- Galton$child
mean(x)   # Average
median(x) # 50th percentile
sd(x)     # Standard deviation
mad(x)    # Median absolute deviation (Median(Y - Median Y))

# Adding an error to the dataset
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10
mean(x_with_error) - mean(x)
sd(x_with_error) - sd(x)
median(x_with_error) - median(x)
mad(x_with_error) - mad(x)
# Whilst median and mad are not changed a boxplot would
# show an outlier

# Simple function to highlight impact of an error (k)
x <- Galton$child
error_avg <- function(k){
  x[1] <- k
  mean(x)
}
error_avg(10000)
error_avg(-10000)


###########
# GGPLOT #
##########
install.packages("tidyverse")
library(ggplot2)    # gg = Grammar of Graphics

# Limitation of ggplot is that is works with data tables
# Rows have to be observations
# Columns have to be variables
library(dslabs)
data("murders")

# Components:
  # 1. The US murders table is being summarised (data component)
  # 2. This is a scatter plot (geometry component)
      # Other components include bar, histogram, smooth density
      # q-q and box plots
  # 3. Mappings (aesthetic mapping component)
      # x-axis displays population size
      # y-axis displays total number of murders
      # text is used to identify the states
      # colours are used to denote the four regions
      # both on log scales (scale component)


# CREATING A NEW PLOT
ggplot(data = murders)  # Associates dataset with plotting object
p <- murders %>% ggplot()       # Using pipes
class(p)    # p is a ggplot
print(p)    # prints blank plot
p           # prints blank plot

# General format for ggplot is like this
# DATA %>% ggplot() + LAYER1 + LAYER2 + ...

# Usually first layer is geometry
# ?geom_point for aethetics help
murders %>% ggplot() +
  geom_point(aes(x=population/10^6, y=total_murders), size = 3) +
# aes recognises column names, most functions don't
  geom_text(aes(population/10^6, total_murders, label=abb), nudge_x = 1)

# Check arguments for ggplot to make code more efficient
args(ggplot)

# Add global labels to make it more readable
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) + geom_text(nudge_x = 1.5)

# Cool trick to override global with local for flexibility
p + geom_point(size = 3) +
  geom_text(aes(x=10, y=800, label="Hello There!"), nudge_x = 1.5)

# Transforming for log scale
p + geom_point(size = 3) + geom_text(nudge_x = 0.075) +
  scale_x_continuous(trans ="log10") +
  scale_y_continuous(trans ="log10")

# Load extra packages
install.packages("ggthemes")
install.packages("ggrepel")
library(ggthemes)
library(ggrepel)

# Use dplyr skills to add the national murder rate
r <- murders %>% summarize(rate = sum(total)/sum(population)*10^6) %>% .$rate
# Becomes
p + 
  # Putting the abline here makes it the lowest layer for display (bottom)
  # a=slope, b=intercept default 1 (correlated slope), lty=2 is dashed line
  geom_abline(intercept = log10(r), lty=2, color="darkgrey") +   
  geom_point(aes(col=region), size = 3) +
  geom_text_repel() +       # Prevents labels sitting on each other
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +   # Capitalise R for region
  theme_economist()
# For final razzmatazz on the chart we needed two themes from outside
# of ggplot - ggthemes and ggrepel


# Swanky theme from Harvard
library(ggplot2)
ds_theme_set()
qplot(hp, mpg, data=mtcars, color=am, facets=gear~cyl, 
      main="Scatterplots of MPG vs. Horsepower", 
      xlab="Horsepower", ylab="Miles per Gallon")


##############################
# HISTOGRAM OF MALE HEIGHTS #
#############################

# Set up basic histogram object
p <- heights %>% filter(sex=="Male") %>%
    ggplot(aes(x=height)) +
    geom_histogram(binwidth=1) +  # Selects column category width
    xlab("Male height in inchnes") +
    ggtitle("Histogram")

# Blue bars
p1 <- heights %>% filter(sex=="Male") %>%
    ggplot(aes(x=height)) +
    geom_histogram(binwidth=1, fill="blue", col="black") +
    xlab("Male height in inches") +
    ggtitle("Filled Histogram")

# Smooth densities
p2 <- heights %>% filter(sex=="Male") %>%
    ggplot(aes(x=height)) +
    geom_density(fill="blue") +
    xlab("Male height in inches") +
    ggtitle("Smooth Densities")

# Q-Q plot (defaults mean 0, sd 1)
# Set up params as per geom_qq help file to define mean and sd
params <- heights %>% filter(sex=="Male") %>%
    summarize(mean=mean(height), sd=sd(height))

p3 <- heights %>% filter(sex=="Male") %>%
    ggplot(aes(sample=height)) +            # x var is now sample
    geom_qq(dparams=params) +
    geom_abline() +
    xlab("Male height in inches") +
    ggtitle("Q-Q Plot")

# Q-Q plot (defaults mean 0, sd 1)
# Represent in standard units to save calculating mean and sd
p4 <- heights %>% filter(sex=="Male") %>%
    ggplot(aes(sample=scale(height))) +        # scale() function
    geom_qq() +
    geom_abline()

# Display charts next to each other
# Set up chart objects as per above
install.packages("gridExtra")
library(gridExtra)

# Display a 3 column chart object
grid.arrange(p1, p2, p3, ncol=3)


# geom_label simple
murders %>% ggplot(aes(population, total, label = abb, color=region)) +
  geom_label()

# 2 Smooth Denisty plots
heights %>% ggplot(aes(height, group=sex)) + geom_density()

# Same with coloured lines
heights %>% ggplot(aes(height, color = sex)) + geom_density()

# Same but fill under lines
heights %>% ggplot(aes(height, fill = sex)) + geom_density(alpha=.2)
# alpha is opacity of fill


################################
# SUMMARISING DATA WITH DPLYR #
################################
library(tidyverse)
library(dslabs)
data(heights)

# Compute average and standard deviation for males
# Summarize stores as a data.frame
s <- heights %>% filter(sex=="Male") %>%
    summarize(mean=mean(height), standard_deviation=sd(height))
# Access columns
s$mean
s$standard_deviation

# We are not just limited to those calculations
# Only use functions that return a single value
heights %>% filter(sex=="Male") %>%
    summarize(median=median(height),
              minimum=min(height),
              maximum=max(height))

# Reminder of how we added the murder rate column with DPLYR
data(murders)
murders <- murders %>% mutate(murder_rate = total/population*100000)
summarize(murders, mean(murder_rate))
# This is average of state level - incorrect

# To compute US murder rate as a whole we need to calculate totals
us_murder_rate <- murders %>%
    summarize(rate = sum(total) / sum (population)*100000)
us_murder_rate

# We can't use this if we need just a numeric object as summarize
# like most DPLYR functions returns data.frames
class(us_murder_rate)
a <- as.numeric(us_murder_rate)   # You could convert
class(a)

# Using dot method to be efficient
us_murder_rate <- murders %>%
    summarize(rate = sum(total) / sum (population)*100000) %>%
    .$rate    # dot converts rate data to a numeric
class(us_murder_rate)


##############
# GROUPINGS #
#############

# Create a group data frame
ds(heights)
heights %>% group_by(sex)
# Think of a group data frame as a bunch of stacked tables with totals
# as the rows.  So if we summarise this now we get
heights %>% group_by(sex) %>%
    summarize(average=mean(height), stan_dev=sd(height))

# Summarise murder rate by region in same way
murders %>% group_by(region) %>%
    summarize(median_rate = median(murder_rate))


########################
# SORTING DATA TABLES #
#######################

# Arrange function in DPLYR is very useful for tables
murders %>% arrange(population) %>% head()
murders %>% arrange(murder_rate) %>% head()
murders %>% arrange(desc(murder_rate)) %>% head()   # descending order
# Multiple orders
murders %>% arrange(region, murder_rate) %>% head()
# top_n instead of head() - unordered
murders %>% top_n(10, murder_rate)  # murder_rate optional param
# top_n ordered
murders %>% arrange(desc(murder_rate)) %>% top_n(10)


# EXERCISES
install.packages("NHANES")
library(dplyr)
library(NHANES)
data(NHANES)

## filter by gender and age
tab <- NHANES %>% filter(Gender=="female" & AgeDecade == " 20-29")
tab

# Blood Pressure 2
ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% 
      summarize(average = mean(BPSysAve, na.rm = TRUE),
                  standard_deviation = sd(BPSysAve, na.rm=TRUE))

# Blood Pressure 3
ref_avg <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>% .$average

# Min and Max with na.rm (remove NA)
## complete the line
NHANES %>%
  filter(AgeDecade == " 20-29"  & Gender == "female") %>%
      summarize(min=min(BPSysAve, na.rm=TRUE),
                max=max(BPSysAve, na.rm=TRUE))

# Mean and SD for female
NHANES %>%
  filter(Gender == "female") %>% group_by(AgeDecade) %>%
      summarize(average=mean(BPSysAve, na.rm=TRUE), 
               standard_deviation=sd(BPSysAve, na.rm=TRUE))

# Mean and SD for male
NHANES %>%
  filter(Gender=="male") %>% group_by(AgeDecade) %>% 
      summarize(average=mean(BPSysAve, na.rm=TRUE),
                standard_deviation=sd(BPSysAve, na.rm=TRUE))

# Combine into single line of code
NHANES %>% group_by(AgeDecade, Gender) %>%
      summarize(average=mean(BPSysAve, na.rm=TRUE), 
                standard_deviation=sd(BPSysAve, na.rm=TRUE))

# Arrange them to be more readable
NHANES %>% filter(Gender=="male" & AgeDecade==" 40-49") %>%
  group_by(Race1) %>% 
      summarize(average=mean(BPSysAve, na.rm=TRUE), 
                standard_deviation=sd(BPSysAve, na.rm=TRUE)) %>% 
      arrange(average)



########################################
# WORLD HEALTH DATA FROM GAPFINDER.OR #
#######################################

# Using data to disprove sensationalist media reports

# Is it fair to say that the west is richer?
# Has income inequality worsened over the last 40 years?
library(dslabs)
data(gapminder)
head(gapminder)

# Who has higher mortality rates - Turkey or Sri Lanka?
gapminder %>% filter(year==2015 & country %in% c("Sri Lanka", "Turkey")) %>%
    select(country, infant_mortality)

# Our misconceptions stem from a view that the world is split into
# two - the west and the rest
# Let's look at a scatterplot of life expectancy v fertility rates
ds_theme_set()
filter(gapminder, year==1962) %>%
    ggplot(aes(fertility, life_expectancy, color=continent)) +
    geom_point()
# Results seem to point to 2 groups
# High life expectancy & low fertility and vice versa
# In 1962 this world view appeared to be true
# Is it still true?

# We could plot the 2012 data as above to see but it is
# preferable to see them side by side, called faceting
# Use facet_grid() with up to 2 variables separated by ~
ds_theme_set()
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, color=continent)) +
  geom_point() +
  facet_grid(continent~year)
# The data has now been 'stratified'

# To do this without continent we use .
ds_theme_set()
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, color=continent)) +
  geom_point() +
  facet_grid(.~year)
# The world has changed and now all are moving to the
# Western cluster, especially Asia

# Now let's compare across many years, we must add code as
# facet_grid default is to display them in 1 row
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
    filter(year %in% years & continent %in% continents) %>%
    ggplot(aes(fertility, life_expectancy, color=continent)) +
    geom_point() + 
    facet_wrap(.~year)
# facet keeps the axis values fixed across all plots making
# comparisons much easier


# TIME SERIES PLOTS
# US fertility rates
gapminder %>% filter(country == "United States") %>%
    ggplot(aes(year, fertility)) +
    geom_line()

# Compare 2 countries
# Without giving extra info ggplot makes a mad graph
# This is actually cool as it shows convergence (or divergence)
countries <- c("Germany", "South Korea")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility)) +
  geom_line()

# To get 2 lines we need to add group=country to aes()
# Replace group=country with col= as ggplot assigns
# colours and labels to coloured lines
countries <- c("Germany", "South Korea")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, col=country)) +
  geom_line()
# Labels are preferred over legends

# Define a data.frame with locations of labels (data.frame is a table)
countries <- c("South Korea", "Germany")
labels <-data.frame(country=countries, x=c(1975,1965), y=c(60, 72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col=country)) +
  geom_line() +
  geom_text(data=labels, aes(x, y, label=country), size=5) +  # labels dataframe as data
  theme(legend.position="none")


# INCOME DISTRIBUTIONS
# Convert GDP into $$$ per day.  Less than $2/day is classed as poverty
# Data is already adjusted for inflation and does not reflect intra-country variation
gapminder <- gapminder %>%
      mutate(dollars_per_day = gdp/population/365)

# Simple histogram
past_year <- 1970
gapminder %>% filter(year == past_year & !is.na(gdp)) %>%  # exclude NA
    ggplot(aes(dollars_per_day)) +
    geom_histogram(binwidth = 1, color="black")
# Histogram is stacked to < than $10 per day
# Might be more informative to look at buckets of 1, 2, 4, 8, 16, 32
# This introduces log transformations as these change multiplicative changes
# into additive ones using base2, every time a value doubles, log increases by 1
past_year <- 1970
gapminder %>% filter(year == past_year & !is.na(gdp)) %>%  # exclude NA
  ggplot(aes(log2(dollars_per_day))) +        # log2 transform in aes()
  geom_histogram(binwidth = 1, color="black")
# Now we see 2 clear bumps, called modes, this is a modal distribution
# There are local modes and the distribution has multiple modes
# This bimodality is consistent with the dichotomous world view

# PAUSE

# Why did we choose log2?  It is easier to compute 2, 4, 16, etc..
# or 10, 100, 1000, in our heads.
# Never use the natural log E as this is impossible to easily understand
# We used log2 because it suited the integer range for dollars per day
# We use log to make skewed distributions less skewed and easier to interpret
# log10 makes more sense for population scales

# There are two ways to make log transformation
# Log the data - easier to interpret intermediate values in the scale (1.5 is halfway)
# Log the axis - axis reflects original numbers and is easy to see what values are

# Code above is log(data), the code below is log(scale) as we have seen before
past_year <- 1970
gapminder %>% filter(year == past_year & !is.na(gdp)) %>%  # exclude NA
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color="black") +
  scale_x_continuous(trans="log2")      # The values are still in $$$/day not logs


###################
# STRATIFICATION #
##################

# STACKING BOX PLOTS
levels(gapminder$region)
length(levels(gapminder$region))

p <- gapminder %>% filter(year == past_year & !is.na(gdp)) %>%  # exclude NA
  ggplot(aes(region, dollars_per_day)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1))  # Rotate label through 90
# hjust justifies the text next to the axis

# Learning reorder()
# Create an arbitrary factor
fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac)
# Let's assign value to the factors by creating a new vector
value <- c(10, 11, 12, 6, 4)
# Now order by the mean of the factors
fac <- reorder(fac, value, FUN = mean)  # Reorder fac by value using function mean
levels(fac)

# Back to our box plot, reorder the data as per above
p <- gapminder %>% filter(year == past_year & !is.na(gdp)) %>%  # exclude NA
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% # FUN capitals!
  ggplot(aes(region, dollars_per_day, fill=continent)) +  # continents by colour
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) + # Rotate label through 90
  xlab("")

# Make this more readable by adding a log transformation to the y axis
p <- gapminder %>% filter(year == past_year & !is.na(gdp)) %>%  # exclude NA
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% # FUN capitals!
  ggplot(aes(region, dollars_per_day, fill=continent)) +  # continents by colour
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) + # Rotate label through 90
  xlab("") + 
  scale_y_continuous(trans="log2")

# Last change we can make is to add data points.  This is not always done
# as adding data points can be confusing but as we don't have much data in this
# we can shot it for clarity by adding geom_point()
p <- gapminder %>% filter(year == past_year & !is.na(gdp)) %>%  # exclude NA
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% # FUN capitals!
  ggplot(aes(region, dollars_per_day, fill=continent)) +  # continents by colour
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=90, hjust=1)) + # Rotate label through 90
  xlab("") + 
  scale_y_continuous(trans="log2") +
  geom_point(show.legend = FALSE)


############################
# COMPARING DISTRIBUTIONS #
###########################

# So far we have established that the dataset shows 2 modes with a bimodal distribution
# related to rich and poor countries. Then by stratifying by box plots we see
# that the richer countries are in the west

# To compare across time we set up a new vector
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

# We start by confirming bimodality observed in 1970
p <- gapminder %>% filter(year==past_year & !is.na(gdp)) %>%
    mutate(group = ifelse(region %in% west, "West", "Developing")) %>% # West & rest
    ggplot(aes(dollars_per_day)) +
    geom_histogram(binwidth=1, color="black") +
    scale_x_continuous(trans="log2") +
    facet_grid(.~group)

# Now we see how the data looks in 2010 by comparing side by side
past_year <- 1970
present_year <- 2010
p <- gapminder %>% filter(year %in% c(past_year, present_year) & !is.na(gdp)) %>%
    mutate(group = ifelse(region %in% west, "West", "Developong")) %>% # West & rest
    ggplot(aes(dollars_per_day)) +
    geom_histogram(binwidth=1, color="black") +
    scale_x_continuous(trans="log2") +
    facet_grid(year~group)

# We notice the disparity in data counts.  Two reasons are availability of data
# and creation of new countries (e.g. breakup of USSR)
# Remake the table using only countries that exist in both timeframes
country_list_1 <- gapminder %>%
                  filter(year==past_year & !is.na(gdp)) %>% .$country # pipe to country column
country_list_2 <- gapminder %>%
                  filter(year==present_year & !is.na(gdp)) %>% .$country # pipe to country column
country_list <- intersect(country_list_1, country_list_2) # Only use countries in both
# There is a better way using tidyverse but we haven't come to that yet
# country_list has 108 countries which is 86% of the total population
# so this is a reasonable representation of the entire world


# Make plot using only countries in both
p <- gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
    mutate(group = ifelse(region %in% west, "West", "Developong")) %>% # West & rest
    ggplot(aes(dollars_per_day)) +
    geom_histogram(binwidth=1, color="black") +
    scale_x_continuous(trans="log2") +
    facet_grid(year~group)

# To see the regions improving the most we can refer back to our boxplot code
p <- gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
    mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% # FUN capitals!
    ggplot() +  # continents by colour
    theme(axis.text.x = element_text(angle=90, hjust=1)) + # Rotate label through 90
    xlab("") + 
    scale_y_continuous(trans="log2") +
    geom_boxplot(aes(region, dollars_per_day, fill=continent)) +
    facet_grid(year~.)
   
# To ease the comparison we will place box plots within the same chart area
# This will give us an easy before and after presentation using ggplot
p <- gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
    mutate(region = reorder(region, dollars_per_day, FUN = median)) %>% # FUN capitals!
    ggplot() +  # continents by colour
    theme(axis.text.x = element_text(angle=90, hjust=1)) + # Rotate label through 90
    xlab("") + 
    scale_y_continuous(trans="log2") +
    geom_boxplot(aes(region, dollars_per_day, fill=factor(year)))  # Change is here
# We can make this even easier by showing ratios of improvement but this will be
# covered later once we learn the coding techniques


###############################
# SMOOTH DENSITY PLOTS AGAIN #
##############################

# We can make the above comparisons even easier to display by using smooth density plots
# We need to show that the gap closed by poor countries becoming richer, not vice versa
# We must display the information that preserves the number of countries in each group
# If we just use the number of countries in each group it looks like both groups are same
# We will use computed variables within geom_density() to give a proportional picture
# We simply multiply the y axis values by the size of the group
# There is a variable called count which can be used instead of the density value
# Achieved by using "..count.."
p <- gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
    mutate(group = ifelse(region %in% west, "West", "Developering")) %>% # FUN capitals!
    ggplot(aes(dollars_per_day, y=..count.., fill=group)) + # ..count.. is here
    scale_x_continuous(trans="log2") +
    geom_density(alpha=0.2) +
    facet_grid(year~.)

# To smooth it out we can add bw to the geom_density
p <- gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
    mutate(group = ifelse(region %in% west, "West", "Developering")) %>% # FUN capitals!
    ggplot(aes(dollars_per_day, y=..count.., fill=group)) + # ..count.. is here
    scale_x_continuous(trans="log2") +
    geom_density(alpha=0.2, bw=0.75) +    # bw = 0.75 was a smoothness trial and error value
    facet_grid(year~.)
# Notice how the developing countries have a larger area (more countries)


# From our earlier exploratory analysis we saw that most of the improvements were Asian
# We can alter the code slightly to display region using case_when
# This is useful for defining groups
# We can now update our groups vector
gapminder <- gapminder %>%
      mutate(group = case_when (
        .$region %in% west ~ "West",
        .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
        .$region %in% c("Caribbean", "Central America", "South America", "Latin America") ~ "Latin America",
        .$continent == "Africa" & .$region != "North Africa" ~ "Sub-Saharan Africa",
        TRUE ~ "Others"))

# Now we turn this group into a factor to control the order of the levels
gapminder <- gapminder %>%
      mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia",
                                              "Sub-Saharan Africa", "West")))
# We have manually set this specific order for a reason.  It will become clear when you see
# the plots
p <- gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
    ggplot(aes(dollars_per_day, y=..count.., fill=group)) + # ..count.. is here
    scale_x_continuous(trans="log2") +
    geom_density(alpha=0.2, bw=0.75) +    # bw = 0.75 was a smoothness trial and error value
    facet_grid(year~.)

# This isn't very clear so we introduce stacking
p <- gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
    ggplot(aes(dollars_per_day, y=..count.., fill=group)) + # ..count.. is here
    scale_x_continuous(trans="log2") +
    geom_density(alpha=0.2, bw=0.75, position="stack") +    # Stacking here
    facet_grid(year~.)

# What this plot doesn't show is population weighting as all points are given the same
# weighting. So China's figures are equal to Laos for example.
# We can correct for this by adding a weight function
p <- gapminder %>% filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
    group_by(year) %>%                                    # Apply weight calc here
      mutate(weight=population/sum(population)*2) %>%     # Apply weight calc here
    ungroup() %>%                                         # Apply weight calc here
    ggplot(aes(dollars_per_day, fill=group, weight=weight)) + # add weight here
    scale_x_continuous(trans="log2") +
    geom_density(alpha=0.2, bw=0.75, position="stack") +    # Stacking here
    facet_grid(year~.)


#######################
# ECOLOGICAL FALLACY #
######################

# We want to look at country child survival rates and average income
# Let's expand our regions using case_when
gapminder <- gapminder %>%
  mutate(group = case_when (
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asian",
    .$region %in% c("Caribbean", "Central America", "South America", "Latin America") ~ "Latin America",
    .$continent == "Africa" & .$region != "North Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

# Now compute the values that we are interested in
surv_income <- gapminder %>%
      filter(year %in% present_year & !is.na(gdp) &
              !is.na(infant_mortality) & !is.na(group)) %>%
      group_by(group) %>%
      summarize(income=sum(gdp)/sum(population)/365,
      infant_survival_rate = 1-sum(infant_mortality/1000*population)/sum(population))

# View results
surv_income %>% arrange(income)
# The relationship between average daily income and child mortality
# is almost perfectly linear

# Introducing the limit argument.  This allows us to fix an axis
# for comparisons against a static range on the plot
surv_income %>%
      ggplot(aes(income, infant_survival_rate, label=group, color=group)) +
      scale_x_continuous(trans="log2", limit=c(0.25, 150)) +
      scale_y_continuous(trans="logit", limit=c(0.875, .9981),
                        breaks = c(0.85, 0.9, 0.95, 0.99
                                   , 0.995, 0.998)) +
      geom_label(size=3,show.legend = FALSE)
# We also introduced the logistical function - logit()
# The logit transformation for a proportional rate p is defined as:
# f(p) = log(p/(1-p))
#   where the proportion p/(1-p) is called "the odds"
# The log makes this proportion symmetric.  If the rates are the same
# then the log odds are zero.  Fold increases/decreases turn
# positive or negative respectively
# The scale is useful when we want to highlight differences near
# 0 or 1, as per our child survival rates
# 90% survival is unacceptable.  99% is relatively good but we would
# prefer a survival rate of 99.9%

# The logit transform shows this clearly as:
# 99.9/0.1 = 999
# 99/1 = 99
# 90/10 = 9
# factors of 10 difference

# Back to our plot, do we conclude that all survival rates in
# sub-saharan africa < southern asia < pacific islands < etc...?
# Jumping to this conclusion based on a plot that only shows averages
# is called the "Ecological Fallacy"

# This linear relationship only holds true for the averages
# If we show the individual countries we see that countries
# within regions are very different


# Examples from assessment questions
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder_Africa_2010 <- gapminder %>% 
    filter(year==2010, continent=="Africa") %>% 
    mutate(dollars_per_day=gdp/population/365) %>% 
    filter(!is.na(dollars_per_day))
gapminder_Africa_2010 %>% 
    ggplot(aes(dollars_per_day, infant_mortality, color=region)) + 
    geom_point()library(dplyr)


gapminder_Africa_2010 <- gapminder %>% 
    filter(continent=="Africa", year %in% c(1970, 2010)) %>% 
    mutate(dollars_per_day=gdp/population/365) %>% 
    filter(!is.na(dollars_per_day), !is.na(infant_mortality))

gapminder_Africa_2010 %>% 
    ggplot(aes(dollars_per_day, infant_mortality, color=region, label=country)) + 
    geom_point() + 
    scale_x_continuous(trans="log2") + 
    geom_text() + 
    facet_grid(year~.)


###############################
# DATA VISUALISATION SUMMARY #
##############################

# Principles of visualisation are to follow methods that fit the way
# that human brains work

# Optimise and adapt graphs for the audience
# Exploratory charts for ourselves will be different to general audience

# PERCENTAGES
# Popular method is to use pie or doughnut charts
# These are sub-optimal as human brains have difficulty distinguishing
# between the angles presented
# It is better to write out the percentages or use bar charts
# Position and length > angles > area
# If you must use a pie chart always give the percentages as text

# KNOW WHEN TO INCLUDE ZERO
# When using bar plots it is dishonest to not start at zero
# Without zero relatively small differences can be made to look bigger
# Media and politicians often do this..
# Comparing 85, 88, 91 without zero looks like big differences
# If using position (and not length) it is acceptable to exlude zero
# For example box plots where the positions are important and are not
# relative to each other

# DO NOT DISTORT QUANTITIES
# Example was a bubble chart of US GDP compared to 4 other countries
# US(14.6) ~ China(5.7) ~ Japan(5.3) ~ Germany(3.3) ~ France(2.5)
# The US bubble was huge implying massive multiples of other countries
# This happened by setting the radius not the area of the bubble
# to be proportional to the quantity (US appeared 30x France)
# A bar plot would have been more honest
# NOTE: ggplot uses area by default

# ORDER BY A MEANINGFUL VALUE
# Order by meaningful values can make it easier to extract information
# Example murder rate - order by murder rate
# Example browser choice overs years - keep same order in each chart

# SOURCE OF PRINCIPLES
# Karl Broman - Creating effective figures and tables
# Peter Aldhous - Introduction to Data Visualisation (class notes)
