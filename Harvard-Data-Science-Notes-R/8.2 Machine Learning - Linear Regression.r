#####################################
# LINEAR REGRESSION FOR PREDICTION #
####################################

# Linear regression can be considered a machine learning algorithm.
# Although it can be too rigid to be useful, it works rather well for some
# challenges. It also serves as a baseline approach: if you can't beat it with 
# a more complex approach, you probably want to stick to linear regression.

# To link regression with ML we will re-formulate Galton's heights dataset
# as a continuous outcome
library(HistData)
galton_heights <- GaltonFamilies %>%
                    filter(childNum == 1 & gender == "male") %>%
                    select(father, childHeight) %>%
                    rename(son = childHeight)

# Task: ML to predict son's height Y using father's height X

# Generate test and train sets
install.packages("createDataPartition")
library(createDataPartition)
library(caret)
y <- galton_heights$son
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

# First if we ignore the fathers heights we guess the average of the sons heights
avg <- mean(train_set$son)  # This is our Yhat

# Our squared loss is 6.8
mean((avg-test_set$son)^2)  # (Yhat - Y)^2

# To improve we remember that in our regression course we saw that the heights
# followed a bivariate normal distribution (xy pair)
# This means that our required conditional expectation is equal to the
# regression line:  f(x) = E(Y|X=x) = B0 + B1x
# We also introduced least square estimate (LSE) as a method for estimating
# the slope and intercept
# To get a fitted model based on this:
fit <- lm(son~father, data=train_set)
fit$coef    # intercept 36.9 and slope 0.49

# This makes our estimate of the conditional expectation to be
# f_hat(x) = 36.9 + 0.49x
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)
# Our squared loss is now 5.04


# PREDICT FUNCTION
# This is very useful for machine learning

# It takes a fitted object from functions such as lm, glm and a data frame with
# the new predictors for which you want to predict and returns a prediction
# This streamlines our formulae above like this:
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)
# It returns the same value in a much simpler way
# Note it does not always return objects of the same type (see helpfile)
# ?predict.lm and ?predict.glm
# also ?predict.knn (future reference)


# COMPREHENSION CHECK
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# We will build 100 linear models using the data above and calculate the mean
# and standard deviation of the combined models. First, set the seed to 1 again
# (make sure to use sample.kind="Rounding" if your R is version 3.6 or later). 
# Then, within a replicate() loop, (1) partition the dataset into test and
# training sets with p = 0.5 and using dat$y to generate your indices, 
# (2) train a linear model predicting y from x, 
# (3) generate predictions on the test set, and
# (4) calculate the RMSE of that model. Then, report the mean and standard 
# deviation (SD) of the RMSEs from all 100 models.

# Set seed
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

# Replicate n times through a function
rmse <- replicate(n, {
  test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y~x, data=train_set)
  y_hat <- predict(fit, test_set)       # Y_hat
  sqrt(mean((y_hat - test_set$y)^2))    # RMSE
})

mean(rmse)
sd(rmse)


# Q2 Repeat the above but using a larger dataset
# Write a function that takes a size n, then 
# (1) builds a dataset using the code provided at the top of Q1 
# but with n observations instead of 100 and without the set.seed(1),
# (2) runs the replicate() loop that you wrote to answer Q1, which builds 
# 100 linear models and returns a vector of RMSEs, and
# (3) calculates the mean and standard deviation of the 100 RMSEs.

# (1)
f <- function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  # (2)
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y~x, data=train_set)
    y_hat <- predict(fit, test_set)       # Y_hat
    sqrt(mean((y_hat - test_set$y)^2))    # RMSE
  })
  
  c(mean(rmse), sd(rmse))
}
# (3)
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- c(100, 500, 1000, 5000, 10000)
sapply(n, f)



# Q3 Now repeat the exercise from Q1, this time making the correlation
# between x and y larger, as in the following code:
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

# Replicate n times through a function
rmse <- replicate(n, {
  test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y~x, data=train_set)
  y_hat <- predict(fit, test_set)       # Y_hat
  sqrt(mean((y_hat - test_set$y)^2))    # RMSE
})

mean(rmse)
sd(rmse)



# Q6 Note that y is correlated with both x_1 and x_2 but the two predictors 
# are independent of each other, as seen by cor(dat)
# Set the seed to 1, then use the caret package to partition into test and
# training sets with p = 0.5. Compare the RMSE when using just x_1, just x_2
# and both x_1 and x_2. Train a single linear model for each (not 100 like
# in the previous questions).
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
# Check correlation
cor(dat)

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y~x_1, data=train_set)
y_hat <- predict(fit, test_set)       # Y_hat
sqrt(mean((y_hat - test_set$y)^2))    # RMSE

fit <- lm(y~x_2, data=train_set)
y_hat <- predict(fit, test_set)       # Y_hat
sqrt(mean((y_hat - test_set$y)^2))    # RMSE

fit <- lm(y~x_1+x_2, data=train_set)
y_hat <- predict(fit, test_set)       # Y_hat
sqrt(mean((y_hat - test_set$y)^2))    # RMSE



# Q8 - Repeat Q6 but make x_1 and x_2 highly correlated
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

# Check correlation
cor(dat)

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(dat$y, times=1, p=0.5, list=FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y~x_1, data=train_set)
y_hat <- predict(fit, test_set)       # Y_hat
sqrt(mean((y_hat - test_set$y)^2))    # RMSE

fit <- lm(y~x_2, data=train_set)
y_hat <- predict(fit, test_set)       # Y_hat
sqrt(mean((y_hat - test_set$y)^2))    # RMSE

fit <- lm(y~x_1+x_2, data=train_set)
y_hat <- predict(fit, test_set)       # Y_hat
sqrt(mean((y_hat - test_set$y)^2))    # RMSE



# REGRESSION FOR CATEGORIAL DATA
library(dslabs)
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list=FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# Outcome Y = 1 for females
# Outcome Y = 0 for males
# where X = height

# We are interested in the conditional probability of being female given
# the height:  Pr(Y=1|X=x)

# What is the conditional probability of being female if you are 66 inch tall?

# In our dataset we can estimate this by rounding to the nearest inch
# and computing the average for these values
train_set %>% filter(round(height)==66) %>% summarise(mean(sex=="Female"))
# We see that the conditional probability is approx 24%

# Looking at a plot
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()

# As the results look close to linear we will try regression
# p(x) = Pr(Y=1|X=x) = B0 + B1x

# Convert the zeros and 1s to factor and estimate the Least Squares
lm_fit <- mutate(train_set, y = as.numeric(sex=="Female")) %>%
  lm(y~height, data = .)
# Calculate probability
p_hat <- predict(lm_fit, test_set)
# Add prediction rule, female is > 50%
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
# Check results in confusion matrix
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]


# LOGISTIC REGRESSION

# Logistic regression is an extension of linear regression that assures that
# the estimate of conditional probability  Pr(Y=1|X=x)  is between 0 and 1. 
# This approach makes use of the logistic transformation: 
# g(p)=log(p/(1???p)) 

# With logistic regression, we model the conditional probability directly with:
# g{Pr(Y=1|X=x)}=??0+??1x 

# Note that with this model, we can no longer use least squares.
# Instead we compute the maximum likelihood estimate (MLE). 

# In R, we can fit the logistic regression model with the function glm()
# (generalized linear models). If we want to compute the conditional probabilities, 
# we want type="response" since the default is to return the logistic
# transformed values.
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

# We see that the range is between -0.4 and 1.12, probabilities should be 0 to 1
range(p_hat)

# fit logistic regression model
glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial")

# Calculate new conditional probabilities
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

# Calculate and plot logistic curve
tmp <- heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) 
logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
tmp %>% 
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)

# Calculate confusion matrix to see slight improvement
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]


# CASE STUDY - Is it a 2 or a 7?

# In this case study we apply logistic regression to classify whether a digit
# is two or seven. We are interested in estimating a conditional probability 
# that depends on two variables:
# g{p(x1,x2} = g{Pr(Y=1|X1=x1,X2=x2)} = ??0 + ??1x1 + ??2x2 

# Through this case, we know that logistic regression forces our estimates to
# be a plane and our boundary to be a line. This implies that a logistic
# regression approach has no chance of capturing the non-linear nature of 
# the true  p(x1,x2) . Therefore, we need other more flexible methods that
# permit other shapes.

# Download sample dataset of 1000 digits from mnist library
data("mnist_27")

# See the digits we'll be analysing
mnist <- read_mnist()
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

# Plot the data points for initial analysis
data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

# Plot how a different pair of 2 & 7 look
is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")
tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})
tmp <- Reduce(rbind, tmp)
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)
# This will be quite challenging


# Let's start with logistic regression
# The conditional probability of being a seven given the two predictors
# x1 and x2 will be a linear function of x1 and x2 after the logistic transform

# p(x1, x2) = Pr(Y=1|X1=x1, X2=x2)
#           = g^-1(B0 + b1x1 + b1x2)
# where g^-1 is the inverse of the logistic function:
# g^-1(x) = exp(x)/{1+exp(x)}

# We can fit it in R using the glm function
fit <- glm(y~x_1 + x_2, data=mnist_27$train, family="binomial")

# Build a conditional probability rule, > 0.5 predict a 7, else a 2
p_hat <- predict(fit, newdata=mnist_27$test)
y_hat <- factor(ifelse(p_hat > 0.5, 7, 2))
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]
# Accuray = 0.75

# Access and plot the true conditional probability from the full master
# dataset that Harvard have access to, for learning purposes only
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) + geom_raster() +
  # Improve plot with this code added for conditional probs > 0.5
  scale_fill_gradientn(colors=c("#F8766D", "white", "#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black")

# To see our version of the data
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")

# The maths forces the true_p to be a straight line,
# as opposed to our curved estimate
p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

# Plot the line against the data points to see where it went wrong
p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)



# COMPREHENSION CHECK

# set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

# Note that we have defined a variable x that is predictive of a binary 
# outcome y:
dat$train %>% ggplot(aes(x, color = y)) + geom_density()



##################################################
# SMOOTIHG / CURVE FITTING / LOW PASS FILTERING #
#################################################

# Smoothing is a very powerful technique used all across data analysis.
# It is designed to detect trends in the presence of noisy data in cases
# in which the shape of the trend is unknown. 

# The concepts behind smoothing techniques are extremely useful in
# machine learning because conditional expectations/probabilities can be
# thought of as trends of unknown shapes that we need to estimate in
# the presence of uncertainty.
library(tidyverse)
library(dslabs)
data("polls_2008")
qplot(day, margin, data = polls_2008)

# When observing a poll do not think of it as a forecasting problem
# We are interested in the shape of the trend
# The true preference of the electorate is f(x) but this comes with an error
# Yi = f(xi) + Ei
qplot(day, margin, data = polls_2008) + geom_smooth()


##############################
# BIN SMOOTHING AND KERNELS #
#############################

# The general idea of smoothing is to group data points into strata in
# which the value of f(x) can be assumed to be constant. We can make
# this assumption because we think f(x) changes slowly and, as a result,
# f(x) is almost constant in small windows of time.

# This assumption implies that a good estimate for f(x) is the average
# of the Yi values in the window. The estimate is:
# fhat(x0) = 1/N0 ???i???A0 Yi 
# In smoothing, we call the size of the interval |x???x0| satisfying the
# particular condition the window size, bandwidth or span.

# Basically choose a week of data, take the mid point 3.5 (x0) and use
# absolute value |x-xo| <= 3.5

# bin smoothers in R
span <- 7 
fit <- with(polls_2008,ksmooth(day, margin, x.points = day, kernel="box", bandwidth =span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

# kernel
span <- 7
fit <- with(polls_2008, ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")


##############################
# LOCAL WEIGHTED REGRESSION #
#############################

# A limitation of the bin smoothing approach is that we need small
# windows for the approximately constant assumptions to hold which may
# lead to imprecise estimates of f(x). Local weighted regression (loess) 
# permits us to consider larger window sizes.

# One important difference between loess and bin smoother is that we
# assume the smooth function is locally linear in a window instead of
# constant.

# The result of loess is a smoother fit than bin smoothing because we
# use larger sample sizes to estimate our local parameters.

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))

### Local Weighted Regression (loess)
span <- 21/diff(range(polls_2008$day))

tmp <- polls_2008 %>%
  crossing(center = polls_2008$day) %>%
  mutate(dist = abs(day - center)) %>%
  filter(rank(dist) / n() <= span) %>%
  mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3)

# Show data span as a line, you fit lines to the curve
tmp %>% 
  filter(center %in% c(-125, -55)) %>%
  ggplot(aes(day, margin)) +   
  scale_size(range = c(0, 3)) +
  geom_smooth(aes(group = center, weight = weight), 
              method = "lm", se = FALSE) +
  geom_point(data = polls_2008, size = 3, alpha = .5, color = "grey") +
  geom_point(aes(size = weight)) +
  facet_wrap(~center)


total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")


# Show how degrees alter the curve
tmp %>% ggplot(aes(day, margin)) +
  geom_point(size = 2, alpha = .5, color = "grey") +
  geom_line(aes(day, .fitted), data = fits, color = "red") +
  facet_wrap(~span)

data.frame(x = seq(min(polls_2008$day), max(polls_2008$day), length.out = 100)) %>%
  mutate(w_0 = (1 - (abs(x-x_0)/21)^3)^3*I(abs(x-x_0)<=21)) %>%
  ggplot(aes(x, w_0)) +
  geom_line()

total_days <- diff(range(polls_2008$day))
span <- 28/total_days
fit_1 <- loess(margin ~ day, degree=1, span = span, data=polls_2008)
fit_2 <- loess(margin ~ day, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth_1 = fit_1$fitted, smooth_2 = fit_2$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth_1), color="red", lty = 2) +
  geom_line(aes(day, smooth_2), color="orange", lty = 1) 

polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth()

# Change degrees
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red",  span = 0.15,
              method.args = list(degree=1))



# COMPREHENSION CHECK
# In the Wrangling course of this series, PH125.6x, we used the 
# following code to obtain mortality counts for Puerto Rico for
# 2015-2018:
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

# Use the loess() function to obtain a smooth estimate of the expected 
# number of deaths as a function of date. Plot this resulting smooth 
# function. Make the span about two months long.
span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = "red")

# Now plot deaths per day, with years in different colours
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)


# Suppose we want to predict 2s and 7s in the mnist_27 dataset with
# just the second covariate. Can we do this? On first inspection it
# appears the data does not have much predictive power.

# In fact, if we fit a regular logistic regression the coefficient for 
# x_2 is not significant!
library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
# Plotting a scatterplot here is not useful since y is binary:
qplot(x_2, y, data = mnist_27$train)

# Fit a loess line to the data above and plot the results. What do you
# observe?
mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")



##########################
# WORKING WITH MATRICES #
#########################

# We can load all 60,000 mnist digits using this code
mnist <- read_mnist()
# In cases like this it is convenient to save the predictors in a matrix
# and the outcomes in a vector
class(mnist$train$images)

# This is a pretty bit matrix so we'll just take the first 1000
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

# We pose 5 challenges to motivate the use of matrices

# 1
# We're going to study the distribution of the total pixel darkness and
# how it varies by digits.

# 2
# We're going to study the variation of each pixel and remove predictors,
# columns, associated with pixels that don't change much and thus
# can't provide much information for classification.

# 3
# We're going to zero out low values that are likely smudges.
# First, we're going to look at the distribution of all pixel values,
# use this to pick a cutoff to define unwritten space,
# then make anything below that cutoff a zero.

# 4
# We're going to binarize the data.
# We're going to first look at the distribution of all pixel values,
# use this to pick a cutoff, and distinguish between writing and no writing.
# Then convert all entries into either zero or one.

# 5
# We're going to scale each of the predictors in each entry to have 
# the same average and standard deviation.


# Matrix Notation
# 3 types of objects in matrix algebra
# Scalar, Vectors, Matrices

# Matrix in r
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)
# Mathematically represent in BOLD UPPER CASE characters
# X = [X1X2]

# In r we extract the dimensions of a matrix like this:
dim(x)
# Notes vectors can be thought of as N by 1 matrices

# Vectors have no dimensions
dim(x_1)
# We can explicitly convert a vector into a matrix
dim(as.matrix(x_1))

# Create a vector
my_vector <- 1:15
# fill the matrix by column
mat <- matrix(my_vector, 5, 3)
mat
# fill by row
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t
identical(t(mat), mat_t)      # t = transpose here
matrix(my_vector, 5, 5)       # no warnings if matrix doesn't match vector
grid <- matrix(x[3,], 28, 28) # turn 784 length vector into the 28x28 grid to see number
image(1:28, 1:28, grid)
# flip the image back
image(1:28, 1:28, grid[, 28:1])


# The function rowSums() computes the sum of each row.
sum <- rowSums(x)
# The function rowMeans() computes the average of each row.
avg <- rowMeans(x)
# Display how the average intensity varies from digit to digit
data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

# We can compute the column sums and averages using the 
# functions colSums() and colMeans().
colSums(x)
rowSums(x)

# The matrixStats package adds functions that performs operations on each
# row or column very efficiently,
# including the functions rowSds() and colSds().
install.packages("matrixStats")
library(matrixStats)
rowSds(x)
colSds(x)

# The apply() function lets you apply any function to a matrix.
# The first argument is the matrix,
# the second is the dimension (1 for rows, 2 for columns),
# and the third is the function. 
avgs <- apply(x, 1, mean)   # mean of rows
sds <- apply(x, 2, sd)      # sd of columns
# Note the flexibility here comes at a speed cost


# Second challenge
# Variation of each pixel with columns
library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))

# Some entries have low variability, makes sense as we tend to write in
# the centre of the box.  See heat map of variation
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])
# We could remove these features as they don't help prediction

# Filtering columns based on summaries
# The operations used to extract columns:
x[,c(351,352)]
# The operations used to extract rows:
x[c(2,3),]

# We can also use logical indexes to determine which columns or rows to keep:
new_x <- x[ ,colSds(x) > 60]
dim(new_x)

# Important note: if you select only one column or only one row,
# the result is no longer a matrix but a vector.
class(x[,1])
dim(x[1,])

# We can preserve the matrix class by using the argument drop=FALSE. 
class(x[ , 1, drop=FALSE])
dim(x[, 1, drop=FALSE])


# INDEXING WITH MATRICES AND BINARISING THE DATA
# We want to look at a histogram of all our pixels
# We can turn matrices back into vectors
mat <- matrix(1:15, 5, 3)
mat
as.vector(mat)

# To see a histogram of all our predictors
qplot(as.vector(x), bins=30, color=I("black"))
# We see 2 distinct results, parts with no ink, and parts with ink

# If we want to remove all snudges then say set to zero if < 25
new_x <- x
new_x[new_x < 50] <- 0
new_x

# To see a smaller example
mat <- matrix(1:15, 5, 3)
mat[mat<3] <- 0
mat
# More complicate example, zero all values between 6 and 12
mat <- matrix(1:15, 5, 3)
mat[mat>6 & mat<12] <- 0
mat


# The histogram we saw suggested the data is mostly binary, ink or no ink
# To binarise the data set all values below half to 0, above to 1
bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1

# We can also convert it to a matrix using logicals and coerce it like this
bin_X <- (x>255/2)*1

# To see a visual of the results
grid <- matrix(x[3,], 28, 28) # turn 784 length vector into the 28x28 grid to see number
image(1:28, 1:28, grid[, 28:1])


# Vectorisation for matrices and Matrix Algebra operations
# For our final challenge we are going to standardise the rows or columns

# In R, we subtract a vector from a matrix, the first element of each vector
# is subtracted from the first row of the matrix
# The second element from the second row, etc..
# The columnular vector subtracts from each column of the matrix

# We can scale each row of a matrix with this simple code
(x-rowMeans(x))/rowSds(x)
# To do it for columns we would have to transpose the matrix
t( t(x)-colMeans(x))

# We can also do this using a function called sweep which works like apply
x_mean_0 <- sweep(x, 2, colMeans(x))    # 2 = columns
# default is to subtract but we can change this, say divide:
x_standardised <- sweep(x_mean_0, 2, colSds(x), FUN="/")

# Matrix multiplication in R is done using
# %*%
t(x) %*% x    # or simply with
crossprod(x)
# To compute the inverse we use solve
solve(crossprod(x))

# Finally the qr decomposition is found using the qr functions
qr(x)


# 100x10 matrix (100 rows, 10 cols) with random numbers
x <- matrix(rnorm(100*10), 100, 10)

# Write code for the dimensions, num rows and num cols of x
dim(x)
nrow(x)
ncol(x)

# Which of the following lines of code would add the scalar 1 to row 1,
# the scalar 2 to row 2, and so on, for the matrix x?
x <- matrix(rnorm(100*10), 100, 10)
x <- x + seq(nrow(x))
x <- sweep(x, 1, 1:nrow(x),"+")

# Which of the following lines of code would add the scalar 1 to column 1,
# the scalar 2 to column 2, and so on, for the matrix x?
x <- matrix(rnorm(100*10), 100, 10)
x <- sweep(x, 2, 1:ncol(x), FUN = "+")


# For each observation in the mnist training data, compute the proportion of
# pixels that are in the grey area, defined as values between 50 and 205
# (but not including 50 and 205). (To visualize this, you can make a boxplot 
# by digit class.)

# What proportion of the 60000*784 pixels in the mnist training data are in the
# grey area overall, defined as values between 50 and 205? Report your answer
# to at least 3 significant digits.

# Download all 60k 
mnist <- read_mnist()
totpix <- 60000*784
# Assign pixels to x for all records
x <- mnist$train$images
# Binarise to calculate proportion
x_prop <- sum(ifelse(x<=50|x>=205, 0, 1)) / totpix

# Alt solution
mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")
mean(y) # proportion of pixels


