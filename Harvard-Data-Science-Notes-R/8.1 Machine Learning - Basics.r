#####################
# MACHINE LEARNINE #
#####################

# In Machine Learning we use features to predict the outcome
# X1, X2, ..., Xp denotes the features
# Y denotes the outcomes
# Yhat denotes the predictions

# ML approach is to train an algorithm using a dataset where we know the outcome
# in order to use it to make a prediction when we don't know an outcome

# Features are sometimes known as predictors or covariates

# Prediction outcomes can be classed as categorical and continuous outcomes
# Categorical - Y can be any one of K classes
# Example - for the digit reader K has 10 classes (0 through 9)
# In speech recognition, the outcome is all possible words
# In spam detection - spam or not spam

# In this course we donate the k categories with indexes K of 1 through K
# k = 1, 2, ..., K
# For binary we use k = 0, 1

# We use the term outcome to denote the actual outcome we observe so the aim
# is to get Yhat to match the outcome

# The outcome can be categorical, such as digit, word, spam, pedestrian or empty
# road ahead,
# or continuous, movie rating, house price, stock price, distance between car 
# and pedestrian
# The concept and algorithms apply to both but differences in approach

# When the outcome is categorical we refer to this as classification
# Our predictions will be categorical and will be right or wrong

# For continuous our outcomes are predictions and cannot be classes as right or wrong
# Instead we will make an error which is the difference between the prediction
# and outcome, we use Yhat for both


# EXAMPLE

# Yi is the outcome for an observation or index i
# We use bold X_i  to distinguish the vector of features
# number identifier 28x28 pixel grid = 784 features
# Bold Xi = xi1, xi2, ..., xi784
# When referring to arbitrary set of outcomes and features we use Y and bold X
# We use upper case as we think of them as random variables
# Lower case is used to denote observed values. E.g. X=x



#############################################################
# CARET PACKAGE, TRAINING & TEST SETS AND OVERALL ACCURACY #
############################################################
install.packages("tidyverse")
install.packages("caret")
library(caret)

# Using machine learning to predict sex from heights dataset
library(dslabs)
data(heights)

# First define predictors
y <- heights$sex        # outcome
x <- heights$height     # features
# This is clearly a categorical outcome

# We know that this will be difficult to predict sex as male and female heights
# are not that different, but can we do better than guessing?  Let's define what
# better means - better than 50% guesswork


# Split the data into 2.  The first is known as the training set, we know the
# outcomes and use it to train our model, i.e. improve accuracy
# The other half is known as the test set and we use it as if we don't know the 
# outcomes, then compare how it did

# A standard way of creating training and test sets is to randomly split the data
# caret allows us to create partitions with indexes for randomly split data
set.seed(2)     # For reproducible results
test_index <- createDataPartition(y, times = 1, p=0.5, list=FALSE)
# times = how many random samples of indexes to return
# p = proportion of data in the index
# list = return index as a list of not

# Define test and training set
train_set <- heights[-test_index, ]
test_set <- heights[test_index, ]

# When looking at the number of correct predictions this is referred to as
# overall accuracy

# Let's build 2 competing algorithms to compare the accuracy.
# First let's create a simple guessing algorithm
y_hat <- sample(c("Male", "Female"), length(test_index), replace=TRUE)
# Note that we are ignoring the predictor and simply guessing the sex

# Moving on to using factors to represent categorical outcomes
# Our functions developed for Machine Learning packages, such as caret, require
# that categorical outcomes be coded as factors
y_hat <- sample(c("Male", "Female"), length(test_index), replace=TRUE) %>%
          factor(levels = levels(test_set$sex))

# Overall accurracy is simply the proportion that was predicted correctly
mean(y_hat == test_set$sex)
# We see it is roughly 49%, because we are just guessing

# How can we do better?  Explanatory analysis reveals that males are slightly
# taller than females
heights %>% group_by(sex) %>% summarise(mean(height), sd(height))

# To use this insight we predict male if the height is within 2sd of the avg male
y_hat <- ifelse(x>62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)
# We see the accuracy goes up to 80%

# Can we make this prediction better?  We can.  We used 62 as the cutoff but
# we can analyse a range of values and choose the one that gives the best result
# Remember, only use the training set for this.  The test set is only for evaluation

# Whilst not a problem here, evaluating an algorithm on the training set can lead
# to overfitting which often results in dangerously over optimistic assessments

# Let's analyse 10 different cutoffs and pick the best
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
                factor(levels = levels(test_set$sex))
    mean(y_hat == train_set$sex)
})
# Make a plot of accuracy
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line()
# Display max accuracy
max(accuracy)
# Find best cutoff
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# Test this cutoff on our test set to make sure that this is not overly optimistic
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)
# Very slightly lower but still higher than before


#####################
# CONFUSION MATRIX #
#####################

# Overall accuracy can be a deceptive measure of accuracy.  To see this we
# construct a confusion matrix which tabulates each combination of prediction
# and actual value
table(predicted = y_hat, actual = test_set$sex)

# If we compute the accuracy separately for each sex we get
test_set %>% mutate(y_hat = y_hat) %>% group_by(sex) %>%
    summarise(accuracy = mean(y_hat == sex))

# We see high accuracy for male predictions but low for females.  There is 
# an imbalance. How is the overall accuracy so high then?
# This is because of prevalance, there are more males than females in the 
# datasets
prev <- mean(y == "Male")     # 77% of students are male

# The mistakes in female prediction is outweighed by correct calls for males
# The algorithm is biased because the data is biased
# This is a big issue for machine learning algorithms
# This is why we use several other metrics other than overall accuracy
# There are other methods that prevent prevalence from clouding our assessments
# These can all be derived from the confusion matrix

# A general improvement to overall accuracy is to study sensitivity
# and specificity separately, using a binary outcome

# In the case of the predicting digits example we can call for specificity
# of correctly calling the digit as 1, and wrong as zero, y=1 or y=0

# In general, sensitivity is defined as the ability of an algorithm to
# predict a positive outcome when the actual outcome is positive
# Y_hat = 1 whenever y = 1

# Because an algorithm that calls everything positive has perfect sensitivity
# this metric is not good enough on its own (y_hat = 1 regardless of y), so
# we also examine specificity which is the ability of an algorithm to not
# predict positive when the actual value isn't positive.
# Y_hat = 0 when y = 0

# In other words:
# High sensitivity means Y = 1 implies Y_hat = 1
# High specificity means Y = 0 implies Y_hat = 0

# Another way to define high specificity is to say positive prediction for positive outcomes
# Y_hat = 1 implies Y = 1

# This gives us the 4 entries of the confusion matrix:

# Predicted Positive + Actual Positive = True Positive (TP)
# Predicted Positive + Actual Negative = False Positive (FP)
# Predicted Negative + Actual Positive = False Negative (FN)
# Predicted Negative + Actual Negative = False Negative (TN)

# Now we can give more specific definitions:

# Sensitivity = TP / (TP+FN) = True Positive Rate or Recall
# Specificity = TN / (TN+FP) = True Negative Rate

# Specificity can also be defined as:
# Precision = TP / (TP+FP) = Positive Predicted Value (PPV)
# Precision relies on prevalence as you can get higher precision even when guessing biased

# The Confusion Matrix function in the caret package does all this for us
# once we define what a positive is. The function expects factors as inputs
# and the first level is defined is considered the positive outcome, or y=1

# In our example the female is the first level because it comes before male
# alphabetically
install.packages("e1071")
confusionMatrix(data = y_hat, reference = test_set$sex)


###################################
# BALANCED ACCURACY AND F1 SCORE #
##################################

# Whilst it is recommended to analyse both figures above, very often it is useful
# to have a one number summary, such as for optimisation purposes

# One number that is preferred is the average of specificity and sensitivity
# and is called the balanced accuracy

# As S+S are rates, it is more appropriate to compute the harmonic average
# F1 Score = 1 / 0.5 * (1/Recall + 1/Precision)
# Often written for ease as:
# 2 * (precision.recall / precision+recall)

# Other real world considerations:
# Plane safety, it is more important to maximise sensitivity or specificity
# It is more costly to fail to predict a plane will malfunction before crashing
# than it is to ground a perfectly good plane

# In a capital murder case, the opposite is true as a false positive can lead to
# killing an innocent person through the death sentence

# The F1 score can be adopted to weigh sensitivity and specificity differently
# We can use Beta to determin how much more sensitivity is than specificity
# This is called a weighted harmonic average
# 1 / ((B^2/1+B^2)*(1/Recall) + (1/1+B^2)*(1/Precision))

# The F_meas function in the caret package computes this summary with
# Beta defaulting to 1

# Let's rebuild our algorithm maximising the F-Score
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
          y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
                    factor(levels = levels(test_set$sex))
          F_meas(data = y_hat, reference = factor(train_set$sex))
})
# Plot the cutoff accuracy
data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()
# Identify max F_1
max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

confusionMatrix(data = y_hat, reference = test_set$sex)
# This is much better and balanced algorithm


# NOTE
# A machine learning algorithm with very high sensitivity and specificity
# may not be useful in practice when prevalence is close to zero or 1

# For example, if you develop an algorithm for disease diagnosis with
# very high sensitivity, but the prevalence of the disease is pretty
# low, then the precision of your algorithm is probably very low 
# based on Bayes' theorem.



####################################
# ROC AND PRECISION RECALL CURVES #
###################################

# A very common approach to evaluating accuracy and F1-score is to compare
# them graphically by plotting both. A widely used plot that does this is the
# receiver operating characteristic (ROC) curve. The ROC curve plots sensitivity
# (TPR) versus 1 - specificity or the false positive rate (FPR).

# However, ROC curves have one weakness and it is that neither of the measures
# plotted depend on prevalence. In cases in which prevalence matters, we may 
# instead make a precision-recall plot, which has a similar idea with ROC curve.

p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")


library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})


height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()



###############
# ASSESSMENT #
##############

library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# Proportion of students
sum(dat$type == "inclass" & dat$sex == "Female") / sum(dat$type == "inclass")
sum(dat$type == "online" & dat$sex == "Female") / sum(dat$type == "online")
# Elegant solution
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

# Predict sex using one gender class based on answers above
y_hat <- ifelse(dat$type == "inclass", "Female", "Male") %>% factor(levels = levels(y))
mean(y == y_hat)

# create confusion matrix
table(predicted=y_hat, actual=y)
table(y_hat, y)

# What is the sensitivity & specificity
sensitivity(data = y_hat, reference = y)
specificity(data = y_hat, reference = y)

# What is the prevalence of females
mean(y == "Female")


# USING IRIS DATASET
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

# Create training and test sets
# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

# find best predictor
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)

# What is the accuracy from the test set
# Using 0.96 from the previous output
cutoff <- seq(train$Petal.Width[which.min(test$Petal.Width)], train$Petal.Width[which.max(train$Petal.Width)], 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Width > x, 'virginica','versicolor') %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
# Make a plot of accuracy
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line()
# Display max accuracy
max(accuracy)
# Find best cutoff
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff
# Update algorithm
cutoff <- 4.7
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(test$Petal.Length > x, 'virginica','versicolor') %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

# Elegant solution
predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]
y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)



# Using 2 features, petal.length and petal.width
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
# Get ranges for features - range returns min and max of a vector
petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)
# Make predictions feature 1
length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7
# Make predictions feature 2
width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5
# Get accuracy
y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)




##############################
# CONDITIONAL PROBABILITIES #
#############################

# The most common reason for not building a perfect algorithm is that it is impossible
# To see this note that most datasets will include groups of observations with
# the same exact observed values for all predictors and thus resulting in the same
# prediction even though they have different outcomes

# To achieve an optimised algorithm that is better than guessing or even expert
# opinion on occasion we build them out using probabilitist representationts

# We use X1=x1, X2=x2, ..., Xp=xp) for observed values x1, x2, ..., xp)
# for covariates X1, X2, ..., Xp
# This does not imply that the outcome, y, will take a specific value but
# rather implies a specific probability

# We denote the conditional probabilities of each class k using:
# Pr(Y=k | 1=x1, ..., Xp=xp ), for k=1,..., K

# To avoid writing out all the notation we use bold letters
# BoldX = (X1,...,Xp) and boldx = x1,...,xp)

# The notation for the conditional probability of being in class k
# pk(x) = Pr(Y=k | boldX = boldx), for k=1,...,K
# where p(x) represents conditional probabilities, not to be confused with p
# the number of predictors

# *********
# For any given set of predictors, x, we'll predict the class, k, with the largest
# probability among p1(x), p2(x), ..., pK(x)
# In mathematical notation it becomes: Yhat = max(pk(x)) with k underneath max

# It's not that simple as we don't know the pk(x) and this is what the main
# challenge of Machine Learning is all about
# The better our algorithm estimates p_hat_k(x) the better our predictor will be
# Yhat = max(pk(x)) with k underneath max

# What determines how good our algorithm is?
# How close our max(pk(x)) is to 1, and 
# How close our estimate is to the actual probabilities

# We can't control the first point but so our effort goes into finding the best
# way to estimate the conditional probabilities of the problem

# Some algorithms, number readers, should be able to achieve near perfect accuracy
# but others, ie movie recommendations, will be restricted by the randomness of
# the process

# We should note that maximising the probability is not always optimal in practice
# As we saw earlier with aeroplane and medical examples, sensitivity and specificity
# may differ in context, but for us to build a good prediction model we should
# be able to tune these cutoff parameters from class to class


##############################################
# Conditional Expectation and Loss Function #
#############################################

# For binary data you can think of the conditional probability that y = 1
# when x = x as a proportion of 1s in the stratum of the population for which
# X=x.  Pr(Y=1 | X=x)

# Many of the algorithms we will learn can be applied to continuous and categorical
# data due to the connection between conditional probabilities and conditional
# expectations.  As the data is zero or 1, the conditional expected value will
# match the conditional probability
# Therefore we often only use the conditional expectation to denote both
# The previously mentioned methods for binary (sensitivity, specificity, F1)
# are not always best for continuous outcomes.

# The general approach for defining best in Machine Learning is to define
# a LOSS FUNCTION.  The most commonly used one is a SQUARED LOSS FUNCTION
# This is simply the difference between prediction and outcome squared:
# (Yhat - Y)^2
# Because we often have n observations, it becomes the mean squared error:
# 1/N Sigma(i=1 to N) (Yhat-Y)^2
# Note that for binary this is equal to the overall accuracy

# Our goal is to build an algorithm that minimises the loss to be as close to 
# zero as possible

# Because our data is usually a random sample the mean squared error (MSE)
# is a random variable.  As algorithms are different the goal is to try as many
# as possible to find the one that minimises the average MSE across many samples

# This becomes the expectation of the MSE given as:
# E{1/N Sigma(i=1 to N) (Yhat-Y)^2}

# This is a theoretical concept as we only have 1 dataset to work with however
# we will later learn techniques to estimate this quantity

# There are other methods such as absolute values but we focus on MSE in this course
# as it is the most widely used

# We care about conditional expectation because the expected value has an attractive
# mathematical property, it minimises the expected square loss.
# Of all the possibly Yhats, the conditional expectation of y given x
# minimises the expected loss given x
# Yhat = E(Y | X=x) minimises E{(Yhat-Y)^2 | X=x}

# Given this, the main task of machine learning is this:
# Use data to estimate conditional probabilities
# f(x) estimate (3line equals sign) E(Y|X=x) for any set of features x=(x1,...,xp)

# Due to this property, a succinct description of the main task of machine
# learning is that we use data to estimate for any set of features. The main
# way in which competing machine learning algorithms differ is in their approach
# to estimating this expectation.


# COMPREHENSION CHECK USING BAYES THEORUM

# P(A|B) = P(B|A) * (P(A)/P(B))
# Patient comes into the doctors to see if they have a disease

# Test is positive 85% when patient has disease (high sensitivity)
# P(test+ | disease) = 0.85

# Test is negative 90% when patient is healthy (high specificity)
# P(test- | no disease) = 0.90

# The disease is prevalent in about 2% of the community
# p(disease) = 0.02

# Calculate the probability that you have disease if the test is positive

# P(test+ | disease) = 0.85
# P(test- | disease) = 0.15

# P(test- | no disease) = 0.90
# P(test+ | no disease) = 0.1


# Using Bayes theorum:
# P(disease|test+) = P(test+|disease) * P(disease)/P(test+)
# = P(test+|disease)*P(disease) / P(test+|disease)P(disease)+P(test+|healthy)P(healthy)
# = 0.85*0.02 / 0.85*0.02+0.1*0.98

# So this means A = disease, B = test+
# P(A|B) = P(B|A)*(P(A)/P(B))

# P(A) = simply prevalence of disease = 0.02
# P(test+) = test+ and you have disease (TP) plus test+ but healthy (FP)
# = test+|disease * prevalence  +  test+|healthy * 1-prevalence

# Using R
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

# Calculate probability that a test is positive
# As we are using binary data a postive test is a 1
# mean(test) = probability of testing positive regardless
mean(test)

# Calculate probability that an individual has the disease given negative test
mean(disease[test==0])

# Calculate the probability that you have the disease if the test is positive
mean(disease[test==1]==1)

# Compare the prevalence of disease in people who test positive to the
# overall prevalence of disease. If a patient's test is positive, how much does
# that increase their risk of having the disease?
mean(disease[test==1]==1)/mean(disease==1)

# MAJOR REVISION NEEDED ON BAYES THEORUM AND THE CONSTRUCTION IN R OF DATASETS


# CONDITIONAL PROBABILITY CHECK 2
library(dslabs)
data("heights")

# We are now going to write code to compute conditional probabilities
# for being male in the heights dataset. Round the heights to the closest 
# inch. Plot the estimated conditional probability P(x)=Pr(Male|height=x)
# for each x.
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%qplot(height, p, data =.)

# In the plot we just made in Q6 we see high variability for low values of
# height. This is because we have few data points. This time use the 
# quantile 0.1,0.2,.,0.9  and the cut() function to assure each group has the
# same number of points. Note that for any numeric vector x, you can create
# groups based on quantiles like this: cut(x, quantile(x, seq(0, 1, 0.1)),
# include.lowest = TRUE).
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)



# You can generate data from a bivariate normal distrubution using the MASS
# package using the following code:
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
# And you can make a quick plot using plot(dat)
# Using an approach similar to that used in the previous exercise, 
# let's estimate the conditional expectations and make a plot. Part of the 
# code has again been provided for you:
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps))) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
qplot(x, y, data =.)


