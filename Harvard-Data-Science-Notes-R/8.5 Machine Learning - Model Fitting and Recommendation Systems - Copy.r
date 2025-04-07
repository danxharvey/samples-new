#######################
# USING MNIST DIGITS #
######################

# This is a popular dataset for trying out and competing in Machine Learning
# Modified National Institute of Standards and Technology database (MNIST)
library(dslabs)
mnist <- read_mnist()

# Dataset contains 2 components, a test and training set
names(mnist)

# Each component includes a matrix of features in columns (784 = 28x28 px grid)
dim(mnist$train$images)

# Vector containing class of integers
class(mnist$train$labels)
table(mnist$train$labels)

# For performance reasons
# sample 10k rows from training set, 1k rows from test set
set.seed(123)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
#note that the line above is the corrected code - code in video at 0:52 is incorrect
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])


#############################
# PRE-PROCESSING MNIST DATA #
############################

# In Machine Learning we often transform predictors before running any 
# ML algorithm, this is actually an important step

# We also remove predictors that are clearly not useful, also an important step

# Examples of pre-processing are:
# Standardising the predictors
# Taking the log transform of some predictors, or some other transformation
# Removing predictors that are highly correlated with others
# Removing predictors with very few non unique values or close to zero variation

# We'll show an example that related to variability, it has a large number
# of features with zero or close to zero variability

# Use this code to compute the standard deviation of each columns and plot
# a histogram
library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

# The caret package contains a function that recommends features with near
# zero variabce
library(caret)
nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))   # On this PC it is the red bits removed
# The bit around the edges of the grid, makes sense as we write in the middle

# We can see how many columns are left
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)


#################################
# MODEL FITTING FOR MNIST DATA #
################################

# The caret package requires that we add column names to the feature matrices
# In general, it is a good idea to test out a small subset of the data first
# to get an idea of how long your code will take to run.

# We will implement knn and random forest on the MNIST data

# First add colnames using the number of the column as a name
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

# First we will train knn, optimising for k
# Note we will be optimising distance between each point in the train and test
# sets.  This is a lot of calculations.
# We will use k-fold cross validation to improve speed
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(x[,col_index], y,
                   method = "knn", 
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)
# Now plot the results to show the model that maximises accuracy
ggplot(train_knn)


# As this can take a long time on a standard laptop we can use smaller subsets
# to help calculate our run-time, or even make hand calculations
n <- 1000     # Number of rows
b <- 2        # Number of cross validation rows
# Take a sample and slowly increase to get an idea of how long the final
# code will take
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[index, col_index], y[index],
                   method = "knn", 
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)

# Once we're done optimising our algorithm we can fit the entire dataset
fit_knn <- knn3(x[ ,col_index], y,  k = 3)
y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
# Check accuracy
cm$overall["Accuracy"]
# Check specificity and sensitivity
cm$byClass[,1:2]
# We see 8 is hardest to detect (lowest sensitivity)
# Hardest to preditct is 7 (lowest specificity)


# Can we do better with random forest?  With RF computation time is an
# even bigger challenge than knn - for each forest we need to build 100 trees
# We also have several parameters that we can tune
# Here we use Rborist as it is faster than the Random Forest package
# (Less features but faster)

# With RF the fitting is the slowest part, as opposed to fitting with knn
# We will only use 5 fold cross validation
# We will also reduce the number of trees as we are only fitting the model
# Finally we'll take a random subset of observations to build each tree
    # We change this with the nsamp function
library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)   # n=5=20%-1-p=0.8
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], 
                   y, 
                   method = "Rborist", 
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
# Plot the results and select best parameters using best tune component of
# the training set
ggplot(train_rf)
train_rf$bestTune

# Now we are ready to fit our final model
# Setting number of trees to a larger number
fit_rf <- Rborist(x[, col_index],y, 
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
# Check final accuracy
cm$overall["Accuracy"]


# Let's look at some samples of the original image in our test set
rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}
# Our predictions look good. With more parameter tuning or examining more
# trees we can get even higher accuracy


########################
# VARIABLE IMPORTANCE #
#######################

# It was mentioned earlier that one of the limitations of RF is that they're
# not very interpretable. However the concept of Variable Importance helps
# us a bit in this regaed

# Rborist does not yet support VI calcuations so we'll use the RandomForest
# packge to demonstrate. Furthermore, we won't delete any columns as per
# the pre-processing steps above
library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y,  ntree = 50)
# Use the importance function to see the importance of each feature
imp <- importance(rf)
imp

# If we plot the importance results in a 28x28 as per the source of each
# feature, we can see where the important features are
image(matrix(imp, 28, 28))

# An important part of data science is visualising results to discern why we
# are failing

# We can write code to compare the knn v RF outputs to see where we made a
# mistake
p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]

# Then make images to see where we made a mistake
# Top 12 knn errors
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2)," but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

# Top 12 RF errors
p_max <- predict(fit_rf, x_test[,col_index])$census  
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2), " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

# By analysing the mistakes you might see ways in which to improve your algorithm


##############
# ENSEMBLES #
#############

# Ensembles combine multiple machine learning algorithms into one model
# to improve predictions. This a very powerful approach.

# Here we present a simple example where we compute new class probabilities
# by taking the average of the class probabilities provided by knn and RF.
# Once we do this we see that the new averages can be used to predict and
# increase overall accuracy over just using one approach.
p_rf <- predict(fit_rf, x_test[,col_index])$census  
p_rf<- p_rf / rowSums(p_rf)
p_knn  <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn)/2

# Takes the maximum
y_pred <- factor(apply(p, 1, which.max)-1)
# New confusion matrix
confusionMatrix(y_pred, y_test)


# This was just with ensembling 2 methods.  In practice we might ensemble
# dozens or hundreds of different methods and this leads to substantial
# improvements in accuracy.



### COMPREHENSION CHECK - ENSEMBLES ###
# For these exercises we are going to build several machine learning models
# for the mnist_27 dataset and then build an ensemble.
# Each of the exercises in this comprehension check builds on the last.

# Use the training set to build a model with several of the models available
# from the caret package. We will test out 10 of the most common machine
# learning models in this exercise:
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn",
              "gamLoess", "multinom", "qda", "rf", "adaboost")

# Apply all of these models using train() with all the default parameters.
# You may need to install some packages. Keep in mind that you will probably
# get some warnings. Also, it will probably take a while to train all of
# the models - be patient!
# Run the following code to train the various models:

library(caret)
library(dslabs)
library(tidyverse)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 
names(fits) <- models


# Now that you have all the trained models in a list, use sapply() or map()
# to create a matrix of predictions for the test set. You should end up with
# a matrix with length(mnist_27$test$y) rows and length(models) columns.
pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)


# Compute the accuracy for each model on the test set
acc <- colMeans(pred == mnist_27$test$y)
mean(acc)


# Build an ensemble by majority vote.  If > 50% then 7, else 2
i <- seq(1, 10)
means <- sapply(i, function(i)
            ifelse(pred[,i]==7,1,0))
j <- 1:200
pred_news <- sapply(j, function(j)
            ifelse(sum(means[j,]) > 5, 7, 2))
mean(pred_news == mnist_27$test$y)

# Elegant solution
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)


# Which models were better than the ensemble?
ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]


# It is tempting to remove the methods that do not perform well and re-do the ensemble.
# The problem with this approach is that we are using the test data to make a decision.
# However, we could use the minimum accuracy estimates obtained from cross validation 
# with the training data for each model. Obtain these estimates and save them in an
# object. Report the mean of these training set accuracy estimates.
i <- 1:10
mins <- sapply(i, FUN=function(i)
          min(fits[[i]]$results$Accuracy))
mean(mins)
# Elegant solution
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)


# Now let's only consider the methods with an estimated accuracy of greater 
# than or equal to 0.8 when constructing the ensemble. Vote 7 if 50% or more
# of the models are predicting a 7, and 2 otherwise.
# What is the accuracy of the ensemble now?
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)




###########################
# RECOMMENDATION SYSTEMS #
##########################

# The motivation for this section is taken from the winners of the Netflix
# Challenge team

# Recommendation systems are more complicated machine learning challenges 
# because each outcome has a different set of predictors. For example, 
# different users rate a different number of movies and rate different movies

# Great advice from the winning team:

    # Of the numerous new algorithmic contributions, I would like to 
    # highlight one - those humble baseline predictors (or biases), which
    # capture main effects in the data. While the literature mostly 
    # concentrates on the more sophisticated algorithmic aspects, we have
    # learned that an accurate treatment of main effects is probably at least
    # as signficant as coming up with modeling breakthroughs.

    # http://blog.echen.me/2011/10/24/winning-the-netflix-prize-a-summary/

    # https://www.netflixprize.com/assets/GrandPrize2009_BPC_BellKor.pdf


library(dslabs)
data("movielens")     # Notice it's in tidy format

# Check number of unique users and movies are in dataset
movielens %>% summarise(n_users = n_distinct(userId),
                        n_movies = n_distinct(movieId))

# If we multiply users by movies we should get about 6 million rows
# We only have 100k - This implies that not every user rated every movie

# If we think of the cartesian product as a matrix then there will be many
# empty cells (i.e. where a user did not rate a specific movie)
# We can use the gather() function to create this matrix but it would most likely
# crash R

# The blanks would have NAs and you can think of the recommedation system as
# filling in the NAs

# You can show a limited grid
keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)
tab <- movielens %>%
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)
tab %>% knitr::kable()


# This challenge is much more complicated than the examples we have learned
# Each outcome yi has a different set of predictors

# If we are predicting movie i for user u then all other ratings for movies
# i and users u can be used as predictors, but different users rate a different
# number of movies and different movies

# We could use info from other movies that are similar to movie i, or from users
# that are similar to user u

# In essence, the entire matrix can be used as predictors for each cell


################################
# Exploratory Data Analysis: #
###############################

# Some movies get rated more than others
movielens %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

# Some users are more active than others at rating movies
movielens %>% 
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")


# First of all create test and training sets
library(caret)
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]


# Using the semi join function we remove users and movies from the test set
# that DO NOT appear in the training set.  The sets are now consistent
test_set <- test_set %>%
              semi_join(train_set, by ="movieId") %>%
              semi_join(train_set, by = "userId")


# In order to see how well we are doing, or to compare different models we
# need a baseline, we need to quantify what it means to do well.
# We need a loss function

# The Netflix challenge used a Residual Mean Squared Error on the test set
# If we define:
# y_u,i as the rating for movie i by user u,
# and yhat_u,i as our prediction, then the RMSE becomes:

# RMSE = sqrt( 1/N * Sigma_u,i of (yhat_u,i - y_u,i)^2  )
# where N is the number of user movie combinations

# Note we can interpret the RMSE as the SD as it is the typical error we make
# when predicting a movie rating

# If RMSE > 1 here, then we're typicalyl out by more than 1 stars, which is bad

# Let's convert the RMSE to R code:
RMSE <- function(true_ratings, predicted_ratings){
            sqrt(mean((true_ratings - predicted_ratings)^2))
}


# The Netflix challange winners implemented two types of models
# First was knn, find movies and users that are similar to each other
# Second was matrix factorisation, that's what we'll focus on here

# Now we're ready to build models and compare them to each other

# We'll start with the simplest model that assumes the same ratings for 
# all users and movies with all differences explained by random variation
# It looks like this:
# Y_ui = mu + Epsilon_ui        # Epsilon represents independent errors
                                # Sampled from same distribution, centred at 0
# mu represent true rating for all users and movies

# We know that the estimate that minimises the RMSE is the LSE of mu
# and this is just the average of all the ratings
mu_hat <- mean(train_set$rating)
mu_hat

# Compute the RMSE on the test set data using mu as a baseline
naive_rmse <- RMSE(test_set$rating, mu_hat)
# Using any other value than mu raises the naive error which fits as we know
# that mu_hat is the value that minimises the RMSE

# The winners had to find an RMSE of about 0.857.  We can definitely do better
# In order to track our results we'll create a table to store results
rmse_results <- data.frame(method="Just the average", RMSE = naive_rmse)

# We know that different movies have different ratings
# We add b_i to our algorithm for the average rating for movie i
# Y_ui = mu + b_i + epsilon_ui
# In statistics we call this effects, or in Netflex challenge, bias

# We use LSE to estimate the b in the following way
# fit <- lm(rating ~ as.factor(userId), data = movielens)
# We don't use this code as it would be painfully slow

# In this particular situation we know that the LSE, bhat_i is just the average
# of y_ui minus the overall mean for each movie
mu <- mean(train_set$rating)
movie_avgs <- train_set %>% group_by(movieId) %>% 
                  summarise(b_i = mean(rating - mu))
# Note we're dropping the hats to make the code cleaner (great!)

# Plot the results
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, 
                      data = ., color = I("black"))


# Let's see how our model improves using the b we just calculated
# Left join by movie_Id to get the b value
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
# Apply some fancy ass formatting
rmse_results %>% knitr::kable()


# Can we make it better by looking at the users u?
# We can compute the average rating for user u for those who rated
# over 100 movies
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")


# This implies we can further improve our model by adding b_u for user effect
# Y_ui = mu + b_i + b_u + epsilon_ui

# We could use lm again, but it would probably crash our computer
# lm(rating ~ as.factor(movieId) + as.factor(userId))
# Instead we will compute our approximation by computing the
# overall mean (uhat), the movie effects(bhat_i) and then estimating
# the user effect bhat_u by taking the average of the residuals obtained
# afer removing the overall mean and movie effect from the ratings y_ui:
user_avgs <- train_set %>%
  left_join(movie_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu - b_i))

# Now to predict
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by="userId") %>%
  mutate(pred=mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User effects model",
                                    RMSE = model_2_rmse))


### COMPREHENSION CHECK ###

# The following exercises all work with the movielens data, which can be
# loaded using the following code:
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

# Compute the number of ratings for each movie and then plot it against the
# year the movie came out. Use the square root transformation on the counts
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# We see that, on average, movies that came out after 1993 get more ratings
# We also see that with newer movies, starting in 1993, the number of
# ratings decreases with year: the more recent a movie is, the less time
# users have had to rate it.


# Among movies that came out in 1993 or later, select the top 25 movies 
# with the highest average number of ratings per year (n/year), and
# caculate the average rating of each of them. To calculate number of
# ratings per year, use 2018 as the end year.

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))


# From the table constructed in Q2, we can see that the most frequently rated
# movies tend to have above average ratings. This is not surprising: more 
# people watch popular movies. To confirm this, stratify the post-1993 movies
# by ratings per year and compute their average ratings. To calculate number
# of ratings per year, use 2018 as the end year. Make a plot of average
# rating versus ratings per year and show an estimate of the trend.
# What type of trend do you observe?
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()
# Trend:  The more often a movie is rated, the higher it's rating


# Suppose you are doing a predictive analysis in which you need to fill in
# the missing ratings with some value.
# Given your observations in the exercise in Q3, which of the following 
# strategies would be most appropriate?

# Answer: Fill in the missing values with a lower value than the average
#         rating across all movies



# The movielens dataset also includes a time stamp. This variable represents 
# the time and data in which the rating was provided. The units are 
# seconds since January 1, 1970. Create a new column date with the date.
# Which code correctly creates this new column?
install.packages("lubridate")
library(lubridate)
movielens <- mutate(movielens, date = as_datetime(timestamp))
# of course, due to the shitty course content they fail to say
# that this is in the lubridate package that needs installing


# Compute the average rating for each week and plot this average against 
# date. Hint: use the round_date() function before you group_by().
# What type of trend do you observe?
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()
# There is some evidence of a time effect on average rating


# If we define du,i as the day for user's u rating of movie i, which of 
# the following models is most appropriate?
# Yu,i = ?? + bi + bu + f(du,i) + ??u,i  - with f a smooth function of du,i


# The movielens data also has a genres column. This column includes every genre
# that applies to the movie. Some movies fall under several genres. Define a
# category as whatever combination appears in this column. Keep only categories 
# with more than 1,000 ratings. Then compute the average and standard error for
# each category. Plot these as error bar plots.
# Which genre has the lowest average rating?
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# The plot you generated in Q8 shows strong evidence of a genre effect. 
# Consider this plot as you answer the following question.
# If we define gu,i as the genre for user u's rating of movie i, which of 
# the following models is most appropriate?

# Yu,i = ?? + bi + bu + ???Kk=1 xku,i ??k + ??u,i 
# with xku,i=1 if gu,i is genre k




###################
# REGULARISATION #
##################

# Regularisation was one of the techniques used by the Netflix challenge winning
# team

# We didn't see overly big decreases as we added effects into our models.  Let's
# see why that was by looking at the biggest errors
test_set %>% 
  left_join(movie_avgs, by="movieId") %>%
  mutate(residual=rating-(mu+b_i)) %>%
  arrange(desc(abs(residual))) %>%
  select(title, residual) %>%
  slice(1:10) %>%
  knitr::kable()
# Most of these are quite obscure but have large predictions

# Let's take a look at the top and bottom 10
movie_titles <- movielens %>%
  select(movieId, title) %>%
  distinct()
# best 10 according to our estimates
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i) %>%
  slice(1:10) %>%
  knitr::kable()
# Top 10 worst movies
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i) %>%
  slice(1:10) %>%
  knitr::kable()

# They are all quite obscure so let's see how they were rated
train_set %>% count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  knitr::kable()
# And for the bad movies, it's the same, low numbers of ratings
train_set %>% count(movieId) %>%
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  knitr::kable()

# With lower number of ratings (users) we are more likely to see larger estimates
# for b_i as we haev more uncertainty due to low quantities
# These are basically noisy estimates that we should not traust
# Large errors can increase our RMSE so we should be conservative

# Previously we have used confidence intervals, but for predictions we need
# just one number.  Hence regularisastion, similar to Bayesian methods
# The general idea is to penalise large values of b to the sum of squares equations
# that we minimise

# We can't penalise every effect or the ratings would be unstable and vary with
# each new data point

# Instead by penalising the equation we optimise to be bigger when the estimates
# b are far from zero

# So to estimate the b, instead of minimising the RSS as is done by least squares
# we now minimise this equation
# 1/N * Sigma_ui (y_ui - mu - b_i)^2 + lambda Sigma_i (b_i)^2
# RSS + Lambda.... (the penalty term)

# Using calculus it can be shown that the values of b that minimise this equation
# is given as:
# bhat_i(lambda) = 1 / (lambda+n_i) Sigma_u=1-n_i  (Y_ui - muhat) 

# When n_i is very large (number or ratings) we will get a very stable estimate
# which means lamba is effectively ignored, however when n_i is very small,
# lambda becomes more relevant and the estimate decreases towards zero
# The larger lambda the more we shrink

# Let's compute these estimates using lambda = 3 (we'll see why later)
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>%
                    group_by(movieId) %>%
                    summarise(b_i = sum(rating-mu)/(n()+lambda), n_i=n())

# To see how the estimates shrink let's plot the regularised estimate with the 
# size of the circle telling how large n_i was
data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)
# You see that when n is small the values shrink more towards zero


# Let's look at our top 10 movies based on estimates when using regularisation
train_set %>% count(movieId) %>%
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>% knitr::kable()
# The results make much more sense
# Even for the bottom 10
train_set %>% count(movieId) %>%
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>% knitr::kable()

# How did this affect our results?
predicted_ratings <- test_set %>%
  left_join(movie_reg_avgs, by="movieId") %>%
  mutate(pred=mu+b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(method="regularised Movie Eff",
                                     RMSE = model_3_rmse))
rmse_results %>% knitr::kable()


# Note that lambda is a tuning parameter and we can use cross validation to
# choose the best value
lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>%
  group_by(movieId) %>%
  summarise(s=sum(rating-mu), n_i=n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>%
    left_join(just_the_sum, by="movieId") %>%
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred=mu+b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})

# Plot a visual to see why we picked 3.0 as lambda
qplot(lambdas, rmses)  
# Pull value
lambdas[which.min(rmses)]

# Note that we show this as an illustration, in practice we should be running
# cross validation just on a training set until the final assessment

# We can also use regularisation to estimate the user effect

# The equation we would minimise now becomes this, it includes parameters
# for the user effects as well:
# 1/N * Sigma_ui (y_ui - mu - b_i - b_u)^2 + lambda (Sigma_i (b_i)^2 + Sigma_u (b_u)^2)


# To include the user effect regularisation:
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, test_set$rating))
})
# Plot results
qplot(lambdas, rmses)  
# Get optimal lambda for user & movie effect regularisation
lambda <- lambdas[which.min(rmses)]
lambda

# Add to results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()



### COMPREHENSION CHECK FOR REGULARISATION ###

# The exercises in Q1-Q8 work with a simulated dataset for 1000 schools. 
# This pre-exercise setup walks you through the code needed to simulate the dataset

# If you have not done so already since the Titanic Exercises, please restart R 
# or reset the number of digits that are printed with options(digits=7)
options(digits=7)

# An education expert is advocating for smaller schools. The expert bases this 
# recommendation on the fact that among the best performing schools, many are small
# schools. Let's simulate a dataset for 1000 schools. First, let's simulate the 
# number of students in each school, using the following code:
# set.seed(1986) # if using R 3.5 or earlier
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))

# Now let's assign a true quality for each school that is completely independent
# from size. This is the parameter we want to estimate in our analysis. The true
# quality can be assigned using the following code:
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

# We can see the top 10 schools using this code: 
schools %>% top_n(10, quality) %>% arrange(desc(quality))

# Now let's have the students in the school take a test. There is random
# variability in test taking, so we will simulate the test scores as normally
# distributed with the average determined by the school quality with a standard
# deviation of 30 percentage points. This code will simulate the test scores:
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))


# What are the top schools based on the average score?
# Show just the ID, size, and the average score.
# Report the ID of the top school and average score of the 10th school.

# What is the ID of the top school and avg of #10?
schools %>% top_n(10, score) %>% arrange(desc(score)) %>% select(id, size, score)

# Compare the median school size to the median school size 
# of the top 10 schools based on the score.
# What is the median school size overall?
median(c(schools$size))
# What is the median school size of the of the top 10 schools based on the score?
schools %>% top_n(10, score) %>% .$size %>% median()


# According to this analysis, it appears that small schools produce better 
# test scores than large schools. Four out of the top 10 schools have 100
# or fewer students. But how can this be? We constructed the simulation so
# that quality and size were independent.
# Repeat the exercise for the worst 10 schools.
# What is the median school size of the bottom 10 schools based on the score?
schools %>% top_n(-10, score) %>% .$size %>% median()


# From this analysis, we see that the worst schools are also small.
# Plot the average score versus school size to see what's going on. 
# Highlight the top 10 schools based on the true quality.
# What do you observe?
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)
# Software picked me up wrong


# Let's use regularization to pick the best schools. Remember
# regularization shrinks deviations from the average towards 0.
# To apply regularization here, we first need to define the overall
# average for all schools, using the following code:
overall <- mean(sapply(scores, mean))


# Then, we need to define, for each school, how it deviates from that average.
# Write code that estimates the score above the average for each school 
# but dividing by n+?? instead of n, with n the school size and ?? a 
# regularization parameter. Try ??=25.
# What is the ID of the top school with regularization?
alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))


# Notice that this improves things a bit. The number of small schools that are
# not highly ranked is now lower. Is there a better ???
# Using values of ?? from 10 to 250, find the ?? that minimizes the RMSE.
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]
# alpha = 135


# Rank the schools based on the average obtained with the best ?? from Q6.
# Note that no small school is incorrectly included.
alpha <- 135
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))



# A common mistake made when using regularization is shrinking values 
# towards 0 that are not centered around 0. For example, if we don't 
# subtract the overall average before shrinking, we actually obtain a 
# very similar result. Confirm this by re-running the code from the 
# exercise in Q6 but without removing the overall mean.
# What value of ?? gives the minimum RMSE here?
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]




#########################
# MATRIX FACTORISATION #
########################

# This is widely used in machine learning. It is very much related to
# Factor Analysis
# Single Value Decomposition (SVD)
# Principal Component Analysis (PCA)

# Here we explain the concept in the context of movie recommendation systems
# We saw our previous model defined as:
# Y_ui = mu + b_i + b_u + Epsilon_ui

# This model leaves out an important source of variation to the fact that
# groups of movies have similar ratings patterns and groups of users

# We will study these patterns by looking at the residuals obtained
# after fitting our models
# r_ui = y_ui - bhat_i - bhat_u

# To study these residuals we will convert the data into a matrix
# So y_ui is the entry in row u column i for user u and movie i

# For illustration purposes we will only consider a small subset where
# users have rated many movies and movies have many ratings
train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% #3252 is Scent of a Woman used in example
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>% 
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

# To facilitate exploration we add row and column names
rownames(y) <- y[,1]
colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])

# We convert these to residuals by removing the column and row averages
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))


# If the model we've been using describes all the signal and the extra
# ones are just the noise then the residuals for different movies should
# be independent of each other, but they are not

# Here are some plots showing genre correlation
m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

# We can see a positive correlation between ganster movies and romcoms
# and a negative correlation between the genres
# This tells us that there is structure in the data we haven't accounted for
cor(y[, c(m_1, m_2, m_3, m_4, m_5)], use="pairwise.complete") %>% 
  knitr::kable()


# We can account for this using matrix factorisation
# We can simulate the data to see the residuals
set.seed(1)
options(digits = 2)
Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)
X <- jitter(P%*%t(Q))
X %>% knitr::kable(align = "c")
# See the strong correlation pattern
cor(X)


# We can explain this using coefficients
# We assign a 1 to gangster movies and -1 to romcoms
t(Q) %>% knitr::kable(aling="c")

# Now we can reduce the users down to 3 groups
P
# Those who like gangsters films, those who like romcoms and those
# with no preference

# We this method we can reconstruct the data and reduce it from 
# a vector containing 60 values to one containing 17 values

# We can model the 60 residuals with the 17 parameter model like this
# r_ui approx pu_qi

# We can explain more of the variance if we use a model like this one
# Y_ui = mu + b_i + b+u + pu_qi + Epsilon_ij


# Our data is more complicated than there, there are other factors
# Suppose we add the movie scent of a woman
set.seed(1)
options(digits = 2)
m_6 <- "Scent of a Woman"
Q <- cbind(c(1 , 1, 1, -1, -1, -1), 
           c(1 , 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(P) <- 1:nrow(X)
X <- jitter(P%*%t(Q), factor=1)
X %>% knitr::kable(align = "c")
# We now see another factor, those who like, dislike or don't care about
# Al Pacino movies
# The correlation is a bit more complicated now and needs a second factor
cor(X)
P
# We can add new coefficients for our new factor
t(Q) %>% knitr::kable(aling="c")
# Our two factors are shown above, the first about genre, the second about
# Al Pacino movies. We also have two sets of coefficients to describe the users
six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[,six_movies]
cor(tmp, use="pairwise.complete")

# Our model now becomes
# Y_ui = mu + b_i + b+u + pu1_qi1 + pu2_qi2 + Epsilon_ij
# We should be able to fit this model using for example, the least square method

# For the Netflix challenge though, they use regularisation which penalised the
# additional factors that we just mentioned for large values of p and q

# How can we model these factors instead of defining them ourselves?
# We can use PCA or SDV



################################################################
# SINGULAR VALUE DECOMPOSITION & PRINCIPAL COMPONENT ANALYSIS #
###############################################################

# The previous section is very much related to SVD and PCA
# They are complicated concepts

# SVD for example, can be thought of as an algorithm that finds the vectors
# p and q that permit us to write the matrix of residuals r as m rows and
# n columns in the following way:

# r_ui = pu1_qi1 + ... + pum_qim

# With the added bonus that the variability is decreasing and also that the 
# ps are uncorrelated to each other. The algorithm also computes these
# variabilities so that we can know how much of the matrix's total variability
# is explained as we add new terms

# This may permit us to see that with just a few terms we can explain most of 
# the variability

# To compute the decomposition we will make all NAs zero
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

# The vectors q are called the Principal Components and are stored in this matrix
dim(pca$rotation)
# While the vectors p are the user effects and are stored in this matrix
dim(pca$x)

# The PCA function returns a component with the variability of each of the principal
# components and we can access it like this and plot it
plot(pca$sdev)

# We can see that just with a few of these components we can explain a large part
# of the data
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)


# To see that the principal components are actually capturing something important
# we can plot for example, the first two components, but label the points with 
# the movie that each is related to
library(ggrepel)
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

# Just by looking briefly we can see the differences
# Critically acclaimed movies
pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)
# Hollywood blockbusters
pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

# Nerd favourites
pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)
# Artsy, independent films
pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)


# Further analysis
# This is complicated because of the missing cells in the matrix data
# The recommenderlab package will try to fit the matrix factorisation model




### COMPREHENSION CHECK - MATRIX FACTORISATION ###

# In this exercise set, we will be covering a topic useful for understanding matrix
# factorization: the singular value decomposition (SVD). SVD is a mathematical result
# that is widely used in machine learning, both in practice and to understand the
# mathematical properties of some algorithms. This is a rather advanced topic and to
# complete this exercise set you will have to be familiar with linear algebra concepts
# such as matrix multiplication, orthogonal matrices, and diagonal matrices.

# The SVD tells us that we can decompose an N×p matrix Y with p<N as 
# Y = U D V^??? 
# with U and V orthogonal of dimensions N×p and p×p respectively and D a p×p diagonal
# matrix with the values of the diagonal decreasing: 
  #   d1,1 ??? d2,2 ??? .dp,p

# In this exercise, we will see one of the ways that this decomposition can be useful.
# To do this, we will construct a dataset that represents grade scores for 100 students
# in 24 different subjects. The overall average has been removed so this data 
# represents the percentage point each student received above or below the average test
# score. So a 0 represents an average grade (C), a 25 is a high grade (A+), and 
# a -25 represents a low grade (F). You can simulate the data like this:
set.seed(1987)
#if using R 3.6 or later, use `set.seed(1987, sample.kind="Rounding")` instead
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))


# Our goal is to describe the student performances as succinctly as possible.
# For example, we want to know if these test results are all just a random
# independent numbers. Are all students just about as good? Does being good
# in one subject imply you will be good in another? How does the SVD help 
# with all this? We will go step by step to show that with just three 
# relatively small pairs of vectors we can explain much of the variability 
# in this 100×24 dataset.

# You can visualize the 24 test scores for the 100 students by
# plotting an image:
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}
my_image(y)
# The students that test well are at the top of the image and there 
# seem to be three groupings by subject.


# You can examine the correlation between the test scores like this:
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
# There is correlation among all tests, but higher if the tests are in
# science and math and even higher within each subject.


# Remember that orthogonality means that U???U and V???V are equal to the
# identity matrix. This implies that we can also rewrite the decomposition as
#   YV = UD or U^???Y = DV^??? 

# We can think of YV and U???V as two transformations of Y that preserve the
# total variability of Y since U and V are orthogonal.

# Use the function svd() to compute the SVD of y. This function will return
# U, V, and the diagonal entries of D.
s <- svd(y)
names(s)

# You can check that the SVD works by typing:
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

# Compute the sum of squares of the columns of Y and store them in ss_y.
# Then compute the sum of squares of columns of the transformed YV and
# store them in ss_yv. Confirm that sum(ss_y) is equal to sum(ss_yv).

# What is the value of sum(ss_y) (and also the value of sum(ss_yv))?
ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)
sum(ss_yv)

# We see that the total sum of squares is preserved. This is because V is
# orthogonal. Now to start understanding how YV is useful, 
# plot ss_y against the column number and then do the same for ss_yv.
# The plots can be made using plot(ss_y) and plot(ss_yv).
# We see that the variability of the columns of YV is decreasing.
# Furthermore, we see that, relative to the first three, the variability
# of the columns beyond the third is almost 0.
plot(ss_y)
plot(ss_yv)




### COMPREHENSION CHECK - DIMENSION REDUCTION

# We want to explore the tissue_gene_expression predictors by plotting them.
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

# We want to get an idea of which observations are close to each other, 
# but, as you can see from the dimensions, the predictors are 
# 500-dimensional, making plotting difficult. Plot the first two principal
# components with color representing tissue type.
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

# What is the correlation
avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])


# We see an association with the first PC and the observation averages.
# Redo the PCA but only after removing the center.
# Part of the code is provided for you.
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()


# For the first 10 PCs, make a boxplot showing the values for each tissue
# For the 7th PC, which two tissues have the greatest median difference?
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}


# Plot the percent variance explained by PC number. 
# Hint: use the summary function.
# How many PCs are required to reach a cumulative percent 
# variance explained greater than 50%?
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))  #from Q3
pc <- prcomp(x)
var_explained <- cumsum(pc$sdev^2/sum(pc$sdev^2))
plot(var_explained)
ifelse(var_explained > 0.5, 1, 0)

# Elegant solution
plot(summary(pc)$importance[3,])




### COMPREHENSION CHECK - CLUSTERING ###

# The textbook section on clustering may help with some of these exercises
# These exercises will work with the tissue_gene_expression dataset, 
# which is part of the dslabs package.

# Load the tissue_gene_expression dataset.
# Remove the row means and compute the distance between each observation
# Store the result in d
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))


# Make a hierarchical clustering plot and add the tissue types as labels
# You will observe multiple branches.
# Which tissue type is in the branch farthest to the left?
# Which of the following lines of code correctly does this computation?
h <- hclust(d)
plot(h)
# Makes a cool dendogram


# Select the 50 most variable genes. Make sure the observations show up 
# in the columns, that the predictor are centered, and add a color bar
# to show the different tissue types. 
# Hint: use the ColSideColors argument to assign colors. 
# Also, use col = RColorBrewer::brewer.pal(11, "RdBu") for a better
# use of colors.
# Part of the code is provided for you here:
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)







