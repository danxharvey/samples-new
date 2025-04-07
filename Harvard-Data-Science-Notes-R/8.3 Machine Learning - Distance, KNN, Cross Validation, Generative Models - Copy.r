#############
# DISTANCE #
############

# To define the distance between 2 points on the cartesian plan
# Point A and Point B (Euclidean Distance)
# dist(A, B) = sqrt( (Ax-Bx)^2 + (Ay-By)^2 )

# In 1 dimension it is simply the absolute difference between numbers
# dist(A, B) = sqrt( (A-B)^2 ) = |A-B|

# To see how this links to our numbers dataset of 784 features/pixels
# We can create a random data set looking at just 2s and 7s
set.seed(0)
if(!exists("mnist")) mnist <- read_mnist
ind <- which(mnist$train$labels %in% c(2, 7)) %>% sample(500)
x <- mnist$train$images[ind,]   # Predictors
y <- mnist$train$labels[ind]    # Labels

# To compute distance we need to know what the points are, since
# mathematical distance is computed between 2 points

# With high dimensional data points are no longer on the cartesian
# plan and we can no longer visualise them, we need to think abstractly

# A predictor xi is defined as a point in 784 dimensional space
# Xi = (xi,1 , xi,2 , ... , xi,784 )T

# Once defined like this the eudclidean distance between x1 and x2
# can be defined as:
# dist(1, 2) = sqrt( Sigma(j=1-784) (x1j-x2j)^2 )
# Sum the individual distances over 784 dimensions and sqrt

# Using R, let's look at the first 3 observations
y[1:3]
# The vectors for predictions will saved here:
x1 <- x[1,]
x2 <- x[2,]
x3 <- x[3,]

# Looking at distances we expect the distances between x1 and x2
# to be the smallest as they are both 7s
sqrt(sum((x1-x2)^2))
sqrt(sum((x1-x3)^2))
sqrt(sum((x2-x3)^2))

# Using matrix algebra we can use crossproduct as a faster method
sqrt(crossprod(x1-x2))
sqrt(crossprod(x1-x3))
sqrt(crossprod(x2-x3))

# Using the dist function we can do this for all observations
d <- dist(x)
class(d)

# We need to coerce the results into a matrix.  Take the first 3
as.matrix(d)[1:3, 1:3]
# We can quickly see an image of the distance function
image(as.matrix(d))
# If we order by labels (observations) we can see that in general
# the 2s are closer to each other and the 7s are closer to each other
image(as.matrix(d)[order(y), order(y)])
# More red = more uniformity in how the image is drawn


# We can also compute the distance between predictors. If N is the 
# number of observations, the distance between 2 predictors, say the 
# first and second, can be computed as:
# dist(1,2) = sqrt( sigma(i=1-N) (xi,1 - xi,2)^2 )

# To compute the distance between all pairs of 784 predictors can be done
# by transposing the matrix first and then using dist function
d <- dist(t(x))
dim(as.matrix(d))

# If we pick a predictor, a pixel we can see which pixels are close
# ie they either ink together, or don't ink together
# Here's what it looks like
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))

# We can see a spatial pattern, pixels that are close in distance on the image
# are also close mathematically


# COMPREHENSION CHECK
library(dslabs)
data(tissue_gene_expression)

# This dataset includes a matrix x:
dim(tissue_gene_expression$x)

# This matrix has the gene expression levels of 500 genes from 189 biological
# samples representing seven different tissues.
# The tissue type is stored in y:
table(tissue_gene_expression$y)

# Which of the following lines of code computes the Euclidean distance between
# each observation and stores it in the object d?
d <- dist(tissue_gene_expression$x)


# Extract specific observations
cerebellum <- tissue_gene_expression$x[1:2,]
colon <- tissue_gene_expression$x[39:40,]
endometrium <- tissue_gene_expression$x[73:74,]
# Combine
mat <- rbind(cerebellum, colon, endometrium)
# Check distance
d <- dist(mat)
as.matrix(d)
image(as.matrix(d))

# Elegant solution
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]
image(as.matrix(d))



########
# Knn #
#######

# k-nearest neighbours algorithm
# Related to smoothing, similar to bin smoothing but easier to adapt
# We will use the previous mnist subset of 7s and 2s

# We have to estimate the conditional probability because some of the data
# is noisy, it is not close to zero or 1 and could go either way

# We look at the k nearest points and then average these points
# The set of points used is called a neighbourhood

# Due to the connection between conditional expectations and conditional
# probability this gives us the estimated conditional probability
# phat(x1,x2), just like bin smoothers gave us the estimated trend

# We control the flexibility of our estimate through k
# larger k = smoother estimate, smaller k = flexible but wigglier estimate

# We start by comparing to logistic regression which is the standard
# we need to beat
library(caret)
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data=y_hat_logistic, reference=mnist_27$test$y)$overall[1]

# knn3 algorithm
# 2 ways to call
# First method - data frame containing the data and the formula
# formula is outcome ~ predictor1 + predictor2 + predictor3 + ...
# for all predictors use .  y~.
# This is quicker and easier to write
knn_fit <- knn3(y~., data=mnist_27$train)

# Second method - uses a matrix of predictors and a vector of outcomes
# This is better for large datasets
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)

# This defaults to k = 5 but we can set it explicitly
knn_fit <- knn3(y~., data=mnist_27$train, k=5)


# Because this dataset is balanced, equal 7s and 2s we will use accuracy
# as the measure of the algorithms success (no bias here)
# and because we care equally about sensitivity and specificity both
# mistakes are equally bad (TP and FN)

# The predict function for this knn function produces either a probability
# for each class, or it could produce an outcome that maximises the
# probability, the outcome with the highest probability

y_hat_knn <- predict(knn_fit, mnist_27$test, type="class")
# type="class" will give us the actual outcomes that are predicted
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]


###################################
# OVERSMOOTHING AND OVERTRAINING #
##################################

# To see why we made improvements in accuracy we can look at a visualisation
# of the true conditional probability and the estimate from knn 5 neighbours
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}
p1 <- plot_cond_prob() + ggtitle("True conditional probability")
p2 <- plot_cond_prob(predict(knn_fit, mnist_27$true_p)[,2]) +
  ggtitle("kNN-5 estimate")
library(gridExtra)
grid.arrange(p1, p2, nrow=1)

# You can see that the knn has the essence of the shape of the true probability
# (only know because Rafa owns the dataset, not normally known)
# This means that we have already beaten the logistic regression straight line

# The islands of blue that are present in the red section are due to overtraining
# To understand overtraining it is spotted by higher accuracy on the training
# set when compared to the test set (i.e. you over-tuned the data)
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class") 
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")  
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

# Overtraining is at its worst when k=1 because the estimate is based off the
# actual y value observed, it becomes its own neighbour
knn_fit1 <- knn3(y~., data=mnist_27$train, k=1)
y_hat_knn1 <- predict(knn_fit1, mnist_27$train, type = "class") 
confusionMatrix(data = y_hat_knn1, reference = mnist_27$train$y)$overall["Accuracy"]
y_hat_knn1 <- predict(knn_fit1, mnist_27$test, type = "class")  
confusionMatrix(data = y_hat_knn1, reference = mnist_27$test$y)$overall["Accuracy"]

# Graphically, we can see this as the blue data points on the left being
# taken as gospel truth in the training set however in the test set they are 
# gone or replaced by red data points which leads to prediction errors
p1 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$train, aes(x_1, x_2, color= y),
             pch=21, show.legend = FALSE) +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Train set")
p2 <- mnist_27$true_p %>% 
  mutate(knn = predict(knn_fit1, newdata = .)[,2]) %>%
  ggplot() +
  geom_point(data = mnist_27$test, aes(x_1, x_2, color= y), 
             pch=21, show.legend = FALSE) +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(aes(x_1, x_2, z = knn), breaks=c(0.5), color="black") +
  ggtitle("Test set")
grid.arrange(p1, p2, nrow=1)
# The estimated conditional probability followed the train data too much


# If we pick a much higher k=401, we get a smooth trend, over smooth
# The accuracy drops to a level similar to logistic regression and we see
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]

# From the graphs that it actually looks similar to the logistic regression line
p1 <- plot_cond_prob(predict(fit_glm, mnist_27$true_p)) +
  ggtitle("Logistic regression")
p2 <- plot_cond_prob(predict(knn_fit_401, mnist_27$true_p)[,2]) +
  ggtitle("kNN-401")
grid.arrange(p1, p2, nrow=1)
# The size of k is almost half the data (401/1000) so it is oversmoothed and
# not flexible enough


# How do we pick k?  5 too small, 401 too big, let's try all the odd numbers
# between 3 and 251 and the df_map function
ks <- seq(3, 251, 2)

# For comparative purposes for this lecture only, we will compare the training
# and test sets, note that we never do this in reality, we only use the test set
# However this shows us how overtraining works
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})
accuracy %>% mutate(k = ks) %>%
  gather(set, accuracy, -k) %>%
  mutate(set = factor(set, levels = c("train", "test"))) %>%
  ggplot(aes(k, accuracy, color = set)) + 
  geom_line() +
  geom_point() 

# Using the k=41 best accuracy from test set
p1 <- plot_cond_prob() + ggtitle("True conditional probability")
knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 41)
p2 <- plot_cond_prob(predict(knn_fit, newdata = mnist_27$true_p)[,2]) +
  ggtitle("kNN-41 estimate")
grid.arrange(p2, p1, nrow=1)
# We see a max accuracy of 0.86
max(accuracy$test)

# Is this realistic in the real world?  The answer is no.
# We broke a golden rule of ML by selecting k from the test set
# Cross-validation provides a way to estimate the expected loss for a 
# given method using only the training set

# COMPREHENSION CHECK
# Previously, we used logistic regression to predict sex based on height. 
# Now we are going to use knn to do the same. Set the seed to 1, then use the
# caret package to partition the dslabs heights data into a training and test
# set of equal size. Use the sapply() function to perform knn with k values
# of seq(1, 101, 3) and calculate F1 scores with the F_meas() function using
# the default value of the relevant argument.
library(dslabs)
data("heights")
y = heights$sex
x = heights$height
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

k <- seq(1, 101, 3)
F1 <- sapply(k, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  F_meas(data = y_hat, reference = test_set$sex)
})
best_k <- k[which.max(F1)]
best_k
max(F1)

# To plot the outcomes
F_values <- data.frame( k = k, F1 = F1 )
F_values %>% ggplot( aes( x = k, y = F1 ) ) +
  geom_point() +
  geom_smooth()


# Next we will use the same gene expression example used in the Comprehension 
# Check: Distance exercises. You can load it like this:
library(tidyverse)
library(dslabs)
library(caret)
data("tissue_gene_expression")
x <-tissue_gene_expression$x
y <- tissue_gene_expression$y

# First, set the seed to 1 and split the data into training and test sets
# with p = 0.5. 
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
y_train <- y[-test_index]
x_train <- x[-test_index,]
y_test <- y[test_index]
x_test <- x[test_index,]
train <- list(x = x_train, y = y_train)
test <- list(x = x_test, y = y_test)

# Then, report the accuracy you obtain from predicting tissue type using
# KNN with k = seq(1, 11, 2) using sapply() or map_df(). 
# Note: use the createDataPartition() function outside of sapply() or map_df()
k <- seq(1, 11, 2)
K_1 <- sapply(k, function(k){
  knn_fit <- knn3(x_train, y_train, k = k)
  y_hat_knn <- predict(knn_fit, x_test, type = "class")
  confusionMatrix(y_hat_knn, y_test)$overall["Accuracy"]
})

# Elegant solution
set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})



############################
# k-fold Cross Validation #
###########################

# For k-fold cross validation, we divide the dataset into a training
# set and a test set. We train our algorithm exclusively on the 
# training set and use the test set only for evaluation purposes.

# For each set of algorithm parameters being considered, we want an
# estimate of the MSE and then we will choose the parameters with 
# the smallest MSE. In k-fold cross validation, we randomly split 
# the observations into k non-overlapping sets, and repeat the
# calculation for MSE for each of these sets. Then, we compute 
# the average MSE and obtain an estimate of our loss. Finally, we 
# can select the optimal parameter that minimized the MSE.

# In terms of how to select k for cross validation, larger values of
# k are preferable but they will also take much more computational
# time. For this reason, the choices of k=5 and k=10 are common.

# We've seen how in ML we want to minimise Mean Squared Error (MSE)
# MSE = E{1/N sigma(i=1-N) (Yhat-Yi)^2} = TRUE ERROR

# When we only have 1 dataset we estimate the MSE like this:
# MSEhat = 1/N sigma(i=1-N) (Yhat-Yi)^2 = APPARENT ERROR

# When dealing with the apparent error ALWAYS remeber:
# 1 - It is a random variable
# 2 - If we train an algorithm on the same data we might overtrain
      # If this happens it will be understatement of the True Error
      # Extreme example was with knn k=1
# Cross-validation helps us alleviate both of these issues


# Think of the True Error, a theoretical quantity, as the average
# of many, many apparent errors obtained from B samples of the data 
# None of them used to train the algorithm
# So we think of the True Error as the average of the Apparent Error
# True Error = MSE = 1/B Sigma(b=1-B) of MSEhat with notation y_i_b

# Break training set into B random samples to make our True Error
# Ths is k-fold cross validation
# We optimise on these B datasets
# We then make our final algorithm on our test set
# Test set is typically 10-20% of the overall training set
# We call our set of parameters lambda
# We fix parameters and use across all training sets
# yhat_i(lambda) denotes prediction obtained for observation i

# To imitate the definition of the expected loss we write
# MSE(lambda) = 1/B Sigma(b=1-B) 1/N sigma(i=1-N) (Yhatib(lambda)-Yib)^2

# We pick M random samples with M = N/k
# y1b, y2b, ..., yMb where b=1, the first fold or subset
# repeat for b = 1 to K

# Then we compute the apparent error on this subset like this:
# MSEhatb(lambda) = 1/M sigma(i=1-M) (yhatib(lambda)-yib)^2
# This is just 1 sample and returns a noisy estimate which is why
# we take k sets as above b = 1 to K
# We get K estimate of the MSE
# MSEhat1(lambda), ... MSEhatk(lambda)

# In our final estimate we compute the average
# MSEhat(lambda) = 1/B sigma(b=1-K) MSEhatb(lambda)
# This is the estimate of our loss

# The final step is to select the lambda that minimises the MSE

# However, the optimisation occurred on training data so now we have
# to compute an estimate of our final algorithm on fresh data...
# Hence this is why we substrated a test set on our training set data
# This is where we compute our final estimate of the MSE

# As time constraints are important in ML algorithms we take the view
# that we optimise on the training subsets, then evaluate on the 
# test set of the training set

# Ideally we'd use larger K in the cross fold validation but it is
# a lot slower, k=100 is 10 times slower than k=10, so 5 and 10 
# are quite popular

# One way to improve the variance of our final estimate is to take 
# more samples.  To do this we use the non-partioned version of the 
# training set and one popular method is to take samples with
# replacement k times.  This approach has some advantages not
# discussed here and is referred to as bootstrapping, this is the 
# default method used in the caret package.


# COMPREHENSION CHECK
# Generate a set of random predictors and outcomes using the
# following code:
library(tidyverse)
library(caret)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,sample(p, 100)]

# Because x and y are completely independent, you should not be able
# to predict y using x with accuracy greater than 0.5. Confirm this 
# by running cross-validation using logistic regression to fit the
# model. Because we have so many predictors, we selected a random
# sample x_subset. Use the subset when training the model.

# Which code correctly performs this cross-validation?
install.packages("randomForest")
fit <- train(x_subset, y, method = "glm")
fit$results

# Now, instead of using a random selection of predictors, we are going
# to search for those that are most predictive of the outcome. We can
# do this by comparing the values for the  y=1  group to those in the
# y=0  group, for each predictor, using a t-test. You can do perform
# this step like this:
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
# Which of the following lines of code correctly creates a vector of
# the p-values called pvals?
pvals <- tt$p.value

# Create an index ind with the column numbers of the predictors that
# were "statistically significantly" associated with y. Use a p-value
# cutoff of 0.01 to define "statistically significantly."
# How many predictors survive this cutoff?
ind <- ifelse(pvals < 0.01, 1, 0)
sum(ind)
# Elegant solution
ind <- which(pvals <= 0.01)
length(ind)


# Now re-run the cross-validation after redefinining x_subset to be the subset
# of x defined by the columns showing "statistically significant"
# association with y. What is the accuracy now?
x_subset <- x[,ind]
fit <- train(x_subset, y, method = "glm")
fit$results


# Re-run the cross-validation again, but this time using kNN. Try out the
# following grid k = seq(101, 301, 25) of tuning parameters. Make a plot of the
# resulting accuracies.
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


# In the previous exercises, we see that despite the fact that x and y are
# completely independent, we were able to predict y with accuracy higher than 70%.
# We must be doing something wrong then. What is it?

# We used the entire dataset to select the columns used in the model. correct

# Because we used the entire dataset to select the columns in the model, the
# accuracy is too high. The selection step needs to be included as part of the
# cross-validation algorithm, and then the cross-validation itself is performed 
# after the column selection step.
# As a follow-up exercise, try to re-do the cross-validation, this time including
# the selection step in the cross-validation algorithm. The accuracy should now 
# be close to 50%.
indexes <- createDataPartition(y, times = 5, p = 0.2)
dat <- data.frame(y=y, data.frame(x))
res <- sapply(indexes, function(test_index){
  
  train_set <- slice(dat, -test_index)
  test_set <- slice(dat, test_index)
  
  pvals <- colttests(as.matrix(train_set[,-1]), train_set$y)$p.value
  
  ind <- c(TRUE, pvals <= 0.01)
  train_set <- train_set[, ind]
  
  fit <- glm(y ~ ., data = train_set, family = "binomial")
  y_hat <- ifelse(predict(fit, newdata = test_set[, ind], type = "response") > 0.5, 1, 0) %>%
    factor()
  mean(y_hat == test_set$y)
})
res


# Use the train() function with kNN to select the best k for predicting tissue
# from gene expression on the tissue_gene_expression dataset from dslabs. 
# Try k = seq(1,7,2) for tuning parameters. For this question, do not split the
# data into test and train sets (understand this can lead to overfitting, but 
# ignore this for now).
# What value of k results in the highest accuracy?
data("tissue_gene_expression")
fit <- with(tissue_gene_expression, train(x, y, method = "knn", tuneGrid = data.frame( k = seq(1, 7, 2))))
ggplot(fit)
fit$results



##################
# BOOTSTRAPPING #
#################

# When we don't have access to the entire population, we can use bootstrap to
# estimate the population median

# The bootstrap permits us to approximate a Monte Carlo simulation without access 
# to the entire distribution. The general idea is relatively simple. We act as if
# the observed sample is the population. We then sample datasets (with replacement)
# of the same sample size as the original dataset. Then we compute the summary 
# statistic, in this case the median, on this bootstrap sample.

# Note that we can use ideas similar to those used in the bootstrap in cross
# validation: instead of dividing the data into equal partitions, we simply 
# bootstrap many times.

# Plot median income to see it approximately normal
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

# Median income
m <- median(income)
m

set.seed(1)
#use set.seed(1, sample.kind="Rounding") instead if using R 3.6 or later
N <- 250
X <- sample(income, N)
M<- median(X)           # Sample Median M
M

# Can we see a confidence interval using Monte Carlo?
library(gridExtra)
B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})
p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)
# View mean and SD
mean(M)
sd(M)
# In the past we've referred to the CLT as we don't have access to the entire
# population, however the CLT refers to the averages, and we're interested in 
# the median

# The bootstrap allows us to approximate a monte carlo without access to the 
# entire distribution. We act as if the sample is the entire population and 
# sample it many times with replacement

# Then we compute the summary statistic, in this case the median, on what is
# called the bootstrap sample

# Theory tells us that the bootstrap statistic approximates the distribution
# of our actual statistic

# This is how we construct bootstrap samples in an approximate distribution
B <- 10^4
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline()

# Look at the quantiles we need for a 95% CI.  They're quite close
# Used 10^4 above as 10^5 was hanging
quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))

# This is better than if we mindlessly use the CLT which is quite wrong
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

# if we know that the distribution is approximately normal, we use a bootstrap
mean(M) + 1.96 * sd(M) * c(-1,1)
mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)


# COMPREHENSION CHECK

# The createResample() function can be used to create bootstrap samples.
# For example, we can create the indexes for 10 bootstrap samples for the
# mnist_27 dataset like this:
library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

# How many times does 3, 4, and 7 appear in the first resample index
sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)


# We see that some numbers appear more than once and others appear no times.
# This has to be this way for each dataset to be independent. Repeat the 
# exercise for all the resampled indexes.
# What is the total number of times that 3 appears in all of the resampled 
# indexes?
x = sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)


# Generate a random dataset using the following code:
y <- rnorm(100, 0, 1)
# Estimate the 75th quantile, which we know is qnorm(0.75), with the
# sample quantile: quantile(y, 0.75)
quantile(y, 0.75)

# Now, set the seed to 1 and perform a Monte Carlo simulation with 10,000
# repetitions, generating the random dataset and estimating the 75th
# quantile each time. What is the expected value and standard error of 
# the 75th quantile?
# set.seed(1) # # if R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
n <- 10000
q <- replicate(n, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(q)
sd(q)


# In practice, we can't run a Monte Carlo simulation. Use the sample:
# Set the seed to 1 again after generating y and use 10 bootstrap samples 
# to estimate the expected value and standard error of the 75th quantile.
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)



# Repeat the exercise above but with 10,000 bootstrap samples instead of 10.
# Set the seed to 1 first.
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)



######################
# GENERATIVE MODELS #
#####################

# Discriminative approaches estimate the conditional probability directly 
# and do not consider the distribution of the predictors. 

# Generative models are methods that model the joint distribution and X
# (we model how the entire data, X and Y, are generated).

# Bayes' rule: 
# p(x)=Pr(Y=1|X=x)=fX|Y=1(X)Pr(Y=1)fX|Y=0(X)Pr(Y=0)+fX|Y=1(X)Pr(Y=1) 
# with fX|Y=1 and fX|Y=0 representing the distribution functions of the
# predictor X for the two classes Y=1 and Y=0

# Generating train and test set
library("caret")
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# Estimating averages and standard deviations
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params

# Estimating the prevalence
pi <- train_set %>% summarize(pi=mean(sex=="Female")) %>% pull(pi)
pi

# Getting an actual rule
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))
p_hat_bayes


############################
# CONTROLLING PREVALENCE #
##########################

# The Naive Bayes approach includes a parameter to account for differences
# in prevalence  ??=Pr(Y=1) . If we use hats to denote the estimates, we can
# write  p(x)^  as: 
#  p^(x)=Pr(Y=1|X=x)=f^X|Y=1(x)??^f^X|Y=0(x)(1?????^)+f^X|Y=1(x)Pr(Y=1)

# The Naive Bayes approach gives us a direct way to correct the imbalance 
# between sensitivity and specificity by simply forcing ??^ to be whatever
# value we want it to be in order to better balance specificity and
# sensitivity. 

# Computing sensitivity
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Computing specificity
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# Changing the cutoff of the decision rule
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

# Draw plot
qplot(x, p_hat_bayes_unbiased, geom = "line") +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_vline(xintercept = 67, lty = 2)



##############
# QDA & LDA #
#############

# QDA - Quadratic Discriminate Analysis is a version of naive Bayes
# We assume the conditional probabilities for the predictors are
# multivariate normal

data("mnist_27")
# There are 2 predictors here so we assume it is bivariate normal
# This implies that we need to estimate
# 2 averages, 2SDs and acorrelation for each case
# Once we have these we can approximate the conditional distribution
# fx1,x2 | Y=1
# fx1,x2 | Y=0

# We can easily estimate these parameters from the data using this code
params <- mnist_27$train %>% group_by(y) %>%
          summarise(avg_1=mean(x_1), avg_2=mean(x_2),
                    sd_1=sd(x_1), sd_2=sd(x_2), r=cor(x_1,x_2))
params

# We can show a contour plot to give an estimate of the 2 normal densities
# We show a curve representing a region that includes 95% of the points
# Contour plots
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm", lwd = 1.5)
# This defines the estimate for the conditional probability of
# Y=1 | x1 and x2

# We can then use the carat package to fit the model and obtain predictors
library(caret)
train_qda <- train(y~., method="qda", data=mnist_27$train)
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(y_hat, mnist_27$test$y)$overall["Accuracy"]

# It is not quite as good as a fit as the kernal smooting that we saw in a 
# previous video and there's a reason for this. Mathematically it can be 
# shown that the boundary must be a quadratic function of the form
# x2 = ax^2 + bx + c
# One reason QDA does not work as well is perhaps the assumptions of 
# normality do not quite hold

# Draw separate plots for 2s and 7s
# Although the bivariate normal approximate for the 2 seems reasonable,
# it, for the 7 it does seem to be off, notice the slight curvature
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm") +
  facet_wrap(~y)

# Although QDA worked quite well here it gets harder as the number of
# predictors increases.  This formula tells u show many parameters to estimate
# K*(2p+p*(p-1)/2) and it gets big pretty fast
# Once the number of paramters reaches the size of our data overfit occurs

# One solution is to assume that the correlation structure is the same for
# all classes. This reduces the numbers of parameters we need to estimate
# In the code we could just compute one set of SD and correlation
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))

params <- params %>% mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r = mean(r))
# Now the elipses are the same size and shape as they have the same SD and
# correlations. That's the assumption, when we force this assumption the 
# boundaries are aligned as with logistic regression.
# We call this Linear Discriminat Analysis - LDA
# The plot now becomes a straight line like logistic regression

# Because of this straight line, we see that the lack of flexibility
# does not allow us to obtain a good estimate and the accuracy is quite low
train_lda <- train(y ~., method = "lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]


####################################
# CASE STUDY: MORE THAN 3 CLASSES #
###################################

# We're going to look at a slightly more complex example with more than 2
# predictors.  The number reader example but with 1, 2 and 7
# First we generate that dataset
if(!exists("mnist"))mnist <- read_mnist()
set.seed(3456)    #use set.seed(3456, sample.kind="Rounding") in R 3.6 or later
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)
# get the quadrants
# temporary object to help figure out the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)
# binarize the values. Above 200 is ink, below is no ink
x <- x > 200 
# cbind proportion of pixels in upper right quadrant and proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x),
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 

train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])

test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])

# Plot of the data for the training set
train_set %>%  ggplot(aes(x_1, x_2, color=y)) + geom_point()

# As an example we'll fit a QDA model
train_qda <- train(y ~ ., method = "qda", data = train_set)

# Now we are predicting 3 conditional probabilities
predict(train_qda, test_set, type = "prob") %>% head()

# Now when we predict we get outcomes of 1s, 2s and 7s
predict(train_qda, test_set) %>% head()

# The confusion matrix is a 3 by 3 matrix
confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]

# We can plot how the regions look for 1, 2, 7 based on conditional probability
GS <- 150
new_x <- expand.grid(x_1 = seq(min(train_set$x_1), max(train_set$x_1), len=GS),
                     x_2 = seq(min(train_set$x_2), max(train_set$x_2), len=GS))
new_x %>% mutate(y_hat = predict(train_qda, new_x)) %>%
  ggplot(aes(x_1, x_2, color = y_hat, z = as.numeric(y_hat))) +
  geom_point(size = 0.5, pch = 16) + 
  stat_contour(breaks=c(1.5, 2.5),color="black") + 
  guides(colour = guide_legend(override.aes = list(size=2)))


# Let's see how it looks for LDA
train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]
# We see that the accuracy is worse and it's because the boundary regions
# have to be lines
new_x %>% mutate(y_hat = predict(train_lda, new_x)) %>%
  ggplot(aes(x_1, x_2, color = y_hat, z = as.numeric(y_hat))) +
  geom_point(size = 0.5, pch = 16) + 
  stat_contour(breaks=c(1.5, 2.5),color="black") + 
  guides(colour = guide_legend(override.aes = list(size=2)))


# The results for knn are much better, look at how high the accuracy is
train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]

# We can see that the estimated probability is much more flexible
new_x %>% mutate(y_hat = predict(train_knn, new_x)) %>%
  ggplot(aes(x_1, x_2, color = y_hat, z = as.numeric(y_hat))) +
  geom_point(size = 0.5, pch = 16) + 
  stat_contour(breaks=c(1.5, 2.5),color="black") + 
  guides(colour = guide_legend(override.aes = list(size=2)))


# Note that the QDA and LDA are not working well due to lack of fit
# We can see this on the plot and note that at least the 1s are definitely
# not bivariate normally distributed.
train_set %>% mutate(y = factor(y)) %>% 
  ggplot(aes(x_1, x_2, fill = y, color=y)) + 
  geom_point(show.legend = FALSE) + 
  stat_ellipse(type="norm") 

# Generative models can be very powerful but only when we're able to
# successfully approximate the joint distribution of the predictor's
# condition on each class


# COMPREHENSION CHECK

# Create a dataset of samples from just cerebellum and hippocampus, two 
# parts of the brain, and a predictor matrix with 10 randomly selected 
# columns using the following code:
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

# Use the train() function to estimate the accuracy of LDA
fit_lda <- train(x, y, method = "lda")
fit_lda$results["Accuracy"]


# In this case, LDA fits two 10-dimensional normal distributions. Look at
# the fitted model by looking at the finalModel component of the result of
# train(). Notice there is a component called means that includes the 
# estimated means of both distributions. Plot the mean vectors against each
# other and determine which predictors (genes) appear to be driving the
# algorithm.

# Which TWO genes appear to be driving the algorithm
# (i.e. the two genes with the highest means)?
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


# Create a dataset of samples from just cerebellum and hippocampus, two parts 
# of the brain, and a predictor matrix with 10 randomly selected columns 
# using the following code:
library(dslabs)      
library(caret)
data("tissue_gene_expression")
set.seed(1993) #set.seed(1993, sample.kind="Rounding") if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
# Repeat the exercise in Q1 with QDA.
fit_qda <- train(x, y, method = "qda")
fit_qda$results["Accuracy"]

# Which TWO genes drive the algorithm when using QDA instead of LDA
# (i.e. the two genes with the highest means)?
t(fit_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()



# One thing we saw in the previous plots is that the values of the predictors
# correlate in both groups: some predictors are low in both groups and others
# high in both groups. The mean value of each predictor found in colMeans(x) 
# is not informative or useful for prediction and often for purposes of
# interpretation, it is useful to center or scale each column. This can be
# achieved with the preProcess argument in train().
# Re-run LDA with preProcess = "center". Note that accuracy does not change, 
# but it is now easier to identify the predictors that differ more between 
# groups than based on the plot made in Q2.
# Which TWO genes drive the algorithm after performing the scaling?
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

# Use the train() function to estimate the accuracy of LDA
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]
# Check plot of predictors after scaling for differences
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(predictor_name, hippocampus)) +
  geom_point() +
  coord_flip()

# You can see that it is different genes driving the algorithm now. This is
# because the predictor means change.
# In the previous exercises we saw that both LDA and QDA approaches worked 
# well. For further exploration of the data, you can plot the predictor 
# values for the two genes with the largest differences between the two 
# groups in a scatter plot to see how they appear to follow a bivariate 
# distribution as assumed by the LDA and QDA approaches, coloring the points
# by the outcome, using the following code:
d <- apply(fit_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)



# Now we are going to increase the complexity of the challenge slightly. 
# Repeat the LDA analysis from Q5 but using all tissue types. Use the 
# following code to create your dataset:
library(dslabs)      
library(caret)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

# Train LDA model and check accuracy
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]

# Check plot of predictors after scaling for differences
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(predictor_name, hippocampus)) +
  geom_point() +
  coord_flip()
# Now we see a different predictor than before

