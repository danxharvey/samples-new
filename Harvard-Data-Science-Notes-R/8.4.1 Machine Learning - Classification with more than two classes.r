#####################
# TREES MOTIVATION #
####################

# LDA and QDA are not meant to be used with many predictors p because the
#number of parameters needed to be estimated becomes too large.

# Curse of dimensionality:
# For kernel methods such as kNN or local regression, when they have multiple 
# predictors used, the span/neighborhood/window made to include a given percentage
# of whe  the data become large. With larger neighborhoods, our methods lose
# flexibility. The dimension here refers to the fact that when we have p predictors, the
# distance between two observations is computed in p-dimensional space.

# Interval line on a straight line
rafalib::mypar()
x <- seq(0,1,len=100)
y <- rep(1, 100)
plot(x,y, xlab="",ylab="", cex=0.25, yaxt="n", xaxt="n",type="n")
lines(x[c(15,35)], y[c(15,35)], col="blue",lwd=3)
points(x,y, cex = 0.25)
points(x[25],y[25],col="blue", cex = 0.5, pch=4)
text(x[c(15,35)], y[c(15,35)], c("[","]"))

# Blue square on a background
tmp <- expand.grid(1:10, 1:10)
x <- tmp[,1]
y <- tmp[,2]
rafalib::mypar()
plot(x,y, xlab="",ylab="", cex=0.25, yaxt="n", xaxt="n",type="n")
polygon(c(x[25]-0.5, x[25]-0.5, x[25]+0.5, x[25]+0.5),
        c(y[25]-0.5, y[25]+0.5, y[25]+0.5, y[25]-0.5), col="blue")
points(x,y, cex = 0.25)
points(x[25],y[25], cex = 0.5, pch=4)

# Bigger blue square on a background
rafalib::mypar()
plot(x,y, xlab="",ylab="", cex=0.25, yaxt="n", xaxt="n",type="n")
polygon(c(x[25]-sqrt(10)/2, x[25]-sqrt(10)/2, x[25]+sqrt(10)/2, x[25]+sqrt(10)/2),
        c(y[25]-sqrt(10)/2, y[25]+sqrt(10)/2, y[25]+sqrt(10)/2, y[25]-sqrt(10)/2),
        col="blue")
points(x,y, cex = 0.25)
points(x[25],y[25], cex = 0.5, pch=4)

# Plot of 10% data shown as 0.1^(1/p) - See how p quickly uses most data points
# It is no longer smoothing very quickly
library(tidyverse)
p <- 1:100
qplot(p, .1^(1/p), ylim = c(0,1))



###############################################
# Classification and Regression Trees (CART) #
##############################################

# A tree is basically a flow chart of yes or no questions. The general idea of
# the methods we are describing is to define an algorithm that uses data to 
# create these trees with predictions at the ends, referred to as nodes.

# For illustrative purposes we can try to predict which region Olives comes from
# by using their fatty acid composition as predictors

# Load data
library(tidyverse)
library(dslabs)
data("olive")
olive %>% as_tibble()
table(olive$region)
olive <- select(olive, -area)     # Renove area as not a predictor

# Predict region using KNN
library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)
# We see an accuracy of about 0.97

# Exploratory data analyis of each predictor reveals insights
# Plot distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())
# We see that 1 fatty acide is only present in southern italy
# Another separates northern italy from sardinia
# This implies we should be able to build an algorithm that perfectly predicts

# We can see this if we plot the values of these 2 fatty acidz
# plot values for eicosenoic and linoleic
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
# We can by eye construct a rule to partition the acids like this
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)


# When the outcome is continuous, we call the decision tree method a
# regression tree.

# Regression and decision trees operate by predicting an outcome variable Y by
# partitioning the predictor space.  We can use election data to show this.
# Load data
data("polls_2008")
qplot(day, margin, data = polls_2008)

# The general idea here is to build a decision tree and, at end of each node, 
# obtain a predictor y^. Mathematically, we are partitioning the predictor
# space into J non-overlapping regions, R1, R2, ..., RJ and then for any
# predictor x that falls within region Rj, estimate f(x) with the average
# of the training observations yi for which the associated predictor xi in
# also in Rj.

# To pick j and its value s, we find the pair that minimizes the residual sum
# of squares (RSS): ???i:xiR1(j,s)(yi???y^R1)2+???i:xiR2(j,s)(yi???y^R2)2 

# To fit the regression tree model, we can use the rpart() function in the 
# rpart package
library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)

# visualize the splits 
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# The final estimate fhat(x) looks like this
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# The trees will stop at a certain point otherwise it will partition until
# the prediction becomes the data point and the RSS is zero as it can't
# vary from itself.

# Two common parameters used for partition decision are the complexity 
# parameter (cp) and the minimum number of observations required in a
# partition before partitioning it further (minsplit in the rpart package). 
# minsplit defaults to 20
# minbucket is minimum number of observations within each partition
# default is round(minsplit/3)

# change parameters and set cp = 0 and minsplit = 2
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")


# If we already have a tree and want to apply a higher cp value, we can use
# the prune() function. We call this pruning a tree because we are snipping
# off partitions that do not meet a cp criterion, after initially making
# the tree very big

# prune the tree 
pruned_fit <- prune(fit, cp = 0.01)


# use cross validation to choose the best cp
library(caret)
train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = polls_2008)
ggplot(train_rpart)

# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

# If we apply this to our polls data to see fhat(x)
polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")



##################################
# CLASSIFICATION DECISION TREES #
#################################

# Classification trees, or decision trees, are used in prediction problems
# where the outcome is categorical. 

# Decision trees form predictions by calculating which class is the most
# common among the training set observations within the partition, rather than
# taking the average in each partition.

# We can no longer use RSS as the data is categorical.

# Two of the more popular metrics to choose the partitions are the 
# Gini index and entropy.

# If we define phat_mk as proportion of observations in partition, of class k
# Gini(j) = sigma(k=1-K) Phat_m_k (1-phat_m_k)
# entropy(j) = ???sigma(k=1-K) Phat_m_k log(phat_m_k), 
# with 0xlog(0) defined as 0

# Both of these seek to partition observations into subsets that have the
# same class, known as purity. Note that of a partition m with one class,
# say the first one for example the Phat = 1 for that partition
# Phat_m_1 = 1, Phat_m_2 = 0, ..., Phat_m_k = 0

# When this happens both gini and entropy are 0, the smallest value

# Let's look at an example on the 2 or 7 sample
# fit a classification tree and plot it
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart)
# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
# Note this is better than logistic regression but not as good
# as kernel methods


# If we plot the estimate of the conditional probability it shows the limitations
# of classification trees
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
p1 <- plot_cond_prob(predict(train_rpart, newdata = mnist_27$true_p, type = "prob")[,2]) +
  ggtitle("Decision Tree")
p1


# Pros: Classification trees are highly interpretable and easy to visualize.
# They can model human decision processes and don't require use of dummy 
# predictors for categorical variables.

# Cons: The approach via recursive partitioning can easily over-train and is
# therefore a bit harder to train than. Furthermore, in terms of accuracy, 
# it is rarely the best performing method since it is not very flexible and
# is highly unstable to changes in training data. 



##################
# RANDOM FOREST #
#################

# Random forests are a very popular machine learning approach that addresses 
# the shortcomings of decision trees. The goal is to improve prediction
# performance and reduce instability by averaging multiple decision trees
# (a forest of trees constructed with randomness).

# The general idea of random forests is to generate many predictors, each
# using regression or classification trees, and then forming a final prediction
# based on the average prediction of all these trees. To assure that the
# individual trees are not the same, we use the bootstrap to induce randomness.

# There are two features that help improve prediction and reduce instability
# The first is bootstrap aggregation or bagging:

# First build many decision trees T1,...,TB, using the training set
# For every observatin j in the test set we form a prediction yhat_j using
    # tree T_j
# To obtain a final prediction we combine the trees in 2 different ways:
    # Continous outcomes = average of yhat_j
    # Categorical outcomes = take the majority class across all trees


# The bootstrap allows us to create many trees from 1 training set
# To create B bootstrap trees we create treet Tj
# Tj, j = 1 to B from training set size N
# Create a bootstrap training set by sampling N observations from the
# training set WITH REPLACEMENT

# Build a tree for each of these bootstrap training sets
# Apply algorithm to get a single final prediction

# This is how we apply random forest to 2008 polls data
library(randomForest)
fit <- randomForest(margin~., data=polls_2008)

# Plot to see number of tree vs error
plot(fit)

# We see that by the time we get to 200 trees the algorithm is not changing
# much but for more complex problems, they will require more trees for the 
# algorithm to converge

# Here is the final result for the polls data
polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")
# Note it is somewhat smooth, not liike a step function for individual trees
# It is the average that gives it the somewhat smooth appearance


# Let's apply a random forest to the 2 or 7 data
library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
# Plot conditional probabilities
p1 <- plot_cond_prob(predict(train_rf, newdata = mnist_27$true_p, type = "prob")[,2]) +
  ggtitle("Random Forest")
p1
# The conditional probabilities are much more flexible than a single tree
# This is a bit too wiggly but note we have not optimised the parameters
# We can use the carat package to do this

# We use the Rborist package which is a bit faster
# use cross validation to choose parameter
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
# Plot new conditional probabilities
p2 <- plot_cond_prob(predict(train_rf_2, newdata = mnist_27$true_p, type="prob")[,2]) +
  ggtitle("Random Forest")
p2


# We can control the smoothness of the random forest in several ways
# Limit the size of each node, we can require the number of points to be larger
# We can use a random selection of features to use for the splits
  # We say only use a random subset of predictors for each split
  # Every tree has a different random selection predictors
  # Therefore reduced correlation which improves accuracy
  # The tuning parameter for random forest function is mtry


# A disadvantage of random forests is that we lose interpretability as we're
# averaging hundreds and thousands of trees.
# There is a measure called variable importance which we will see later
# It helps us to interpret the results

# Variable importance tells us how much each predictor influences the
# final predictions


# COMPREHENSION CHECK

# Create a simple dataset where the outcome grows 0.75 units on average 
# for every increase in a predictor, using this code:
library(rpart)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

# Which code correctly uses rpart() to fit a regression tree and saves the
# result to fit?
fit <- rpart(y ~ ., data = dat) 

# Which plot matches the rpart above?
plot(fit)

# Below is most of the code to make a scatter plot of y versus x along with
# the predicted values based on the fit.
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
# Complete the code
  geom_step(aes(x, y_hat), col=2)

# Now run Random Forests instead of a regression tree using randomForest() from 
# the randomForest package, and remake the scatterplot with the prediction 
# line. Part of the code is provided for you below.
library(randomForest)
fit <-  randomForest(y ~ x, data = dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

# Use the plot() function to see if the Random Forest from Q4 has converged 
# or if we need more trees
plot(fit) 


# It seems that the default values for the Random Forest result in an estimate
# that is too flexible (unsmooth). Re-run the Random Forest but this time with
# a node size of 50 and a maximum of 25 nodes. Remake the plot.
library(randomForest)
fit <-  randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")



##################
# CARET PACKAGE #
#################

library(tidyverse)
library(dslabs)
data("mnist_27")

library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall[["Accuracy"]]


# Caret package links
# http://topepo.github.io/caret/available-models.html
# http://topepo.github.io/caret/train-models-by-tag.html



#################################
# TUNING PARAMETERS WITH CARET #
################################

# When an alorithm includes a tuning parameter, train() automatically uses
# cross-validation to decide among a few default values

# To find out which it used you can look here:
# # http://topepo.github.io/caret/available-models.html
# Or study the output of the following code:
getModelInfo("knn")

# You can get a quick lookup, seeing easily that it is k
modelLookup("knn")

# If we run train with default values and use ggplot you can quickly see
# the results of the cross-validation
train_knn <- train(y~., method="knn", data=mnist_27$train)

# Use the argument highlight to highlight the parameter use to optimise algorithm
ggplot(train_knn, highlight=TRUE)

# By default the cross-validation is performed by testing 25 bootstraps on
# 25% of the observations.  Also for knn, the default is to try k = c(5, 7, 9)

# Perhaps there is a better k?  We can use tunegrid parameter in train()
# The grid of values supplied must be a data.frame with column names as specified
# by the parameters that you get in the modelLookup output
data.frame(k=seq(9, 67, 2))
# When we run the code we'll be fitting 30 version of knn to 25 bootstraps
# 25 * 30 = 750 knn models
train_knn <- train(y~., method="knn", data=mnist_27$train,
                   tuneGrid = data.frame(k=seq(9, 71, 2)))
# We can which k maximises accuracy, as highlighted
ggplot(train_knn, highlight=TRUE)
# Or we can use this code
train_knn$bestTune

# We can access the best performing model using this code
train_knn$finalModel

# If you apply the function predict() to the output of the train function
# it will use this best performing model to make predictions
# To gauge accuracy on the test set
confusionMatrix(predict(train_knn, mnist_27$test, type="raw"),
                        mnist_27$test$y)$overall["Accuracy"]


# Sometimes we might want to change the way that we perform cross-validation
# To do this we need to use the trainControl function
# If we wanted to make the code we just showed go a bit faster we could use
# 10-fold validation. This means that we're going to have 10 validation
# sample that use 10% of the observations each
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(y~., method="knn", data = mnist_27$train,
                    tuneGrid = data.frame(k=seq(9, 71, 2)),
                    trControl = control)
ggplot(train_knn, highlight = TRUE)


# We can also make a plot of each model showing the point estimates and SD
train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))

# To finish up this example, we can plot the conditional probability
# against the true probability and see that it matches pretty well
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
plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])
# We see that the boundary is somewhat wiggly, this is because knn, like the
# basic bin smoother does not use a smoother kernel

# To improve this we could try loess
# By reading through the models of caret we see that we can see gamLoess method
install.packages("gam")
# We see that we have 2 parameters to optimise - Degree and Span
modelLookup("gamLoess")

# For this example we will leave Degree at 1, and vary Span
# However we still have to include a column in the table for Degree
# This is a requirement of the carat package, as opposed to a mathematical requirement
grid <- expand.grid(span = seq(0.15, 0.65, len=10), degree = 1)

# using default cross-validation and control parameters we train our model
train_loess <- train(y~., method="gamLoess", tuneGrid=grid, data=mnist_27$train)

# Check the best performing model and see it performs similarly to knn
ggplot(train_loess, highlight = TRUE)

# Check accuracy
confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

# Check the conditional probability versus the true probability
# This is smoother than what we get with knn
p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1


# Note that not all parameters in machine learning algorithms are tuned. We use
# the train() function to only optimize parameters that are tunable.
# In knn, k is tunable
# In regression, the number of predictors are tunable

# In caret package in train(), we only optimise parameters that are tunable
# It won't be the case for example that in regression models, the caret package
# will optimise the regression coefficients that are estimated.
# Instead it will just estimate the least squares.

# This is an important estimat to make when using the caret package, knowing
# which parameters are optimised, and which ones are not


# COMPREHENSION CHECK

# Load the rpart package and then use the caret::train() function with 
# method = "rpart" to fit a classification tree to the tissue_gene_expression 
# dataset. Try out cp values of seq(0, 0.1, 0.01). Plot the accuracies to 
# report the results of the best model. Set the seed to 1991.
# Which value of cp gives the highest accuracy?
install.packages("rpart")
library(rpart)
modelLookup("rpart")

# Set seed
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
dat <- data.frame(tissue_gene_expression)
grid <- expand.grid(cp = seq(0, 0.1, 0.01))
train_rpart <- train(y~.,
                     method = "rpart", 
                     tuneGrid = grid, 
                     data = dat)
# Check plot for best model
ggplot(train_rpart, highlight = TRUE)
train_rpart$bestTune
train_rpart$finalModel

# Elegant solution
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later

library(caret)
library(rpart)          
library(dslabs)
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data("tissue_gene_expression")
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
ggplot(fit)


# Note that there are only 6 placentas in the dataset. By default, 
# rpart requires 20 observations before splitting a node. That means
# that it is difficult to have a node in which placentas are the 
# majority. Rerun the analysis you did in Q1 with caret::train(),
# but this time with method = "rpart" and allow it to split any node
# by using the argument control = rpart.control(minsplit = 0). Look 
# at the confusion matrix again to determine whether the accuracy 
# increases. Again, set the seed to 1991.
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data("tissue_gene_expression")
fit2 <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))),
                  control = rpart.control(minsplit = 0)          
            )
confusionMatrix(fit2)


# Plot the tree from the best fitting model of the analysis you ran in Q2.
# Which gene is at the first split?
fit2$finalModel
# GPA33 >= 8.794


# We can see that with just seven genes, we are able to predict the tissue type. 
# Now let's see if we can predict the tissue type with even fewer genes using a 
# Random Forest. Use the train() function and the rf method to train a Random Forest
# model and save it to an object called fit. Try out values of mtry ranging from 
# seq(50, 200, 25) (you can also explore other values on your own). What mtry 
# value maximizes accuracy? To permit small nodesize to grow as we did with the
# classification trees, use the following argument: nodesize = 1.

# Note: This exercise will take some time to run. If you want to test out your 
# code first, try using smaller values with ntree. Set the seed to 1991 again.
# What value of mtry maximizes accuracy?
set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
data("tissue_gene_expression")
fit3 <- with(tissue_gene_expression,
             train(x, y, method="rf",
                   tuneGrid = data.frame(mtry = seq(50, 200, 25))),
                   nodesize = 1
             )
ggplot(fit3)


# Use the function varImp() on the output of train() and save it to an object
# called imp:
imp <- varImp(fit3)   # Shows the variable importance of the predictors
imp


# The rpart() model we ran above in Q2 produced a tree that used just seven
# predictors. Extracting the predictor names is not straightforward, but can be 
# done. If the output of the call to train was fit_rpart, we can extract the names
# like this:
fit_rpart <- fit2
tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms


# The following code can be used to calculate the rank and importance in the
# Random Forest call for the predictors from the rpart() model:
data.frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)