#############################
# BREAST CANCER ASSESSMENT #
############################

install.packages("dslabs")
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(dplyr)
data(brca)

# How many samples are in the dataset?
dim(brca$x)[1]
# How many predictors are in the matrix?
dim(brca$x)[2]
# What proportion of the samples are malignant?
mean(brca$y == "M")
# Which column number has the highest mean?
which.max(colMeans(brca$x))
# Which column number has the lowest standard deviation?
which.min(colSds(brca$x))



# Use sweep() two times to scale each column: subtract the column means of brca$x,
# then divide by the column standard deviations of brca$x
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")
# After scaling, what is the standard deviation of the first column?
sd(x_scaled[ ,1])
# After scaling, what is the median value of the first column?
median(x_scaled[ ,1])


# Calculate the distance between all samples using the scaled matrix.
d_samples <- dist(x_scaled)
# What is the average distance between the first sample, which is benign,
# and other benign samples?
dist_BtoB <- as.matrix(d_samples)[1, brca$y == "B"]
mean(dist_BtoB[2:length(dist_BtoB)])
# What is the average distance between the first sample and malignant samples?
dist_BtoM <- as.matrix(d_samples)[1, brca$y == "M"]
mean(dist_BtoM)


# Make a heatmap of the relationship between features using the scaled matrix
d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)



# Perform hierarchical clustering on the 30 features. Cut the tree into 5 groups.
# All but one of the answer options are in the same group.
# Which is in a different group?
h <- hclust(d_features)
groups <- cutree(h, k = 5)
split(names(groups), groups)



# Perform a principal component analysis of the scaled matrix.
# What proportion of variance is explained by the first principal component?
pca <- prcomp(x_scaled)
summary(pca)    # see PC1 Cumulative Proportion
# The proportion of variance explained can be determined using the
# following code:
pca <- prcomp(x_scaled)
summary(pca)    # see PC1 Cumulative Proportion
# Plot
plot(summary(pca)$importance[3,])


# Plot the first two principal components with color representing 
# tumor type (benign/malignant).
# Which of the following is true?
data.frame(pc_1 = brca$x[,1], pc_2 = brca$x[,2], 
           tissue = brca$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()


# Make a boxplot of the first 10 PCs grouped by tumor type.
# Which PCs are significantly different enough by tumor type that there 
# is no overlap in the interquartile ranges (IQRs) for benign and
# malignant samples?
data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()



# Set the seed to 1, then create a data partition splitting brca$y and the
# scaled version of the brca$x matrix into a 20% test set and 80% train 
# using the following code:
# set.seed(1) if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]
# Save models as you progress through the questions as you'll be creating
# an ensemble towards the end


# Check that the training and test sets have similar proportions of
# benign and malignant tumors.

# What proportion of the training set is benign?
mean(train_y == "B")
# What proportion of the test set is benign?
mean(test_y == "B")



# The predict_kmeans() function defined here takes two arguments - a matrix 
# of observations x and a k-means object k - and assigns each row of x to
# a cluster from k.
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

# Set the seed to 3. Perform k-means clustering on the training set with 
# 2 centers and assign the output to k. Then use the predict_kmeans() function 
# to make predictions on the test set.
# set.seed(3) if using R 3.5 or earlier
set.seed(3, sample.kind = "Rounding")    # if using R 3.6 or later
k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmeans_preds == test_y)


# What proportion of benign tumours are identified correctly
sensitivity(factor(kmeans_preds), test_y, positive = "B")
# The proportion of malignant tumors that are correctly identified, which 
# is the sensitivity for malignant tumors:
sensitivity(factor(kmeans_preds), test_y, positive = "M")

# Fit a logistic regression model on the training set with caret::train() using 
# all predictors. Ignore warnings about the algorithm not converging. 
# Make predictions on the test set.
library(caret)
dat_train <- data.frame(y=train_y, x=train_x)
dat_test <- data.frame(y=test_y, x=test_x)
# What is the accuracy of the logistic regression model on the test set?
train_glm <- train(y~., method="glm", data=dat_train)
glm_preds <- predict(train_glm, dat_test)
confusionMatrix(y_hat, dat_test$y)$overall["Accuracy"]
# What is the accuracy of the lda model on the test set?
train_lda <- train(y~., method="lda", data=dat_train)
lda_preds <- predict(train_lda, dat_test)
confusionMatrix(y_hat, dat_test$y)$overall["Accuracy"]
# What is the accuracy of the qda model on the test set?
train_qda <- train(y~., method="qda", data=dat_train)
qda_preds <- predict(train_qda, dat_test)
confusionMatrix(y_hat, dat_test$y)$overall["Accuracy"]
# What is the accuracy of the gamLoess model on the test set?
train_gam <- train(y~., method="gamLoess", data=dat_train)
gam_preds <- predict(train_gam, dat_test)
confusionMatrix(y_hat, dat_test$y)$overall["Accuracy"]


# Set the seed to 7, then train a k-nearest neighbors model on the training 
# set using the caret package. Try odd values of k from 3 to 21. Use the final
# model to generate predictions on the test set.
set.seed(7, sample.kind = "Rounding")    # if using R 3.6 or later
tuning <- data.frame(k = seq(3, 21, 2))
train_knn <- train(train_x, train_y,
                   method = "knn", 
                   tuneGrid = tuning)
train_knn$bestTune
# What is the accuracy using the optimised k
knn_preds <- predict(train_knn, test_x)
mean(knn_preds == test_y)



# Set the seed to 9, then train a random forest model on the training set using
# the caret package. Test mtry values of c(3, 5, 7, 9). Use the argument
# importance = TRUE so that feature importance can be extracted. Generate 
# predictions on the test set.
# Note: please use c(3, 5, 7, 9) instead of seq(3, 9, 2) in tuneGrid.
# What value of mtry gives the highest accuracy?
set.seed(9, sample.kind = "Rounding") # if using R 3.6 or later
train_rf <- with(train_dat,
             train(x, y, method="rf",
                   tuneGrid = data.frame(mtry = c(3,5,7,9))),
             importance = TRUE
)
# What is the accuracy of the random forest model on the test set
rf_preds <- predict(train_rf, test_x)
mean(rf_preds == test_y)
# What is the most important variable in the random forest model
varImp(train_rf)


# Create an ensemble using the predictions from the 7 models created 
# in the previous exercises: k-means, logistic regression, LDA, QDA, 
# loess, k-nearest neighbors, and random forest. Use the ensemble to
# generate a majority prediction of the tumor type (if most models suggest
# the tumor is malignant, predict malignant).
# What is the accuracy of the ensemble prediction?
ensemble <- cbind(glm = glm_preds == "B", lda = lda_preds == "B", qda = qda_preds == "B", loess = gam_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B", kmeans = kmeans_preds == "B")
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)

# Make a table of the accuracies of the 7 models and the accuracy 
# of the ensemble model.
# Which of these models has the highest accuracy?
models <- c("K means", "Logistic regression", "LDA", "QDA", "Loess", "K nearest neighbors", "Random forest", "Ensemble")
accuracy <- c(mean(kmeans_preds == test_y),
              mean(glm_preds == test_y),
              mean(lda_preds == test_y),
              mean(qda_preds == test_y),
              mean(loess_preds == test_y),
              mean(knn_preds == test_y),
              mean(rf_preds == test_y),
              mean(ensemble_preds == test_y))
data.frame(Model = models, Accuracy = accuracy)