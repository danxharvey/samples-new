#####################################
# TITANIC QUESTIONS FOR ASSESSMENT #
####################################

install.packages("titanic")
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)


# Split titanic_clean into test and training sets - after running the setup
# code, it should have 891 rows and 9 variables.
nrow(titanic_clean)
ncol(titanic_clean)

# Set the seed to 42, then use the caret package to create a 20% data 
# partition based on the Survived column. Assign the 20% partition to 
# test_set and the remaining 80% partition to train_set.
set.seed(42, sample.kind = "Rounding") # if using R 3.6 or later
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p=0.2, list=FALSE)
train_set <- titanic_clean[-test_index, ]
test_set <- titanic_clean[test_index, ]

# How many observations are in the training set?
nrow(train_set)
# How many observations are in the test set?
nrow(test_set)
# What proportion of individuals in the training set survived
sum(train_set$Survived == 1) / nrow(train_set)
mean(train_set$Survived == 1)


# BASELINE PREDICTION BY GUESSING THE OUTCOME
# The simplest prediction method is randomly guessing the outcome without 
# using additional predictors. These methods will help us determine whether 
# our machine learning algorithm performs better than chance. How accurate 
# are two methods of guessing Titanic passenger survival?

# Set the seed to 3. For each individual in the test set, randomly guess
# whether that person survived or not by sampling from the vector c(0,1) 
# (Note: use the default argument setting of prob from the sample function).
set.seed(3, sample.kind = "Rounding") # if using R 3.6 or later
y_hat <- sample(c(0, 1), length(test_set), replace=TRUE)
mean(y_hat == test_set$Survived)


# Use the training set to determine whether members of a given sex were more
# likely to survive or die. Apply this insight to generate survival 
# predictions on the test set.

# What proportion of training set females survived?
mean(train_set$Survived[train_set$Sex=='female']==1)
# What propertion of training set males survived?
mean(train_set$Survived[train_set$Sex=='male']==1)
# Alternate solution
train_set %>% group_by(Sex) %>%
  summarize(Survived = mean(Survived == 1)) %>%
  filter(Sex == "male") %>%
  pull(Survived)


# Predict survival using sex on the test set: if the survival rate
# for a sex is over 0.5, predict survival for all individuals of that 
# sex, and predict death if the survival rate for a sex is under 0.5.
sex_model <- ifelse(test_set$Sex == "female", 1, 0)
mean(test_set$Survived == sex_model)


# In the training set, which class(es) (Pclass) were passengers more
# likely to survive than die?
train_set %>% group_by(Pclass) %>% summarise(Survived = mean(Survived == 1))


# Predict survival using passenger class on the test set: 
# predict survival if the survival rate for a class is over 0.5, 
# otherwise predict death.
# What is the accuracy of this class-based prediction method on the test set?
class_model <- ifelse(test_set$Pclass == 1, 1, 0)
mean(test_set$Survived == class_model)


# Use the training set to group passengers by both sex and passenger class.
# Which sex and class combinations were more likely to survive than die?
train_set %>% group_by(Sex, Pclass) %>% 
      summarise(Survived = mean(Survived == 1)) %>%
      filter(Survived > 0.5)


# Predict survival using both sex and passenger class on the test set.
# Predict survival if the survival rate for a sex/class combination is over 0.5, otherwise predict death.
# What is the accuracy of this sex and class-based prediction method
# on the test set?
combined_model <- ifelse(test_set$Pclass %in% c(1, 2) & test_set$Sex == "female", 1, 0)
mean(test_set$Survived == combined_model)


# Use the confusionMatrix() function to create confusion matrices for the 
# sex model, class model, and combined sex and class model. You will need
# to convert predictions and survival status to factors to use this function.
confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(class_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(combined_model), reference = factor(test_set$Survived))
# What is the "positive" class used to calculate confusion matrix metrics?
# 0
# Which model has the highest sensitivity?
# Combined model
# Which model has the highest specificity?
# Sex model
# Which model has the highest balanced accuracy?
# Sex model

# What is the maximum value of balanced accuracy?
# 0.806


# Use the F_meas() function to calculate  F1  scores for the sex model, 
# class model, and combined sex and class model. You will need to convert
# predictions to factors to use this function.
F_meas(data = factor(sex_model), reference = factor(test_set$Survived))
F_meas(data = factor(class_model), reference = factor(test_set$Survived))
F_meas(data = factor(combined_model), reference = factor(test_set$Survived))


###########
# PART 2 #
##########

# Set the seed to 1. Train a model using linear discriminant analysis (LDA) 
# with the caret lda method using fare as the only predictor.
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
predict_lda <- predict(train_lda, test_set)
confusionMatrix(predict_lda, test_set$Survived)
# Repeat for qda
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
predict_qda <- predict(train_qda, test_set)
confusionMatrix(predict_qda, test_set$Survived)
# Accuracy - mean(predict_qda == test_set$Survived)


# Set the seed to 1. Train a logistic regression model with the caret 
# glm method using age as the only predictor.
# What is the accuracy on the test set using age as the only predictor?
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_glm <- train(Survived ~ Age, method = "glm", data = train_set)
predict_glm <- predict(train_glm, test_set)
confusionMatrix(predict_glm, test_set$Survived)

# Repeat using four predictors: sex, class, fare, and age.
# What is the accuracy on the test set using these four predictors?
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_glm4 <- train(Survived ~ Sex+Pclass+Fare+Age, method = "glm", data = train_set)
predict_glm4 <- predict(train_glm4, test_set)
confusionMatrix(predict_glm4, test_set$Survived)

# Repeat using all
# What is the accuracy on the test set using all predictors?
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
predict_glm_all <- predict(train_glm_all, test_set)
confusionMatrix(predict_glm_all, test_set$Survived)


# Set the seed to 6. Train a kNN model on the training set using the 
# caret train function. Try tuning with k = seq(3, 51, 2).
# What is the optimal value of the number of neighbors k?
set.seed(6, sample.kind = "Rounding") # if using R 3.6 or later
train_knn <- train(Survived ~ ., method = "knn", 
                   tuneGrid = data.frame(k = seq(3, 51, 2)),
                   data = train_set)
train_knn$bestTune

# Plot the kNN model to investigate the relationship between the number of 
# neighbors and accuracy on the training set.
ggplot(train_knn, highlight=TRUE)
confusionMatrix(predict(train_knn, test_set), test_set$Survived)$overall["Accuracy"]
# knn_preds <- predict(train_knn, test_set)
# mean(knn_preds == test_set$Survived)    # This is also accuracy


# Set the seed to 8 and train a new kNN model. Instead of the default training
# control, use 10-fold cross-validation where each partition consists of
# 10% of the total. Try tuning with k = seq(3, 51, 2).
set.seed(8, sample.kind = "Rounding") # if using R 3.6 or later
control <- trainControl(method = "cv", number = 10, p = .9)  # 1 - 10% above
train_knn_cv <- train(Survived ~ ., method="knn", 
                  data = train_set,
                  tuneGrid = data.frame(k=seq(3, 51, 2)),
                  trControl = control)

# What is the optimal value of k using cross-validation?
train_knn_cv$bestTune
# What is the accuracy on the test set using the cross-validated kNN model?
confusionMatrix(predict(train_knn_cv, test_set), test_set$Survived)$overall["Accuracy"]


# Set the seed to 10. Use caret to train a decision tree with the rpart
# method. Tune the complexity parameter with cp = seq(0, 0.05, 0.002).
install.packages("rpart")
library(rpart)
set.seed(10, sample.kind = "Rounding") # if using R 3.6 or later
grid <- expand.grid(cp = seq(0, 0.05, 0.002))
train_rpart <- train(Survived ~ .,
                     method = "rpart", 
                     tuneGrid = grid, 
                     data = train_set)
# What is the optimal value of the complexity parameter (cp)?
train_rpart$bestTune
# What is the accuracy of the decision tree model on the test set?
confusionMatrix(predict(train_rpart, test_set), test_set$Survived)$overall["Accuracy"]


# Inspect the final model and plot the decision tree.
# Which variables are used in the decision tree?
# make plot of decision tree
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel)


# Set the seed to 14. Use the caret train() function with the rf method to
# train a random forest. Test values of mtry = seq(1:7). Set ntree to 100.
set.seed(14, sample.kind = "Rounding") # if using R 3.6 or later
train_rf <- train(Survived~., method="rf",
                  tuneGrid = data.frame(mtry = seq(1:7)),
                  data = train_set,
                  ntree = 100)

#What mtry value maximizes accuracy?
train_rf$bestTune

# What is the accuracy of the random forest model on the test set?
confusionMatrix(predict(train_rf, test_set), test_set$Survived)$overall["Accuracy"]

# Use varImp() on the random forest model object to determine the importance
# of various predictors to the random forest model.
# What is the most important variable?
varImp(train_rf)

