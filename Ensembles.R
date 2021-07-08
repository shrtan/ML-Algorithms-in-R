models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
library(tidyverse)
set.seed(1, sample.kind = "Rounding")

#build 10 of the most popular ML models
fits <- lapply(models, function(model) {
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
})


names(fits) <- models

#create a matrix of predictions for the test set using the 10 models
predictions <- sapply(fits, function(fit) {
  predict(fit, mnist_27$test)
})

#rows and columns of predictions
dim(predictions)
length(mnist_27$test$y)
length(models)

#convert matrix to numeric
pred_num <- apply(as.matrix(predictions), 2, as.numeric)

#compute accuracy for each model
accuracy <- apply(pred_num, 2, function(pred) {
  mean(pred == mnist_27$test$y)
})
mean(accuracy)

#alternate method of calculating accuracy
colMeans(pred_num == mnist_27$test$y)

#build an ensemble prediction by majority vote and compute the accuracy of the ensemble 
#Vote 7 if more than 50% of the models are predicting a 7, and 2 otherwise
ensemble_pred <- apply(pred_num, 1, function(temp_row) {
  temp_count <- ifelse(temp_row == 2, 1, 0)
  ifelse(mean(temp_count) > 0.5, 2, 7)
})

#alternate way of predicting
votes <- rowMeans(pred_num == 7)
y_hat <- ifelse(votes > 0.5, 7, 2)
mean(y_hat == mnist_27$test$y)


#mean of the ensemble
ensemble_mean <- mean(ensemble_pred == mnist_27$test$y)

#number of the individual methods do better than the ensemble
sum(accuracy > ensemble_mean)
#individual methods that perform better than the ensemble
accuracy[accuracy > ensemble_mean]


#minimum accuracy estimates obtained from cross validation with the training data for each model 
min_accuracy <- sapply(fits, function(fit) {
  min(fit$results$Accuracy)
})

#mean of min accuracy estimates
mean(min_accuracy)

model_names <- names(min_accuracy)


#Consider the methods with a minimum accuracy estimate of greater than or equal to 0.8 when constructing the ensemble
#Vote 7 if 50% or more of those models are predicting a 7, and 2 otherwise
ensemble_pred <- apply(pred_num[,model_names], 1, function(temp_row) {
  temp_count <- ifelse(temp_row == 2, 1, 0)
  ifelse(mean(temp_count) > 0.5, 2, 7)
})

#accuracy of new ensemble model
mean(ensemble_pred == mnist_27$test$y)

