install.packages("gbm")
library(gbm)
library(caret)

#dataset
boston = MASS::Boston
str(boston)

#training and test datasets
indexes = createDataPartition(boston$medv, p = .90, list = F)
train = boston[indexes, ]
test = boston[-indexes, ]

model_gbm = gbm(train$medv ~.,
                data = train,
                # gaussian loss function
                distribution = "gaussian",
                # Number of cross-validation folds to perform 
                # If cv.folds>1 then gbm, in addition to the usual fit, will perform a 
                # cross-validation, calculate an estimate of generalization error returned in cv.error
                cv.folds = 10, 
                # learning rate or step-size reduction; 0.001 to 0.1 usually work, 
                # but a smaller learning rate typically requires more trees
                shrinkage = .01,
                # Integer specifying the minimum number of observations in the terminal nodes of the trees
                n.minobsinnode = 10,
                # Integer specifying the total number of trees to fit. This is 
                # equivalent to the number of iterations and the number of basis 
                # functions in the additive expansion.
                n.trees = 500)


#predict test data and visialize the result in a plot
test_x = test[, -14] 
test_y = test[, 14] 

pred_y = predict.gbm(model_gbm, test_x)
x_ax = 1:length(pred_y)
plot(x_ax, test_y, col="blue", pch=20, cex=.9)
lines(x_ax, pred_y, col="red", pch=20, cex=.9) 



## GBM WITH CARET TRAIN METHOD

tc = trainControl(method = "cv", number=10)
model = train(medv ~., data=train, method="gbm", trControl=tc)

#predict test data and visialize the result in a plot
pred_y = predict(model, test_x)
x_ax = 1:length(pred_y)
plot(x_ax, test_y, col="blue", pch=20, cex=.9)
lines(x_ax, pred_y, col="red", pch=20, cex=.9)

mean(pred_y - test_y)
