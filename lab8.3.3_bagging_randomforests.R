############################################
## Team 9 Labs - Bagging & Random Forests ##
############################################
rm(list=ls())

#install.packages("randomForest")
#install.packages("MASS")

library (randomForest)
library(MASS)

set.seed (1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
boston.test=Boston[-train,"medv"]

# The mtry=13 argument indicates that all 13 predictors should be considered
# for each split of the tree-in other words bagging should be done.
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, importance =TRUE)
# Inspect formula object
bag.boston

# Evaluate bagged model on test set
yhat.bag = predict(bag.boston, newdata=Boston[-train ,])
# Plot results
plot(yhat.bag, boston.test)
abline (0,1)

# Test set MSE associated with the bagged regression tree
mean((yhat.bag-boston.test)^2)

# change the number of trees grown by randomForest() using the ntree argument, 25 in this example
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25)

# Evaluate bagged model of 25 trees on test set
yhat.bag = predict(bag.boston, newdata=Boston[-train ,])
# Plot results
plot(yhat.bag, boston.test)
abline (0,1)

# Test set MSE associated with the 25-tree bagged regression tree
mean((yhat.bag-boston.test)^2)

# Grow a random forest by using a smaller value of the mtry argument (6 in this example). 
# By default, randomForest() uses p/3 variables when building a random forest of regression trees, 
# and square root p variables when building a random forest of classification trees.
set.seed (1)

rf.boston = randomForest(medv~., data=Boston, subset=train , mtry=6, importance=TRUE)
# Inspect formula object
rf.boston

# Evaluate random forest model on test set
yhat.rf = predict (rf.boston, newdata=Boston[-train ,])
# MSE
mean((yhat.rf-boston.test)^2)

# View the importance of each variable
# The %IncMSE is based upon the mean decrease of accuracy in predictions on the out of bag samples
# when a given variable is excluded from the model. 
# The IncNodePurity is a measure of the total decrease in node impurity 
# that results from splits over that variable, averaged over all trees.
importance(rf.boston)

# Plots of importance 
varImpPlot(rf.boston)



