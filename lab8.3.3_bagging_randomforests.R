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


# Random Forest Classification
rm(list=ls())

# Importing the dataset
diabetes= read.csv('diabetes.csv')
names(diabetes) = c("npregant","plasma","bp","triceps","insulin","bmi","pedigree","age","class")
diabetes$class = factor(diabetes$class, levels = c(0,1), labels = c("normal", "diabetic"))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(5028)
split = sample.split(diabetes$class, SplitRatio = 0.75)
training_set = subset(diabetes, split == TRUE)
test_set = subset(diabetes, split == FALSE)

# install.packages('randomForest')
# Fitting Random Forest Classification to the Training set
library(randomForest)
set.seed(5082)
classifier = randomForest(x = training_set[-9],
                          y = training_set$class,
                          ntree = 500)
#checking the importantce of each variable
importance(classifier)
varImpPlot(classifier)

#checking the confusion matrix of the model
classifier$confusion
plot(classifier, log ='y')
# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-9])

# Making the Confusion Matrix using test data
cm = table(test_set[,9], y_pred)
err.rate = (cm[1,2] + cm[2,1])/sum(cm)
print(err.rate)



#multiple classification comaprasion using caret package


install.packages("caret")
install.package("mlbench")
library(caret)
library(mlbench)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# CART
set.seed(7)
fit.cart <- train(class~., data=diabetes, method="rpart", trControl=control)
# LDA
set.seed(7)
fit.lda <- train(class~., data=diabetes, method="lda", trControl=control)
# SVM
set.seed(7)
fit.svm <- train(class~., data=diabetes, method="svmRadial", trControl=control)
# kNN
set.seed(7)
fit.knn <- train(class~., data=diabetes, method="knn", trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(class~., data=diabetes, method="rf", trControl=control)
# collect resamples
results <- resamples(list(CART=fit.cart, LDA=fit.lda, SVM=fit.svm, KNN=fit.knn, RF=fit.rf))


# summarize the distributions
summary(results)
# boxplots of results
bwplot(results)



#(Rattle) The R Analytic Tool To Learn Easily (Rattle) provides 
#a Gnome (RGtk2) based interface to R functionality for data mining.
library(rattle)
#Please choose Yes to install GDK+
rattle()

