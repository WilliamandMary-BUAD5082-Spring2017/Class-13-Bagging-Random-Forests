
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
install.packages("mlbench")
install.packages("rpart")
install.packages("Kernlab")
library(caret)
library(mlbench)
library(MASS)
library(rpart)
library(kernlab)
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
