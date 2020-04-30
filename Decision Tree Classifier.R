
######## Fifth Third - University of Cincinnati Project ###########################
####### Customer Churn Model - Tree Model #########################################
######## April - 2020 ############################################################


# Importing Train data

X_train = read.csv('X_train.txt', sep=",")
y_train = read.csv('y_train.txt', sep = ",")

traindata = cbind(X_train,y_train)

# Importing Test data
testdata = read.csv('X_y_test.txt',sep = ",")

# Importing required libraries 
library(rpart)
library(ROCR)
library(rpart.plot)

# Building initial tree model with no hyperparameter tuning
credit.rpart0 <- rpart(formula = target ~ ., data = traindata, 
                       method = "class")

# Hyperparameter tuning, finding the best cp(complexity parameter) for optimizing the tree 
prp(credit.rpart0, extra = 1)
plotcp(credit.rpart0)

# Rebuilding tree model with tuned parameter and loss matrix (FN=19, FP=1)
credit.rpart1 <- rpart(formula = target ~ ., data = traindata, 
                       method = "class",parms = list(loss=matrix(c(0,19,1,0), nrow = 2)), cp = 0.0033)


# Train data - Building confusion matrix 
pred0<- predict(credit.rpart0, type="class")
table(traindata$target, pred0, dnn = c("True", "Pred"))


# Train data (in-sample) AUC under ROC curve 

pred_insamp <- predict(credit.rpart0, traindata, type="prob")
pred = prediction(pred_insamp[,2], traindata$target)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE, main="Train AUC")

slot(performance(pred, "auc"), "y.values")[[1]]


# Test Data (out-of-sample) AUC under ROC curve 

pred_outsamp <- predict(credit.rpart0, testdata, type="prob")
pred_o = prediction(pred_outsamp[,2], testdata$target)
perf_o = performance(pred_o, "tpr", "fpr")
plot(perf_o, colorize=TRUE, main="Test AUC")

slot(performance(pred_o, "auc"), "y.values")[[1]]

# Test data - Building confusion matrix 

pred1 = predict(credit.rpart0, testdata, type="class")
table(testdata$target, pred1, dnn = c("True", "Pred"))

#  NOTE: 
#  Recall is defined as: TruePositives / Total Actual Positives 

