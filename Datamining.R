#install.packages('keras')
#install.packages('tensorflow')
#install.packages('neuralnet')

library(ROSE)

setwd("C:\\Users\\Vinod\\Desktop\\Multivariate Analysis")
data <- read.csv("creditcardcsvpresent - Segregated Merchants.csv")

#pos <- c(which(data$Is.a.Fraud.merchant.==1))
#data_pos=data[pos,]
#data_pos$isFradulent
#data_neg=data[-pos,]
#data_neg <- data_neg[1:448,]
#data <- rbind(data_pos,data_neg)
str(data)
#cov(data)
#row.names(data) <- data[1,]
data <- subset(data,select = -c(1))
data[,1:9] <- scale(data[,1:9])
head(data)
data$Is.a.Fraud.merchant. <- as.factor(data$Is.a.Fraud.merchant.)
#x <- cor(data)
#write.csv(x,"correlation.csv")
#Data Split
set.seed(7)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.75,0.25))
training <- data[ind==1,]
testing <- data[ind==2,]
data_balance <- ovun.sample(Is.a.Fraud.merchant.~., data = training, method = "both", p=.5, N = 2261)$data
#training <- data

#k fold
library(tidyverse)
library(caret)
set.seed(123) 

#KNN
train.control <- trainControl(method = "cv", number = 10)
model_knn <- train(Is.a.Fraud.merchant.~., data = training, method = "knn",
                   trControl = train.control)
pred_knn<-predict(object = model_knn,testing[,1:9])
confusionMatrix(testing$Is.a.Fraud.merchant.,pred_knn)


#Neural Network
train.control <- trainControl(method = "cv", number = 10)
model_nn <- train(Is.a.Fraud.merchant.~., data = training, method = "pcaNNet",
                  trControl = train.control)
pred_nn<-predict(object = model_nn,testing[,1:9])
confusionMatrix(testing$Is.a.Fraud.merchant.,pred_nn)

#Logistic Regression
train.control <- trainControl(method = "cv", number = 10)
model_lr <- train(Is.a.Fraud.merchant.~., data = training, method = "regLogistic",
                  trControl = train.control, tuneLength = 3)
pred_lr<-predict(object = model_lr,testing[,1:9])
confusionMatrix(testing$Is.a.Fraud.merchant.,pred_lr)

#Random Forset
train.control <- trainControl(method = "cv", number = 10)
model_rf <- train(Is.a.Fraud.merchant.~., data = training, method = "cforest",
                  trControl = train.control)
pred_rf <-predict(object = model_rf,testing[,1:9])
confusionMatrix(testing$Is.a.Fraud.merchant.,pred_rf)

#SVM
train.control <- trainControl(method = "cv", number = 10)
model_svm <- train(Is.a.Fraud.merchant.~., data = training, method = "svmLinear3",
                  trControl = train.control)
pred_svm <-predict(object = model_svm,testing[,1:9])
confusionMatrix(testing$Is.a.Fraud.merchant.,pred_svm)


print(model_knn)
print(model_nn)
print(model_lr)
print(model_rf)
print(model_svm)

pred_knn
pred_lr
pred_nn
pred_rf
pred_svm

model_knn$results
model_nn$results
model_lr$results
model_rf$pred
model_svm$pred

plot(model_knn)
plot(model_nn)
plot(model_lr)
plot(model_rf)
plot(model_svm)

#Ensemble
testing$pred_rf_prob<-predict(object = model_rf,testing[,1:9],type='prob')
testing$pred_knn_prob<-predict(object = model_knn,testing[,1:9],type='prob')
testing$pred_lr_prob<-predict(object = model_lr,testing[,1:9],type='prob')
testing$pred_nn_prob<-predict(object = model_nn,testing[,1:9],type='prob')
testing$pred_svm_prob<-predict(object = model_svm,testing[,1:9],type='prob')

#Majority
testing$pred_majority<-as.factor(ifelse(testing$pred_rf_prob>=0.5 && testing$pred_knn_prob>=0.5 && testing$pred_lr_prob>=0.5,'Y','N'))
                                       