#install.packages("caret")
#install.packages("klaR")
#install.packages('e1071', dependencies=TRUE)

library(caret)
library(klaR)
library(e1071)

#set seed
set.seed(3211)
# define an 75%/25% train/test split of the dataset
split=0.75
trainIndex <- createDataPartition(banknote_norm$class, p=split, list=FALSE)
data_train <- banknote_norm[ trainIndex,]
data_test <- banknote_norm[-trainIndex,]

#get number train and test set
dim(data_train) 
dim(data_test)

########################
###                  ###
### DECISION TREE    ###
###   (C 4.5)        ###
###   HOLDOUT        ###
###   METHOD         ###
###                  ###
########################
library(RWeka)
model_J48_HO_1 <- J48(class~., data=data_train)
print(model_J48_HO_1)

predict_J48_HO_1 <- predict(model_J48_HO_1, newdata = data_test)
confusionMatrix(predict_J48_HO_1, data_test$class )

#Alternative method
train_control_HO <- trainControl(method="none")
model_J48_HO_2 <- train(class~., data=data_train, method="J48", trControl=train_control_HO)
print(model_J48_HO_2)
predict_J48_HO_2 <- predict(model_J48_HO_2, newdata = data_test)
confusionMatrix(predict_J48_HO_2, data_test$class )

########################
###                  ###
### DECISION TREE    ###
###   (C 4.5)        ###
###   10 FOLDS       ###
### CROSS VALIDATION ###
###   METHOD         ###
########################
# define training control
train_control_CV <- trainControl(method="cv", number=10)

# train the model
model_J48_CV <- train(class~., data=data_train, trControl=train_control_CV, method="J48")
# summarize results
print(model_J48_CV)

########################
###                  ###
### DECISION TREE    ###
###   (C 4.5)        ###
### LEAVE ONE OUT    ###
###   METHOD         ###
###                  ###
########################
train_control_LOOCV <- trainControl(method="LOOCV")
# train the model
model_J48_LOOCV <- train(class~., data=data_train, trControl=train_control_LOOCV, method="J48")
# summarize results
print(model_J48_LOOCV)


########################
###                  ###
### DECISION TREE    ###
### RANDOM FOREST    ###
###   HOLDOUT        ###
###   METHOD         ###
###                  ###
########################
#install.packages("randomForest")
library(randomForest)

model_RF_HO_1 <- randomForest(class~., data=data_train)
print(model_RF_HO_1)
predict_RF_HO_1 <- predict(model_RF_HO_1, newdata = data_test)
confusionMatrix(predict_RF_HO_1, data_test$class )

#Alternative method
train_control_HO <- trainControl(method="none")
model_RF_HO_2 <- train(class~., data=data_train, method="rf", trControl=train_control_HO)
print(model_RF_HO_2)
predict_RF_HO_2 <- predict(model_RF_HO_2, newdata = data_test)
confusionMatrix(predict_RF_HO_2, data_test$class )

########################
###                  ###
### DECISION TREE    ###
### RANDOM FOREST    ###
###   10 FOLDS       ###
### CROSS VALIDATION ###
###   METHOD         ###
########################
# define training control
train_control_CV <- trainControl(method="cv", number=10)
# train the model
model_RF_CV <- train(class ~ ., data=data_train, trControl=train_control_CV, method="rf")
# summarize results
print(model_RF_CV)


########################
###                  ###
### DECISION TREE    ###
### RANDOM FOREST    ###
### LEAVE ONE OUT    ###
###   METHOD         ###
###                  ###
########################
train_control_LOOCV <- trainControl(method="LOOCV")
# train the model
model_RF_LOOCV <- train(class~., data=data_train, trControl=train_control_LOOCV, method="rf")
# summarize results
print(model_RF_LOOCV)

########################
###                  ###
### NÄIVE BAYES      ###
###   HOLDOUT        ###
###   METHOD         ###
###                  ###
########################
model_NB_HO_1 <- NaiveBayes(class~., data=data_train)
print(model_NB_HO_1)
predict_NB_HO_1 <- predict(model_NB_HO_1, newdata = data_test)
confusionMatrix(predict_NB_HO_1$class, data_test$class )

########################
###                  ###
### NÄIVE BAYES      ###
###   10 FOLDS       ###
### CROSS VALIDATION ###
###   METHOD         ###
########################
# define training control
train_control_CV <- trainControl(method="cv", number=10)

# train the model
model_NB_CV <- train(class~., data=data_train, trControl=train_control_CV, method="nb")
# summarize results
print(model_NB_CV)

########################
###                  ###
### NÄIVE BAYES      ###
### LEAVE ONE OUT    ###
###   METHOD         ###
###                  ###
########################
train_control_LOOCV <- trainControl(method="LOOCV")

# train the model
model_NB_LOOCV <- train(class~., data=data_train, trControl=train_control_LOOCV, method="nb")
# summarize results
print(model_NB_LOOCV)

########################
###                  ###
###   KNN            ###
###   HOLDOUT        ###
###   METHOD         ###
###                  ###
########################
library(class)

#Let's plot classiﬁcation accuracy as a function of k (k = 1,...,50)
#To determine the 'best' number of k
accuracy <- rep(0, 50)
k <- 1:50
for(x in k){
  prediction <- knn(train = data_train[,1:4], test = data_test[,1:4], cl = data_train$class, k = x)
  accuracy[x] <- mean(prediction == data_train$class)
  
}
plot(k, accuracy, type = 'b')


model_KNN_HO_1 <- knn(train = data_train[,1:4], test = data_test[,1:4], cl = data_train$class, k = 5)
confusionMatrix(model_KNN_HO_1, data_test$class )

########################
###                  ###
###      KNN         ###
###   10 FOLDS       ###
### CROSS VALIDATION ###
###   METHOD         ###
########################
# define training control
train_control_CV <- trainControl(method="cv", number=10)

# train the model
model_KNN_CV <- train(data_train[,1:4], data_train$class, method = "knn", trControl = train_control_CV)
# summarize results
print(model_KNN_CV)

########################
###                  ###
### KNN              ###
### LEAVE ONE OUT    ###
### METHOD           ###
###                  ###
########################
train_control_LOOCV <- trainControl(method="LOOCV")
# train the model

model_KNN_LOOCV <- train(data_train[,1:4], data_train$class, method = "knn", trControl = train_control_LOOCV)
# summarize results
print(model_KNN_LOOCV)

