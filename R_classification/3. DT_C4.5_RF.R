########################
###                  ###
### DECISION TREE    ###
###   (C 4.5)        ###
###   HOLDOUT        ###
###   METHOD         ###
###                  ###
########################

#Predict on test data
predict_J48_HO_1 <- predict(model_J48_HO_1, newdata = data_test)
confusionMatrix(predict_J48_HO_1, data_test$class)

########################
###                  ###
### DECISION TREE    ###
###   (C 4.5)        ###
###   10 FOLDS       ###
### CROSS VALIDATION ###
###   METHOD         ###
########################
#Predict on test data
predict_J48_CV <- predict(model_J48_CV, newdata = data_test)
confusionMatrix(predict_J48_CV, data_test$class)

########################
###                  ###
### DECISION TREE    ###
###   (C 4.5)        ###
### LEAVE ONE OUT    ###
###   METHOD         ###
###                  ###
########################
#Predict on test data
predict_J48_LOOCV <- predict(model_J48_LOOCV, newdata = data_test)
confusionMatrix(predict_J48_LOOCV, data_test$class)


########################
###                  ###
### DECISION TREE    ###
### RANDOM FOREST    ###
###   HOLDOUT        ###
###   METHOD         ###
###                  ###
########################
#Predict on test data
predict_RF_HO_1 <- predict(model_RF_HO_1, newdata = data_test)
confusionMatrix(predict_RF_HO_1, data_test$class)

########################
###                  ###
### DECISION TREE    ###
### RANDOM FOREST    ###
###   10 FOLDS       ###
### CROSS VALIDATION ###
###   METHOD         ###
########################
#Predict on test data
predict_RF_CV <- predict(model_RF_CV, newdata = data_test)
confusionMatrix(predict_RF_CV, data_test$class)

########################
###                  ###
### DECISION TREE    ###
### RANDOM FOREST    ###
### LEAVE ONE OUT    ###
###   METHOD         ###
###                  ###
########################
#Predict on test data
predict_RF_LOOCV <- predict(model_RF_LOOCV, newdata = data_test)
confusionMatrix(predict_RF_LOOCV, data_test$class)

