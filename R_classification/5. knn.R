########################
###                  ###
###   KNN            ###
###   HOLDOUT        ###
###   METHOD         ###
###                  ###
########################
#Predict on test data
confusionMatrix(model_KNN_HO_1, data_test$class )

########################
###                  ###
###      KNN         ###
###   10 FOLDS       ###
### CROSS VALIDATION ###
###   METHOD         ###
########################
#Predict on test data
predict_KNN_CV <- predict(model_KNN_CV, newdata = data_test)
confusionMatrix(predict_KNN_CV, data_test$class)

########################
###                  ###
### KNN              ###
### LEAVE ONE OUT    ###
### METHOD           ###
###                  ###
########################
#Predict on test data
predict_KNN_LOOCV <- predict(model_KNN_LOOCV, newdata = data_test)
confusionMatrix(predict_KNN_LOOCV, data_test$class)
