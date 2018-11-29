########################
###                  ###
### NÄIVE BAYES      ###
###   HOLDOUT        ###
###   METHOD         ###
###                  ###
########################
#Predict on test data
predict_NB_HO_1 <- predict(model_NB_HO_1, newdata = data_test)
confusionMatrix(predict_NB_HO_1$class, data_test$class )

########################
###                  ###
### NÄIVE BAYES      ###
###   10 FOLDS       ###
### CROSS VALIDATION ###
###   METHOD         ###
########################
#Predict on test data
predict_NB_CV <- predict(model_NB_CV, newdata = data_test)
confusionMatrix(predict_NB_CV, data_test$class)

########################
###                  ###
### NÄIVE BAYES      ###
### LEAVE ONE OUT    ###
###   METHOD         ###
###                  ###
########################
#Predict on test data
#print(model_loo_dt)
predict_NB_LOOCV <- predict(model_NB_LOOCV, newdata = data_test)
confusionMatrix(predict_NB_LOOCV, data_test$class)
