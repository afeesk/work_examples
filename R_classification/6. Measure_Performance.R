#install.packages("ROCR")
library(ROCR)

########################
###                  ###
### DECISION TREE    ###
###   (C 4.5)        ###
###   HOLDOUT        ###
###   METHOD         ###
###                  ###
########################
confusionMatrix(predict_J48_HO_1, data_test$class)

#Precision/recall
pred_J48_HO <- prediction(as.numeric(predict_J48_HO_1), as.numeric(data_test$class))
perf_J48_HO_preRecall <- performance(pred_J48_HO, measure="prec", x.measure="rec")
perf_J48_HO_preRecall@x.values[[1]] # Recall values
perf_J48_HO_preRecall@y.values[[1]] # Precision values
paste0(round(100*perf_J48_HO_preRecall@x.values[[1]][2], 2), "%") # Recall values
paste0(round(100*perf_J48_HO_preRecall@y.values[[1]][2], 2), "%") # Precision values

#Accuracy
perf_J48_HO_acc <- performance(pred_J48_HO, "acc")
perf_J48_HO_acc@y.values[[1]] #Accuracy
paste0(round(100*perf_J48_HO_acc@y.values[[1]][2], 2), "%") #Accuracy

#ROC curve
perf_J48_HO_ROC <- performance(pred_J48_HO,  measure = "tpr", x.measure = "fpr")
plot(perf_J48_HO_ROC, main = "ROC curve for C4.5 Holdout method", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

#AUC
perf_J48_HO_AUC <- performance(pred_J48_HO, measure = "auc")
perf_J48_HO_AUC@y.values[[1]] #AUC
paste0(round(100*perf_J48_HO_AUC@y.values[[1]], 2), "%") #AUC

########################
###                  ###
### DECISION TREE    ###
###   (C 4.5)        ###
###   10 FOLDS       ###
### CROSS VALIDATION ###
###   METHOD         ###
########################
confusionMatrix(predict_J48_CV, data_test$class)
#Precision/recall
pred_J48_CV <- prediction(as.numeric(predict_J48_CV), as.numeric(data_test$class))
perf_J48_CV_preRecall <- performance(pred_J48_CV, measure="prec", x.measure="rec")
perf_J48_CV_preRecall@x.values[[1]] # Recall values
perf_J48_CV_preRecall@y.values[[1]] # Precision values
paste0(round(100*perf_J48_CV_preRecall@x.values[[1]][2], 2), "%") # Recall values
paste0(round(100*perf_J48_CV_preRecall@y.values[[1]][2], 2), "%") # Precision values

#Accuracy
perf_J48_CV_acc <- performance(pred_J48_CV, "acc")
perf_J48_CV_acc@y.values[[1]] #Accuracy
paste0(round(100*perf_J48_CV_acc@y.values[[1]][2], 2), "%") #Accuracy

#ROC curve
perf_J48_CV_ROC <- performance(pred_J48_CV,  measure = "tpr", x.measure = "fpr")
plot(perf_J48_CV_ROC, main = "ROC curve for C4.5 CV method", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)


#AUC
perf_J48_CV_AUC <- performance(pred_J48_CV, measure = "auc")
perf_J48_CV_AUC@y.values[[1]] #AUC
paste0(round(100*perf_J48_CV_AUC@y.values[[1]], 2), "%") #AUC

########################
###                  ###
### DECISION TREE    ###
###   (C 4.5)        ###
### LEAVE ONE OUT    ###
###   METHOD         ###
###                  ###
########################
confusionMatrix(predict_J48_LOOCV, data_test$class)
#Precision/recall
pred_J48_LOOCV <- prediction(as.numeric(predict_J48_LOOCV), as.numeric(data_test$class))
perf_J48_LOOCV_preRecall <- performance(pred_J48_LOOCV, measure="prec", x.measure="rec")
perf_J48_LOOCV_preRecall@x.values[[1]] # Recall values
perf_J48_LOOCV_preRecall@y.values[[1]] # Precision values
paste0(round(100*perf_J48_LOOCV_preRecall@x.values[[1]][2], 2), "%") # Recall values
paste0(round(100*perf_J48_LOOCV_preRecall@y.values[[1]][2], 2), "%") # Precision values


#Accuracy
perf_J48_LOOCV_acc <- performance(pred_J48_LOOCV, "acc")
perf_J48_LOOCV_acc@y.values[[1]] #Accuracy
paste0(round(100*perf_J48_LOOCV_acc@y.values[[1]][2], 2), "%")

#ROC curve
perf_J48_LOOCV_ROC <- performance(pred_J48_LOOCV,  measure = "tpr", x.measure = "fpr")
plot(perf_J48_LOOCV_ROC, main = "ROC curve for C4.5 leave one out method", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)



#AUC
perf_J48_LOOCV_AUC <- performance(pred_J48_LOOCV, measure = "auc")
perf_J48_LOOCV_AUC@y.values[[1]] #AUC
paste0(round(100*perf_J48_LOOCV_AUC@y.values[[1]], 2), "%") #AUC

########################
###                  ###
### DECISION TREE    ###
### RANDOM FOREST    ###
###   HOLDOUT        ###
###   METHOD         ###
###                  ###
########################
confusionMatrix(predict_RF_HO_1, data_test$class)

#Precision/recall
pred_RF_HO <- prediction(as.numeric(predict_RF_HO_1), as.numeric(data_test$class))
perf_RF_HO_preRecall <- performance(pred_RF_HO, measure="prec", x.measure="rec")
perf_RF_HO_preRecall@x.values[[1]] # Recall values
perf_RF_HO_preRecall@y.values[[1]] # Precision values
paste0(round(100*perf_RF_HO_preRecall@x.values[[1]][2], 2), "%") # Recall values
paste0(round(100*perf_RF_HO_preRecall@y.values[[1]][2], 2), "%") # Precision values

#Accuracy
perf_RF_HO_acc <- performance(pred_RF_HO, "acc")
perf_RF_HO_acc@y.values[[1]] #Accuracy
paste0(round(100*perf_RF_HO_acc@y.values[[1]][2], 2), "%") #Accuracy

#ROC curve
perf_RF_HO_ROC <- performance(pred_RF_HO,  measure = "tpr", x.measure = "fpr")
plot(perf_RF_HO_ROC, main = "ROC curve for Random Forest Holdout method", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

#AUC
perf_RF_HO_AUC <- performance(pred_RF_HO, measure = "auc")
perf_RF_HO_AUC@y.values[[1]] #AUC
paste0(round(100*perf_RF_HO_AUC@y.values[[1]], 2), "%") #AUC

########################
###                  ###
### DECISION TREE    ###
### RANDOM FOREST    ###
###   10 FOLDS       ###
### CROSS VALIDATION ###
###   METHOD         ###
########################
confusionMatrix(predict_RF_CV, data_test$class)
#Precision/recall
pred_RF_CV <- prediction(as.numeric(predict_RF_CV), as.numeric(data_test$class))
perf_RF_CV_preRecall <- performance(pred_RF_CV, measure="prec", x.measure="rec")
perf_RF_CV_preRecall@x.values[[1]] # Recall values
perf_RF_CV_preRecall@y.values[[1]] # Precision values
paste0(round(100*perf_RF_CV_preRecall@x.values[[1]][2], 2), "%") # Recall values
paste0(round(100*perf_RF_CV_preRecall@y.values[[1]][2], 2), "%") # Precision values

#Accuracy
perf_RF_CV_acc <- performance(pred_RF_CV, "acc")
perf_RF_CV_acc@y.values[[1]] #Accuracy
paste0(round(100*perf_RF_CV_acc@y.values[[1]][2], 2), "%") #Accuracy

#ROC curve
perf_RF_CV_ROC <- performance(pred_RF_CV,  measure = "tpr", x.measure = "fpr")
plot(perf_RF_CV_ROC, main = "ROC curve for Random Forest cv method", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

#AUC
perf_RF_CV_AUC <- performance(pred_RF_CV, measure = "auc")
perf_RF_CV_AUC@y.values[[1]] #AUC
paste0(round(100*perf_RF_CV_AUC@y.values[[1]], 2), "%") #AUC

########################
###                  ###
### DECISION TREE    ###
### RANDOM FOREST    ###
### LEAVE ONE OUT    ###
###   METHOD         ###
###                  ###
########################
confusionMatrix(predict_RF_LOOCV, data_test$class)
#Precision/recall
pred_RF_LOOCV <- prediction(as.numeric(predict_RF_LOOCV), as.numeric(data_test$class))
perf_RF_LOOCV_preRecall <- performance(pred_RF_LOOCV, measure="prec", x.measure="rec")
perf_RF_LOOCV_preRecall@x.values[[1]] # Recall values
perf_RF_LOOCV_preRecall@y.values[[1]] # Precision values
paste0(round(100*perf_RF_LOOCV_preRecall@x.values[[1]][2], 2), "%") # Recall values
paste0(round(100*perf_RF_LOOCV_preRecall@y.values[[1]][2], 2), "%") # Precision values

#Accuracy
perf_RF_LOOCV_acc <- performance(pred_RF_LOOCV, "acc")
perf_RF_LOOCV_acc@y.values[[1]] #Accuracy
paste0(round(100*perf_RF_LOOCV_acc@y.values[[1]][2], 2), "%") #Accuracy

#ROC curve
perf_RF_LOOCV_ROC <- performance(pred_RF_LOOCV,  measure = "tpr", x.measure = "fpr")
plot(perf_RF_LOOCV_ROC, main = "ROC curve for Random Forest leave one out method", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

#AUC
perf_RF_LOOCV_AUC <- performance(pred_RF_LOOCV, measure = "auc")
perf_RF_LOOCV_AUC@y.values[[1]] #AUC
paste0(round(100*perf_RF_LOOCV_AUC@y.values[[1]], 2), "%") #AUC


########################
###                  ###
### NÄIVE BAYES      ###
###   HOLDOUT        ###
###   METHOD         ###
###                  ###
########################
confusionMatrix(predict_NB_HO_1$class, data_test$class )

#Precision/recall
pred_NB_HO <- prediction(as.numeric(predict_NB_HO_1$class), as.numeric(data_test$class))
perf_NB_HO_preRecall <- performance(pred_NB_HO, measure="prec", x.measure="rec")
perf_NB_HO_preRecall@x.values[[1]] # Recall values
perf_NB_HO_preRecall@y.values[[1]] # Precision values
paste0(round(100*perf_NB_HO_preRecall@x.values[[1]][2], 2), "%") # Recall values
paste0(round(100*perf_NB_HO_preRecall@y.values[[1]][2], 2), "%") # Precision values


#Accuracy
perf_NB_HO_acc <- performance(pred_NB_HO, "acc")
perf_NB_HO_acc@y.values[[1]] #Accuracy
paste0(round(100*perf_NB_HO_acc@y.values[[1]][2], 2), "%") #Accuracy

#ROC curve
perf_NB_HO_ROC <- performance(pred_NB_HO,  measure = "tpr", x.measure = "fpr")
plot(perf_NB_HO_ROC, main = "ROC curve for Naive Bayes Holdout method", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

#AUC
perf_NB_HO_AUC <- performance(pred_NB_HO, measure = "auc")
perf_NB_HO_AUC@y.values[[1]] #AUC
paste0(round(100*perf_NB_HO_AUC@y.values[[1]], 2), "%") #AUC

########################
###                  ###
### NÄIVE BAYES      ###
###   10 FOLDS       ###
### CROSS VALIDATION ###
###   METHOD         ###
########################
confusionMatrix(predict_NB_CV, data_test$class)
#Precision/recall
pred_NB_CV <- prediction(as.numeric(predict_NB_CV), as.numeric(data_test$class))
perf_NB_CV_preRecall <- performance(pred_NB_CV, measure="prec", x.measure="rec")
perf_NB_CV_preRecall@x.values[[1]] # Recall values
perf_NB_CV_preRecall@y.values[[1]] # Precision values
paste0(round(100*perf_NB_CV_preRecall@x.values[[1]][2], 2), "%") # Recall values
paste0(round(100*perf_NB_CV_preRecall@y.values[[1]][2], 2), "%") # Precision values


#Accuracy
perf_NB_CV_acc <- performance(pred_NB_CV, "acc")
perf_NB_CV_acc@y.values[[1]] #Accuracy
paste0(round(100*perf_NB_CV_acc@y.values[[1]][2], 2), "%") #Accuracy

#ROC curve
perf_NB_CV_ROC <- performance(pred_NB_CV,  measure = "tpr", x.measure = "fpr")
plot(perf_NB_CV_ROC, main = "ROC curve for Naive Bayes cv method", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

#AUC
perf_NB_CV_AUC <- performance(pred_NB_CV, measure = "auc")
perf_NB_CV_AUC@y.values[[1]] #AUC
paste0(round(100*perf_NB_CV_AUC@y.values[[1]], 2), "%") #AUC

########################
###                  ###
### NÄIVE BAYES      ###
### LEAVE ONE OUT    ###
###   METHOD         ###
###                  ###
########################
confusionMatrix(predict_NB_LOOCV, data_test$class)
#Precision/recall
pred_NB_LOOCV <- prediction(as.numeric(predict_NB_LOOCV), as.numeric(data_test$class))
perf_NB_LOOCV_preRecall <- performance(pred_NB_LOOCV, measure="prec", x.measure="rec")
perf_NB_LOOCV_preRecall@x.values[[1]] # Recall values
perf_NB_LOOCV_preRecall@y.values[[1]] # Precision values
paste0(round(100*perf_NB_LOOCV_preRecall@x.values[[1]][2], 2), "%") # Recall values
paste0(round(100*perf_NB_LOOCV_preRecall@y.values[[1]][2], 2), "%") # Precision values


#Accuracy
perf_NB_LOOCV_acc <- performance(pred_NB_LOOCV, "acc")
perf_NB_LOOCV_acc@y.values[[1]] #Accuracy
paste0(round(100*perf_NB_LOOCV_acc@y.values[[1]][2], 2), "%") #Accuracy

#ROC curve
perf_NB_LOOCV_ROC <- performance(pred_NB_LOOCV,  measure = "tpr", x.measure = "fpr")
plot(perf_NB_LOOCV_ROC, main = "ROC curve for Naive Bayes leave one out method", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

#AUC
perf_NB_LOOCV_AUC <- performance(pred_NB_LOOCV, measure = "auc")
perf_NB_LOOCV_AUC@y.values[[1]] #AUC
paste0(round(100*perf_NB_LOOCV_AUC@y.values[[1]], 2), "%") #AUC

########################
###                  ###
###   KNN            ###
###   HOLDOUT        ###
###   METHOD         ###
###                  ###
########################
confusionMatrix(model_KNN_HO_1, data_test$class )

#Precision/recall
pred_KNN_HO <- prediction(as.numeric(model_KNN_HO_1), as.numeric(data_test$class))
perf_KNN_HO_preRecall <- performance(pred_KNN_HO, measure="prec", x.measure="rec")
perf_KNN_HO_preRecall@x.values[[1]] # Recall values
perf_KNN_HO_preRecall@y.values[[1]] # Precision values
paste0(round(100*perf_KNN_HO_preRecall@x.values[[1]][2], 2), "%") # Recall values
paste0(round(100*perf_KNN_HO_preRecall@y.values[[1]][2], 2), "%") # Precision values


#Accuracy
perf_KNN_HO_acc <- performance(pred_KNN_HO, "acc")
perf_KNN_HO_acc@y.values[[1]] #Accuracy
paste0(round(100*perf_KNN_HO_acc@y.values[[1]][2], 2), "%") #Accuracy

#ROC curve
perf_KNN_HO_ROC <- performance(pred_KNN_HO,  measure = "tpr", x.measure = "fpr")
plot(perf_KNN_HO_ROC, main = "ROC curve for KNN method", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

#AUC
perf_KNN_HO_AUC <- performance(pred_KNN_HO, measure = "auc")
perf_KNN_HO_AUC@y.values[[1]] #AUC
paste0(round(100*perf_KNN_HO_AUC@y.values[[1]], 2), "%") #AUC

########################
###                  ###
###      KNN         ###
###   10 FOLDS       ###
### CROSS VALIDATION ###
###   METHOD         ###
########################
confusionMatrix(predict_KNN_CV, data_test$class)
#Precision/recall
pred_KNN_CV <- prediction(as.numeric(predict_KNN_CV), as.numeric(data_test$class))
perf_KNN_CV_preRecall <- performance(pred_KNN_CV, measure="prec", x.measure="rec")
perf_KNN_CV_preRecall@x.values[[1]] # Recall values
perf_KNN_CV_preRecall@y.values[[1]] # Precision values
paste0(round(100*perf_KNN_CV_preRecall@x.values[[1]][2], 2), "%") # Recall values
paste0(round(100*perf_KNN_CV_preRecall@y.values[[1]][2], 2), "%") # Precision values


#Accuracy
perf_KNN_CV_acc <- performance(pred_KNN_CV, "acc")
perf_KNN_CV_acc@y.values[[1]] #Accuracy
paste0(round(100*perf_KNN_CV_acc@y.values[[1]][2], 2), "%") #Accuracy

#ROC curve
perf_KNN_CV_ROC <- performance(pred_KNN_CV,  measure = "tpr", x.measure = "fpr")
plot(perf_KNN_CV_ROC, main = "ROC curve for knn cv method", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

#AUC
perf_KNN_CV_AUC <- performance(pred_KNN_CV, measure = "auc")
perf_KNN_CV_AUC@y.values[[1]] #AUC
paste0(round(100*perf_KNN_CV_AUC@y.values[[1]], 2), "%") #AUC

########################
###                  ###
### KNN              ###
### LEAVE ONE OUT    ###
### METHOD           ###
###                  ###
########################
confusionMatrix(predict_KNN_LOOCV, data_test$class)
#Precision/recall
pred_KNN_LOOCV <- prediction(as.numeric(predict_KNN_LOOCV), as.numeric(data_test$class))
perf_KNN_LOOCV_preRecall <- performance(pred_KNN_LOOCV, measure="prec", x.measure="rec")
perf_KNN_LOOCV_preRecall@x.values[[1]] # Recall values
perf_KNN_LOOCV_preRecall@y.values[[1]] # Precision values
paste0(round(100*perf_KNN_LOOCV_preRecall@x.values[[1]][2], 2), "%") # Recall values
paste0(round(100*perf_KNN_LOOCV_preRecall@y.values[[1]][2], 2), "%") # Precision values


#Accuracy
perf_KNN_LOOCV_acc <- performance(pred_KNN_LOOCV, "acc")
perf_KNN_LOOCV_acc@y.values[[1]] #Accuracy
paste0(round(100*perf_KNN_LOOCV_acc@y.values[[1]][2], 2), "%") #Accuracy

#ROC curve
perf_KNN_LOOCV_ROC <- performance(pred_KNN_LOOCV,  measure = "tpr", x.measure = "fpr")
plot(perf_KNN_LOOCV_ROC, main = "ROC curve for knn leave one out method", col = "blue", lwd = 3)
abline(a = 0, b = 1, lwd = 2, lty = 2)

#AUC
perf_KNN_LOOCV_AUC <- performance(pred_KNN_LOOCV, measure = "auc")
perf_KNN_LOOCV_AUC@y.values[[1]] #AUC
paste0(round(100*perf_KNN_LOOCV_AUC@y.values[[1]], 2), "%") #AUC





