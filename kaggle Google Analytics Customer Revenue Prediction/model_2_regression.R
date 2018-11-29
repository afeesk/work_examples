registerDoSEQ()
load("training_data.RData")
load("test_data.RData")
dim(customer_train_clean_3)
dim(customer_test_clean_3)
names(customer_train_clean_3)
names(customer_test_clean_3)
rm(customer_test_clean_2, customer_train_clean_2)


###########################
###                     ###
### Linear Regression   ###
###                     ### 
###########################
# define training control
model_gbm_CV <- lm(transactionRevenue ~ ., data = customer_train_clean_3)
summary(model_gbm_CV)

model_gbm_CV
#save(model_gbm_CV, file = "model_gbm_CV.RData")
#load("model_gbm_CV.RData")

predict_gbm_CV_test <- predict(model_gbm_CV, data = customer_test_clean_3)
#save(predict_gbm_CV_test, file = "predict_gbm_CV_test.RData")
length(predict_gbm_CV_test)
length(unique(predict_gbm_CV_test))
unique(predict_gbm_CV_test)
###
#LOAD FROM SAVE
load("predict_gbm_CV_test.Rdata")
load("fullVisitorId_test.RData")
load("transaction_revenue_for_denorm.RData")

#Denorm naive way
#denormalized = (normalized)*(max(x)-min(x))+min(x)
predict_gbm_CV_test_denorm = (predict_gbm_CV_test)*
  (max(transaction_revenue_for_denorm)-min(transaction_revenue_for_denorm))+
  min(transaction_revenue_for_denorm)

#ATTACH THE fullVisitorId_test, fullVisitorId
prdicted_revenue <- data.frame(fullVisitorId = fullVisitorId_test, PredictedRevenue = predict_gbm_CV_test_denorm)
head(prdicted_revenue)
str(prdicted_revenue)

#a <- prdicted_revenue
#head(a)
#dim(a)
#group sum
prdicted_revenue_sum <- prdicted_revenue %>% group_by(fullVisitorId) %>% 
  summarise(SumPredictedRevenue = sum(PredictedRevenue))
dim(prdicted_revenue_sum)
head(prdicted_revenue_sum)
#LOG REVENUE
prdicted_revenue_sum$PredictedLogRevenue = ifelse(prdicted_revenue_sum$SumPredictedRevenue != 0, log(prdicted_revenue_sum$SumPredictedRevenue + 1), 0)
head(prdicted_revenue_sum)

submision_1 <- prdicted_revenue_sum[c(1,3)]
write.csv(submision_1, file = paste0(getwd(), "/", "submissions/submission_3.csv"), row.names = FALSE)


prdicted_revenue$PredictedRevenue = ifelse(prdicted_revenue$PredictedRevenue == min(prdicted_revenue$PredictedRevenue), 0, prdicted_revenue$PredictedRevenue)
head(prdicted_revenue)
#group sum
prdicted_revenue_sum <- prdicted_revenue %>% group_by(fullVisitorId) %>% 
  summarise(SumPredictedRevenue = sum(PredictedRevenue))
dim(prdicted_revenue_sum)
head(prdicted_revenue_sum)
#LOG REVENUE
prdicted_revenue_sum$PredictedLogRevenue = ifelse(prdicted_revenue_sum$SumPredictedRevenue != 0, log(prdicted_revenue_sum$SumPredictedRevenue + 1), 0)
head(prdicted_revenue_sum)

submision_2 <- prdicted_revenue_sum[c(1,3)]

write.csv(submision_2, file = paste0(getwd(), "/", "submissions/submission_4.csv"), row.names = FALSE)

