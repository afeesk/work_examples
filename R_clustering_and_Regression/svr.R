#require(e1071) 
install.packages("e1071")
library(e1071)
library(tidyverse)

#load exchange dataset
exchange <- readxl::read_xlsx("Exchange.xls.xlsx", sheet = 1)

##Explore and prepare the data
#check attribute and info of the exchange data
head(exchange)
str(exchange)

#Rename variables in the exchange to proper variable names
exchange <- rename(exchange, date = `YYYY/MM/DD`, usd_eur = `USD/EUR`)

#check to see the effect
head(exchange)
nrow(exchange)
#get the column with exchange rate
usd_eur <- exchange$usd_eur

#check
head(usd_eur)

#Data preparation for neural net
#Convert time series into a supervised learning problem
#check for missing value
#use suitable code
anyNA(usd_eur)

# A function that will convert our data into the required dataset matrix for NN
#Two inputs and one output
create_dataset_2in <- function(dataset){
  look_back <- 1
  dataA <- c(); dataB <- c(); data_out <- c()
  for(i in look_back:(length(dataset)-look_back)){
    
    dataA <- append(dataA, dataset[i])
    dataB <- append(dataB, dataset[i + look_back])
    data_out <- append(data_out, dataset[i + look_back + 1])
  }
  result <- as.data.frame(cbind(dataA, dataB, data_out))
  result
}

#Let's test our function on the usd_eur data
usd_eur_checker_2 <- create_dataset_2in(usd_eur) 
head(usd_eur_checker_2, 10)
tail(usd_eur_checker_2)
str(usd_eur_checker_2)

keep <- usd_eur_checker_2
keep[321:326, ]

#Remove all columns with NA
usd_eur_checker_2 <- drop_na(usd_eur_checker_2)
summary(usd_eur_checker_2)

#To normalize the usd_eur data,
#write the normalize function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#Apply the normalize function to every column in the usd_eur data 
#summarize to verify
usd_eur_norm_2 <- as.data.frame(lapply(usd_eur_checker_2, normalize))
summary(usd_eur_norm_2)

#Train a model on the data
nrow(usd_eur_norm_2)

#We have 390 observation, therefore I'll split it into first 320 for training set,
#and the last 70 for test set
usd_eur_train_2 <- usd_eur_norm_2[1:320, ] #320 training set
usd_eur_test_2 <- usd_eur_norm_2[321:388, ] #68 test set
nrow(usd_eur_train_2)
nrow(usd_eur_test_2)
head(usd_eur_train_2)
tail(usd_eur_test_2)
head(usd_eur_test_2)


#Fit a SVR model
set.seed(3211)
svr_model_2_1 <- svm(data_out ~ dataA + dataB, data = usd_eur_train_2, kernel='linear')
#View model summary
summary(svr_model_2_1)
svr_model_2_1_predict <- predict(svr_model_2_1, usd_eur_test_2[, 1:2])
svr_model_2_1_predict
###
#######
##De-normalize
#######
#Write a de-normalize fuction
#De-normalizer
minvec <- sapply(usd_eur_checker_2, min)
maxvec <- sapply(usd_eur_checker_2, max)
denormalize <- function(x, minval, maxval) {
  x*(maxval - minval) + minval
}

test_denorm_2 <- as.data.frame(Map(denormalize, usd_eur_test_2, minvec, maxvec))
test_denorm_out_2 <- test_denorm_2$data_out
head(test_denorm_out_2)
length(test_denorm_out_2)

pred_svr_2_1 <- as.data.frame(svr_model_2_1_predict)
head(pred_svr_2_1)
pred_svr_denorm_2_1 <- as.data.frame(Map(denormalize, pred_svr_2_1, minvec, maxvec))
pred_svr_denorm_2_1 <- pred_svr_denorm_2_1[, 1]
head(pred_svr_denorm_2_1)
length(pred_svr_denorm_2_1)

#plot the line graph
series_svr_2_1 <- cbind(seq_along(test_denorm_out_2), test_denorm_out_2, pred_svr_denorm_2_1)
head(series_svr_2_1)

colnames(series_svr_2_1) <- c("Time","Expected Output","SVR Output")
series_svr_2_1 <- as.data.frame(series_svr_2_1)
head(series_svr_2_1)

ggplot(series_svr_2_1, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `SVR Output`, colour = "SVR Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



######
#mse 
#####
mse <- function(actual, predicted){
  sum((actual - predicted)^2)/length(actual)
}
mse_svr_2_1 <- mse(series_svr_2_1$`Expected Output`, series_svr_2_1$`SVR Output`) 
mse_svr_2_1
######
#rmse 
#####
rsme <- function(actual, predicted){
  sqrt(sum((actual - predicted)^2)/length(actual))
}
rmse_svr_2_1 <- rsme(series_svr_2_1$`Expected Output`, series_svr_2_1$`SVR Output`)  
rmse_svr_2_1
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape <- function(actual, predicted){
  paste0(round(mean(100*abs((actual - predicted)/ actual)), 6) , "%")
}
mape_svr_2_1 <- mape(series_svr_2_1$`Expected Output`, series_svr_2_1$`SVR Output`)  
mape_svr_2_1

#####################################################################################################
svr_model_2_2_tune <- tune(svm, data_out ~ dataA + dataB, data = usd_eur_train_2, kernel='linear', 
                     ranges = list(gamma = seq(.05,.11,.01), cost = seq(1,4,0.5)),
                     tunecontrol = tune.control(sampling = "cross"))

summary(svr_model_2_2_tune)
ls(svr_model_2_2_tune)
par( mfrow = c( 1, 1) )
plot(svr_model_2_2_tune)
svr_model_2_2_tune$best.parameters


svr_model_2_2 <- svm(data_out ~ dataA + dataB, data = usd_eur_train_2, kernel='linear', 
                     gamma = 0.05, cost = 1)
#View model summary
summary(svr_model_2_2)
svr_model_2_2_predict <- predict(svr_model_2_2, usd_eur_test_2[, 1:2])
svr_model_2_2_predict
###
#######
##De-normalize
#######
#Write a de-normalize fuction
#De-normalizer

pred_svr_2_2 <- as.data.frame(svr_model_2_2_predict)
head(pred_svr_2_2)
pred_svr_denorm_2_2 <- as.data.frame(Map(denormalize, pred_svr_2_2, minvec, maxvec))
pred_svr_denorm_2_2 <- pred_svr_denorm_2_2[, 1]
head(pred_svr_denorm_2_2)
length(pred_svr_denorm_2_2)

#plot the line graph
series_svr_2_2 <- cbind(seq_along(test_denorm_out_2), test_denorm_out_2, pred_svr_denorm_2_2)
head(series_svr_2_2)

colnames(series_svr_2_2) <- c("Time","Expected Output","SVR Output")
series_svr_2_2 <- as.data.frame(series_svr_2_2)
head(series_svr_2_2)

ggplot(series_svr_2_2, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `SVR Output`, colour = "SVR Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



######
#mse 
#####

mse_svr_2_2 <- mse(series_svr_2_2$`Expected Output`, series_svr_2_2$`SVR Output`) 
mse_svr_2_2
######
#rmse 
#####

rmse_svr_2_2 <- rsme(series_svr_2_2$`Expected Output`, series_svr_2_2$`SVR Output`)  
rmse_svr_2_2
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape_svr_2_2 <- mape(series_svr_2_2$`Expected Output`, series_svr_2_2$`SVR Output`)  
mape_svr_2_2


#########################################################################################################

svr_model_2_3 <- svm(data_out ~ dataA + dataB, data = usd_eur_train_2)
#View model summary
summary(svr_model_2_3)
svr_model_2_3_predict <- predict(svr_model_2_3, usd_eur_test_2[, 1:2])
svr_model_2_3_predict
###
#######
##De-normalize
#######

pred_svr_2_3 <- as.data.frame(svr_model_2_3_predict)
head(pred_svr_2_3)
pred_svr_denorm_2_3 <- as.data.frame(Map(denormalize, pred_svr_2_3, minvec, maxvec))
pred_svr_denorm_2_3 <- pred_svr_denorm_2_3[, 1]
head(pred_svr_denorm_2_3)
length(pred_svr_denorm_2_3)

#plot the line graph
series_svr_2_3 <- cbind(seq_along(test_denorm_out_2), test_denorm_out_2, pred_svr_denorm_2_3)
head(series_svr_2_3)

colnames(series_svr_2_3) <- c("Time","Expected Output","SVR Output")
series_svr_2_3 <- as.data.frame(series_svr_2_3)
head(series_svr_2_3)

ggplot(series_svr_2_3, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `SVR Output`, colour = "SVR Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



######
#mse 
#####
mse_svr_2_3 <- mse(series_svr_2_3$`Expected Output`, series_svr_2_3$`SVR Output`) 
mse_svr_2_3
######
#rmse 
#####

rmse_svr_2_3 <- rsme(series_svr_2_3$`Expected Output`, series_svr_2_3$`SVR Output`)  
rmse_svr_2_3
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape_svr_2_3 <- mape(series_svr_2_3$`Expected Output`, series_svr_2_3$`SVR Output`)  
mape_svr_2_3

###################################################################################################
#####################################################################################################
svr_model_2_4_tune <- tune(svm, data_out ~ dataA + dataB, data = usd_eur_train_2, 
                           ranges = list(gamma = seq(.05,.11,.01), cost = seq(1,4,0.5)),
                           tunecontrol = tune.control(sampling = "cross"))

summary(svr_model_2_4_tune)

plot(svr_model_2_4_tune)
svr_model_2_4_tune$best.parameters


svr_model_2_4 <- svm(data_out ~ dataA + dataB, data = usd_eur_train_2, gamma = 0.06, cost = 4)
#View model summary
summary(svr_model_2_4)
svr_model_2_4_predict <- predict(svr_model_2_4, usd_eur_test_2[, 1:2])
svr_model_2_4_predict
###
#######
##De-normalize
#######
#Write a de-normalize fuction
#De-normalizer

pred_svr_2_4 <- as.data.frame(svr_model_2_4_predict)
head(pred_svr_2_4)
pred_svr_denorm_2_4 <- as.data.frame(Map(denormalize, pred_svr_2_4, minvec, maxvec))
pred_svr_denorm_2_4 <- pred_svr_denorm_2_4[, 1]
head(pred_svr_denorm_2_4)
length(pred_svr_denorm_2_4)

#plot the line graph
series_svr_2_4 <- cbind(seq_along(test_denorm_out_2), test_denorm_out_2, pred_svr_denorm_2_4)
head(series_svr_2_4)

colnames(series_svr_2_4) <- c("Time","Expected Output","SVR Output")
series_svr_2_4 <- as.data.frame(series_svr_2_4)
head(series_svr_2_4)

ggplot(series_svr_2_4, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `SVR Output`, colour = "SVR Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



######
#mse 
#####

mse_svr_2_4 <- mse(series_svr_2_4$`Expected Output`, series_svr_2_4$`SVR Output`) 
mse_svr_2_4
######
#rmse 
#####

rmse_svr_2_4 <- rsme(series_svr_2_4$`Expected Output`, series_svr_2_4$`SVR Output`)  
rmse_svr_2_4
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape_svr_2_4 <- mape(series_svr_2_4$`Expected Output`, series_svr_2_4$`SVR Output`)  
mape_svr_2_4

#####################################
####################################
##### 3INPUTS 
####################################
####################################
##################################
### 3 INPUTS
#################################

# A function that will convert our data into the required dataset matrix for NN
#Three inputs and one output
create_dataset_3in <- function(dataset){
  look_back <- 1
  dataA <- c(); dataB <- c(); dataC <- c(); data_out <- c()
  for(i in look_back:(length(dataset)-look_back)){
    
    dataA <- append(dataA, dataset[i])
    dataB <- append(dataB, dataset[i + look_back])
    dataC <- append(dataC, dataset[i + look_back + 1])
    data_out <- append(data_out, dataset[i + look_back + 2])
  }
  result <- as.data.frame(cbind(dataA, dataB, dataC, data_out))
  result
}

#Let's test our function on the usd_eur data
usd_eur_checker_3 <- create_dataset_3in(usd_eur) 
head(usd_eur_checker_3, 10)
tail(usd_eur_checker_3)
str(usd_eur_checker_3)

#Remove all columns with NA
usd_eur_checker_3 <- drop_na(usd_eur_checker_3)
summary(usd_eur_checker_3)

#To normalize the usd_eur data,
#write the normalize function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#Apply the normalize function to every column in the usd_eur data 
#summarize to verify
usd_eur_norm_3 <- as.data.frame(lapply(usd_eur_checker_3, normalize))
summary(usd_eur_norm_3)

#Train a model on the data
nrow(usd_eur_norm_3)

#We have 390 observation, therefore I'll split it into first 320 for training set,
#and the last 70 for test set
usd_eur_train_3 <- usd_eur_norm_3[1:320, ]
usd_eur_test_3 <- usd_eur_norm_3[321:387, ]
nrow(usd_eur_train_3)
nrow(usd_eur_test_3)
head(usd_eur_train_3)
tail(usd_eur_test_3)

##################################
### 3 INPUTS, model1
#################################

#Fit a neural network model
svr_model_3_1 <- svm(data_out ~ dataA + dataB + dataC, data = usd_eur_train_3, kernel='linear')
#View model summary
summary(svr_model_3_1)
svr_model_3_1_predict <- predict(svr_model_3_1, usd_eur_test_3[, 1:3])
svr_model_3_1_predict
###
#######
##De-normalize
#######
#Write a de-normalize fuction
#De-normalizer
minvec <- sapply(usd_eur_checker_3, min)
maxvec <- sapply(usd_eur_checker_3, max)
denormalize <- function(x, minval, maxval) {
  x*(maxval - minval) + minval
}

test_denorm_3 <- as.data.frame(Map(denormalize, usd_eur_test_3, minvec, maxvec))
test_denorm_out_3 <- test_denorm_2$data_out
head(test_denorm_out_3)
length(test_denorm_out_3)

pred_svr_3_1 <- as.data.frame(svr_model_3_1_predict)
head(pred_svr_3_1)
pred_svr_denorm_3_1 <- as.data.frame(Map(denormalize, pred_svr_3_1, minvec, maxvec))
pred_svr_denorm_3_1 <- pred_svr_denorm_3_1[, 1]
head(pred_svr_denorm_3_1)
length(pred_svr_denorm_3_1)

#plot the line graph
series_svr_3_1 <- cbind(seq_along(test_denorm_out_3), test_denorm_out_3, pred_svr_denorm_3_1)
head(series_svr_3_1)

colnames(series_svr_3_1) <- c("Time","Expected Output","SVR Output")
series_svr_3_1 <- as.data.frame(series_svr_3_1)
head(series_svr_3_1)

ggplot(series_svr_3_1, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `SVR Output`, colour = "SVR Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



######
#mse 
#####
mse_svr_3_1 <- mse(series_svr_3_1$`Expected Output`, series_svr_3_1$`SVR Output`) 
mse_svr_3_1
######
#rmse 
#####
rmse_svr_3_1 <- rsme(series_svr_3_1$`Expected Output`, series_svr_3_1$`SVR Output`)  
rmse_svr_3_1
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape_svr_3_1 <- mape(series_svr_3_1$`Expected Output`, series_svr_3_1$`SVR Output`)  
mape_svr_3_1
###############################################################################################
##MODEL 2
####################################################################################
##################################
### 3 INPUTS, model1
#################################

#Fit a neural network model
svr_model_3_2_tune <- tune(svm, data_out ~ dataA + dataB + dataC, data = usd_eur_train_3, kernel= "linear",
                           ranges = list(gamma = seq(.05,.11,.01), cost = seq(1,4,0.5)),
                           tunecontrol = tune.control(sampling = "cross"))

summary(svr_model_3_2_tune)

plot(svr_model_3_2_tune)
svr_model_3_2_tune$best.parameters


svr_model_3_2 <- svm(data_out ~ dataA + dataB + dataC, data = usd_eur_train_3,kernel = "linear",
                     gamma = 0.05, cost = 2.5)
#View model summary
summary(svr_model_3_2)
svr_model_3_2_predict <- predict(svr_model_3_2, usd_eur_test_3[, 1:3])
svr_model_3_2_predict
###
###
#######
##De-normalize
#######

pred_svr_3_2 <- as.data.frame(svr_model_3_2_predict)
head(pred_svr_3_2)
pred_svr_denorm_3_2 <- as.data.frame(Map(denormalize, pred_svr_3_2, minvec, maxvec))
pred_svr_denorm_3_2 <- pred_svr_denorm_3_2[, 1]
head(pred_svr_denorm_3_2)
length(pred_svr_denorm_3_2)

#plot the line graph
series_svr_3_2 <- cbind(seq_along(test_denorm_out_3), test_denorm_out_3, pred_svr_denorm_3_2)
head(series_svr_3_2)

colnames(series_svr_3_2) <- c("Time","Expected Output","SVR Output")
series_svr_3_2 <- as.data.frame(series_svr_3_2)
head(series_svr_3_2)

ggplot(series_svr_3_2, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `SVR Output`, colour = "SVR Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



######
#mse 
#####
mse_svr_3_2 <- mse(series_svr_3_2$`Expected Output`, series_svr_3_2$`SVR Output`) 
mse_svr_3_2
######
#rmse 
#####
rmse_svr_3_2 <- rsme(series_svr_3_2$`Expected Output`, series_svr_3_2$`SVR Output`)  
rmse_svr_3_2
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape_svr_3_2 <- mape(series_svr_3_2$`Expected Output`, series_svr_3_2$`SVR Output`)  
mape_svr_3_2

################################################################################################
#########################################################################################################
##

svr_model_3_3 <- svm(data_out ~ dataA + dataB + dataC, data = usd_eur_train_3)
#View model summary
summary(svr_model_3_3)
svr_model_3_3_predict <- predict(svr_model_3_3, usd_eur_test_3[, 1:3])
svr_model_3_3_predict
###
#######
##De-normalize
#######

pred_svr_3_3 <- as.data.frame(svr_model_3_3_predict)
head(pred_svr_3_3)
pred_svr_denorm_3_3 <- as.data.frame(Map(denormalize, pred_svr_3_3, minvec, maxvec))
pred_svr_denorm_3_3 <- pred_svr_denorm_3_3[, 1]
head(pred_svr_denorm_3_3)
length(pred_svr_denorm_3_3)

#plot the line graph
series_svr_3_3 <- cbind(seq_along(test_denorm_out_3), test_denorm_out_3, pred_svr_denorm_3_3)
head(series_svr_3_3)

colnames(series_svr_3_3) <- c("Time","Expected Output","SVR Output")
series_svr_3_3 <- as.data.frame(series_svr_3_3)
head(series_svr_3_3)

ggplot(series_svr_3_3, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `SVR Output`, colour = "SVR Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



######
#mse 
#####
mse_svr_3_3 <- mse(series_svr_3_3$`Expected Output`, series_svr_3_3$`SVR Output`) 
mse_svr_3_3
######
#rmse 
#####

rmse_svr_3_3 <- rsme(series_svr_3_3$`Expected Output`, series_svr_3_3$`SVR Output`)  
rmse_svr_3_3
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape_svr_3_3 <- mape(series_svr_3_3$`Expected Output`, series_svr_3_3$`SVR Output`)  
mape_svr_3_3
###################################################################################################
#####################################################################################################
svr_model_3_4_tune <- tune(svm, data_out ~ dataA + dataB + dataC, data = usd_eur_train_3, 
                           ranges = list(gamma = seq(.05,.11,.01), cost = seq(1,4,0.5)),
                           tunecontrol = tune.control(sampling = "cross"))

summary(svr_model_3_4_tune)

plot(svr_model_3_4_tune)
svr_model_3_4_tune$best.parameters


svr_model_3_4 <- svm(data_out ~ dataA + dataB, data = usd_eur_train_3, gamma = 0.05, cost = 3)
#View model summary
summary(svr_model_3_4)
svr_model_3_4_predict <- predict(svr_model_3_4, usd_eur_test_3[, 1:3])
svr_model_3_4_predict
###
#######
##De-normalize
#######
#Write a de-normalize fuction
#De-normalizer

pred_svr_3_4 <- as.data.frame(svr_model_3_4_predict)
head(pred_svr_3_4)
pred_svr_denorm_3_4 <- as.data.frame(Map(denormalize, pred_svr_3_4, minvec, maxvec))
pred_svr_denorm_3_4 <- pred_svr_denorm_3_4[, 1]
head(pred_svr_denorm_3_4)
length(pred_svr_denorm_3_4)

#plot the line graph
series_svr_3_4 <- cbind(seq_along(test_denorm_out_3), test_denorm_out_3, pred_svr_denorm_3_4)
head(series_svr_3_4)

colnames(series_svr_3_4) <- c("Time","Expected Output","SVR Output")
series_svr_3_4 <- as.data.frame(series_svr_3_4)
head(series_svr_3_4)

ggplot(series_svr_3_4, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `SVR Output`, colour = "SVR Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))



######
#mse 
#####

mse_svr_3_4 <- mse(series_svr_3_4$`Expected Output`, series_svr_3_4$`SVR Output`) 
mse_svr_3_4
######
#rmse 
#####

rmse_svr_3_4 <- rsme(series_svr_3_4$`Expected Output`, series_svr_3_4$`SVR Output`)  
rmse_svr_3_4
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape_svr_3_4 <- mape(series_svr_3_4$`Expected Output`, series_svr_3_4$`SVR Output`)  
mape_svr_3_4



#######################
## Error collation
######################
error_svr_2_1 <- c(mse_svr_2_1, rmse_svr_2_1, mape_svr_2_1)
error_svr_2_2 <- c(mse_svr_2_2, rmse_svr_2_2, mape_svr_2_2)
error_svr_2_3 <- c(mse_svr_2_3, rmse_svr_2_3, mape_svr_2_3)
error_svr_2_4 <- c(mse_svr_2_4, rmse_svr_2_4, mape_svr_2_4)

error_svr_3_1 <- c(mse_svr_3_1, rmse_svr_3_1, mape_svr_3_1)
error_svr_3_2 <- c(mse_svr_3_2, rmse_svr_3_2, mape_svr_3_2)
error_svr_3_3 <- c(mse_svr_3_3, rmse_svr_3_3, mape_svr_3_3)
error_svr_3_4 <- c(mse_svr_3_4, rmse_svr_3_4, mape_svr_3_4)

error_svr <- cbind(error_svr_2_1, error_svr_2_2, error_svr_2_3, error_svr_2_4, 
                  error_svr_3_1, error_svr_3_2, error_svr_3_3, error_svr_3_4)

colnames(error_svr) <- c("model1_2", "model2_2", "model3_2", "model4_2",
                        "model1_3", "model2_3", "model3_3", "model4_3")

error_svr <-  as.data.frame(error_svr)
error_svr


##visualize the best model
ggplot(series_svr_3_2, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `SVR Output`, colour = "SVR Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


nn_best <- as.data.frame(error_nn$model1_3)
svm_best <- as.data.frame(error_svr$model2_3)
compare_svm_nn_best <- cbind(nn_best, svm_best)
colnames(compare_svm_nn_best) <- c("neural net Error", "Support Vector Regression Error")
compare_svm_nn_best
