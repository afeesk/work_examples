
#install.packages("neuralnet")
#install.packages("NeuralNetTools")

## Loading required package: grid
## Loading required package: MASS
library(neuralnet)
library(grid)
library(MASS)
library(NeuralNetTools)
library(tidyverse)
set.seed(3211)
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
#CHECK FOR NA's
anyNA(usd_eur_checker_2)

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


#Fit a neural network model
set.seed(3211)
usd_eur_model2_1 <- neuralnet(data_out ~ dataA + dataB, data = usd_eur_train_2)
#View model summary
print(usd_eur_model2_1)

#Plot the model to see nn graph
plot(usd_eur_model2_1, alpha = 0.6)
# plotnet
par(mar = numeric(4), family = 'serif')
plotnet(usd_eur_model2_1, alpha = 0.6)






#######
##MODEL PERFORMANCE EVALUATION
#######
net_results_2_1 <- neuralnet::compute(usd_eur_model2_1, usd_eur_test_2[, 1:2])

pred_2_1 <- net_results_2_1$net.result
cor(pred_2_1, usd_eur_test_2$data_out)

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

ls(net_results_2_1)
pred_all_2_1 <- as.data.frame(net_results_2_1$net.result)
head(pred_all_2_1)
pred_denorm_2_1 <- as.data.frame(Map(denormalize, pred_all_2_1, minvec, maxvec))
pred_denorm_2_1 <- pred_denorm_2_1[, 1]
head(pred_denorm_2_1)
length(pred_denorm_2_1)

#plot the line graph
series_2_1 <- cbind(seq_along(test_denorm_out_2), test_denorm_out_2, pred_denorm_2_1)
head(series_2_1)

colnames(series_2_1) <- c("Time","Expected Output","Neural Net Output")
series_2_1 <- as.data.frame(series_2_1)
head(series_2_1)

ggplot(series_2_1, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `Neural Net Output`, colour = "Neural Net Output"))+
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
mse_2_1 <- mse(series_2_1$`Expected Output`, series_2_1$`Neural Net Output`) 
mse_2_1
######
#rmse 
#####
rsme <- function(actual, predicted){
  sqrt(sum((actual - predicted)^2)/length(actual))
}
rmse_2_1 <- rsme(series_2_1$`Expected Output`, series_2_1$`Neural Net Output`)  
rmse_2_1
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape <- function(actual, predicted){
  paste0(round(mean(100*abs((actual - predicted)/ actual)), 6) , "%")
}
mape_2_1 <- mape(series_2_1$`Expected Output`, series_2_1$`Neural Net Output`)  
mape_2_1


##############
## 2 input, model2
##################
usd_eur_model2_2 <- neuralnet(data_out ~ dataA + dataB, data = usd_eur_train_2, hidden = 8)
#View model summary
print(usd_eur_model2_2)

#Plot the model to see nn graph
plot(usd_eur_model2_2, alpha = 0.6)
# plotnet
par(mar = numeric(4), family = 'serif')
plotnet(usd_eur_model2_2, alpha = 0.6)

#######
##MODEL correlation
#######
net_results_2_2 <- neuralnet::compute(usd_eur_model2_2, usd_eur_test_2[, 1:2])
pred_2_2 <- net_results_2_2$net.result
cor(pred_2_2, usd_eur_test_2$data_out)

pred_all_2_2 <- as.data.frame(net_results_2_2$net.result)
head(pred_all_2_2)
pred_denorm_2_2 <- as.data.frame(Map(denormalize, pred_all_2_2, minvec, maxvec))
pred_denorm_2_2n <- pred_denorm_2_2[, 1]
head(pred_denorm_2_2n)
length(pred_denorm_2_2n)

#plot the line graph
series_2_2 <- cbind(seq_along(test_denorm_out_2), test_denorm_out_2, pred_denorm_2_2n)
head(series_2_2)

colnames(series_2_2) <- c("Time","Expected Output","Neural Net Output")
series_2_2 <- as.data.frame(series_2_2)
head(series_2_2)

ggplot(series_2_2, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `Neural Net Output`, colour = "Neural Net Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


######
#mse 
#####

mse_2_2 <- mse(series_2_2$`Expected Output`, series_2_2$`Neural Net Output`) 
mse_2_2
######
#rmse 
#####

rmse_2_2 <- rsme(series_2_2$`Expected Output`, series_2_2$`Neural Net Output`)  
rmse_2_2
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape_2_2 <- mape(series_2_2$`Expected Output`, series_2_2$`Neural Net Output`)  
mape_2_2


##############
## 2 input, model3
##################
usd_eur_model2_3 <- neuralnet(data_out ~ dataA + dataB, data = usd_eur_train_2, hidden = 8, threshold = 0.01)
#View model summary
print(usd_eur_model2_3)

#Plot the model to see nn graph
plot(usd_eur_model2_3, alpha = 0.6)
# plotnet
par(mar = numeric(4), family = 'serif')
plotnet(usd_eur_model2_3, alpha = 0.6)

#######
##MODEL correlation
#######
net_results_2_3 <- neuralnet::compute(usd_eur_model2_3, usd_eur_test_2[, 1:2])
pred_2_3 <- net_results_2_3$net.result
cor(pred_2_3, usd_eur_test_2$data_out)

pred_all_2_3 <- as.data.frame(net_results_2_3$net.result)
head(pred_all_2_3)
pred_denorm_2_3 <- as.data.frame(Map(denormalize, pred_all_2_3, minvec, maxvec))
pred_denorm_2_3n <- pred_denorm_2_3[, 1]
head(pred_denorm_2_3n)
length(pred_denorm_2_3n)

#plot the line graph
series_2_3 <- cbind(seq_along(test_denorm_out_2), test_denorm_out_2, pred_denorm_2_3n)
head(series_2_3)

colnames(series_2_3) <- c("Time","Expected Output","Neural Net Output")
series_2_3 <- as.data.frame(series_2_3)
head(series_2_3)

ggplot(series_2_3, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `Neural Net Output`, colour = "Neural Net Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


######
#mse 
#####

mse_2_3 <- mse(series_2_3$`Expected Output`, series_2_3$`Neural Net Output`) 
mse_2_3
######
#rmse 
#####

rmse_2_3 <- rsme(series_2_3$`Expected Output`, series_2_3$`Neural Net Output`)  
rmse_2_3
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape_2_3 <- mape(series_2_3$`Expected Output`, series_2_3$`Neural Net Output`)  
mape_2_3


##############
## 2 input, model4
##################
usd_eur_model2_4 <- neuralnet(data_out ~ dataA + dataB, data = usd_eur_train_2, hidden = c(8,6))
#View model summary
print(usd_eur_model2_4)

#Plot the model to see nn graph
plot(usd_eur_model2_4, alpha = 0.6)
# plotnet
par(mar = numeric(4), family = 'serif')
plotnet(usd_eur_model2_4, alpha = 0.6)

#######
##MODEL correlation
#######
net_results_2_4 <- neuralnet::compute(usd_eur_model2_4, usd_eur_test_2[, 1:2])
pred_2_4 <- net_results_2_4$net.result
cor(pred_2_4, usd_eur_test_2$data_out)

pred_all_2_4 <- as.data.frame(net_results_2_4$net.result)
head(pred_all_2_4)
pred_denorm_2_4 <- as.data.frame(Map(denormalize, pred_all_2_4, minvec, maxvec))
pred_denorm_2_4n <- pred_denorm_2_4[, 1]
head(pred_denorm_2_4n)
length(pred_denorm_2_4n)


#plot the line graph
series_2_4 <- cbind(seq_along(test_denorm_out_2), test_denorm_out_2, pred_denorm_2_4n)
head(series_2_4)

colnames(series_2_4) <- c("Time","Expected Output","Neural Net Output")
series_2_4 <- as.data.frame(series_2_4)
head(series_2_4)

ggplot(series_2_4, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `Neural Net Output`, colour = "Neural Net Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


######
#mse 
#####

mse_2_4 <- mse(series_2_4$`Expected Output`, series_2_4$`Neural Net Output`) 
mse_2_4
######
#rmse 
#####

rmse_2_4 <- rsme(series_2_4$`Expected Output`, series_2_4$`Neural Net Output`)  
rmse_2_4
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape_2_4 <- mape(series_2_4$`Expected Output`, series_2_4$`Neural Net Output`)  
mape_2_4




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
usd_eur_model3_1 <- neuralnet(data_out ~ dataA + dataB + dataC, data = usd_eur_train_3)
usd_eur_model3_1
#Plot the model to see nn graph
plot(usd_eur_model3_1)

head(usd_eur_test_3)
#######
##MODEL PERFORMANCE EVALUATION
#######
net_results_3_1 <- neuralnet::compute(usd_eur_model3_1, usd_eur_test_3[, 1:3])
pred_3_1 <- net_results_3_1$net.result
cor(pred_3_1, usd_eur_test_3$data_out)



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
head(test_denorm_3)
test_denorm_out_3 <- test_denorm_3$data_out
head(test_denorm_out_3)
length(test_denorm_out_3)

# ls(net_results_3_1)
# net_results_3_1$net.result
pred_all_3_1 <- as.data.frame(net_results_3_1$net.result)
head(pred_all_3_1)
pred_denorm_3_1 <- as.data.frame(Map(denormalize, pred_all_3_1, minvec, maxvec))
pred_denorm_3_1n <- pred_denorm_3_1[, 1]
head(pred_denorm_3_1n)
length(pred_denorm_3_1n)

#plot the line graph
series_3_1 <- cbind(seq_along(test_denorm_out_3), test_denorm_out_3, pred_denorm_3_1n)
head(series_3_1)

colnames(series_3_1) <- c("Time","Expected Output","Neural Net Output")
series_3_1 <- as.data.frame(series_3_1)
head(series_3_1)

ggplot(series_3_1, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `Neural Net Output`, colour = "Neural Net Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


######
#mse 
#####

mse_3_1 <- mse(series_3_1$`Expected Output`, series_3_1$`Neural Net Output`) 
mse_3_1
######
#rmse 
#####

rmse_3_1 <- rsme(series_3_1$`Expected Output`, series_3_1$`Neural Net Output`)  
rmse_3_1
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape_3_1 <- mape(series_3_1$`Expected Output`, series_3_1$`Neural Net Output`)  
mape_3_1


##################################
### 3 INPUTS, model2
#################################

#Fit a neural network model
usd_eur_model3_2 <- neuralnet(data_out ~ dataA + dataB + dataC, data = usd_eur_train_3, hidden = 8)
usd_eur_model3_2
#Plot the model to see nn graph
plot(usd_eur_model3_2)
#a <- neuralnet(data_out ~ dataA + dataB + dataC, data = usd_eur_train_3, hidden = 8)
#plot(a)
#head(usd_eur_test_3)
#######
##MODEL PERFORMANCE EVALUATION
#######
net_results_3_2 <- neuralnet::compute(usd_eur_model3_2, usd_eur_test_3[, 1:3])
pred_3_2 <- net_results_3_2$net.result
cor(pred_3_2, usd_eur_test_3$data_out)

#######
##De-normalize predicted values
#######
#Write a de-normalize fuction
#De-normalizer

# ls(net_results_3_1)
# net_results_3_1$net.result
pred_all_3_2 <- as.data.frame(net_results_3_2$net.result)
head(pred_all_3_2)
pred_denorm_3_2 <- as.data.frame(Map(denormalize, pred_all_3_2, minvec, maxvec))
pred_denorm_3_2n <- pred_denorm_3_2[, 1]
head(pred_denorm_3_2n)
length(pred_denorm_3_2n)

#plot the line graph
series_3_2 <- cbind(seq_along(test_denorm_out_3), test_denorm_out_3, pred_denorm_3_2n)
head(series_3_2)

colnames(series_3_2) <- c("Time","Expected Output","Neural Net Output")
series_3_2 <- as.data.frame(series_3_2)
head(series_3_2)

ggplot(series_3_2, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `Neural Net Output`, colour = "Neural Net Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


######
#mse 
#####

mse_3_2 <- mse(series_3_2$`Expected Output`, series_3_2$`Neural Net Output`) 
mse_3_2
######
#rmse 
#####

rmse_3_2 <- rsme(series_3_2$`Expected Output`, series_3_2$`Neural Net Output`)  
rmse_3_2
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape_3_2 <- mape(series_3_2$`Expected Output`, series_3_2$`Neural Net Output`)  
mape_3_2


##################################
### 3 INPUTS, model3
#################################

#Fit a neural network model
usd_eur_model3_3 <- neuralnet(data_out ~ dataA + dataB + dataC, data = usd_eur_train_3, hidden = 8, threshold = 0.01)
usd_eur_model3_3
#Plot the model to see nn graph
plot(usd_eur_model3_3)

#head(usd_eur_test_3)
#######
##MODEL PERFORMANCE EVALUATION
#######
net_results_3_3 <- neuralnet::compute(usd_eur_model3_3, usd_eur_test_3[, 1:3])
pred_3_3 <- net_results_3_3$net.result
cor(pred_3_3, usd_eur_test_3$data_out)

#######
##De-normalize predicted values
#######
#Write a de-normalize fuction
#De-normalizer

# ls(net_results_3_1)
# net_results_3_1$net.result
pred_all_3_3 <- as.data.frame(net_results_3_3$net.result)
head(pred_all_3_3)
pred_denorm_3_3 <- as.data.frame(Map(denormalize, pred_all_3_3, minvec, maxvec))
pred_denorm_3_3n <- pred_denorm_3_3[, 1]
head(pred_denorm_3_3n)
length(pred_denorm_3_3n)

#plot the line graph
series_3_3 <- cbind(seq_along(test_denorm_out_3), test_denorm_out_3, pred_denorm_3_3n)
head(series_3_3)

colnames(series_3_3) <- c("Time","Expected Output","Neural Net Output")
series_3_3 <- as.data.frame(series_3_3)
head(series_3_3)

ggplot(series_3_3, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `Neural Net Output`, colour = "Neural Net Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


######
#mse 
#####

mse_3_3 <- mse(series_3_3$`Expected Output`, series_3_3$`Neural Net Output`) 
mse_3_3
######
#rmse 
#####

rmse_3_3 <- rsme(series_3_3$`Expected Output`, series_3_3$`Neural Net Output`)  
rmse_3_3
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape_3_3 <- mape(series_3_3$`Expected Output`, series_3_3$`Neural Net Output`)  
mape_3_3


##################################
### 3 INPUTS, model4
#################################

#Fit a neural network model
usd_eur_model3_4 <- neuralnet(data_out ~ dataA + dataB + dataC, data = usd_eur_train_3, hidden = c(8, 6))
usd_eur_model3_4
#Plot the model to see nn graph
plot(usd_eur_model3_4)

#head(usd_eur_test_3)
#######
##MODEL PERFORMANCE EVALUATION
#######
net_results_3_4 <- neuralnet::compute(usd_eur_model3_4, usd_eur_test_3[, 1:3])
pred_3_4 <- net_results_3_4$net.result
cor(pred_3_4, usd_eur_test_3$data_out)

#######
##De-normalize predicted values
#######
#Write a de-normalize fuction
#De-normalizer

# ls(net_results_3_1)
# net_results_3_1$net.result
pred_all_3_4 <- as.data.frame(net_results_3_4$net.result)
head(pred_all_3_4)
pred_denorm_3_4 <- as.data.frame(Map(denormalize, pred_all_3_4, minvec, maxvec))
pred_denorm_3_4n <- pred_denorm_3_4[, 1]
head(pred_denorm_3_4n)
length(pred_denorm_3_4n)

#plot the line graph
series_3_4 <- cbind(seq_along(test_denorm_out_3), test_denorm_out_3, pred_denorm_3_4n)
head(series_3_4)

colnames(series_3_4) <- c("Time","Expected Output","Neural Net Output")
series_3_4 <- as.data.frame(series_3_4)
head(series_3_4)

ggplot(series_3_4, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `Neural Net Output`, colour = "Neural Net Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


######
#mse 
#####

mse_3_4 <- mse(series_3_4$`Expected Output`, series_3_4$`Neural Net Output`) 
mse_3_4
######
#rmse 
#####

rmse_3_4 <- rsme(series_3_4$`Expected Output`, series_3_4$`Neural Net Output`)  
rmse_3_4
#######
#Mean Percentage Absolute Error
#train_output = actual
#pred = predicted 
#########

mape_3_4 <- mape(series_3_4$`Expected Output`, series_3_4$`Neural Net Output`)  
mape_3_4


#######################
## Error collation
######################
error_2_1 <- c(mse_2_1, rmse_2_1, mape_2_1)
error_2_2 <- c(mse_2_2, rmse_2_2, mape_2_2)
error_2_3 <- c(mse_2_3, rmse_2_3, mape_2_3)
error_2_4 <- c(mse_2_4, rmse_2_4, mape_2_4)

error_3_1 <- c(mse_3_1, rmse_3_1, mape_3_1)
error_3_2 <- c(mse_3_2, rmse_3_2, mape_3_2)
error_3_3 <- c(mse_3_3, rmse_3_3, mape_3_3)
error_3_4 <- c(mse_3_4, rmse_3_4, mape_3_4)

error_nn <- cbind(error_2_1, error_2_2, error_2_3, error_2_4, error_3_1, error_3_2, error_3_3, error_3_4)

colnames(error_nn) <- c("model1_2", "model2_2", "model3_2", "model4_2",
                        "model1_3", "model2_3", "model3_3", "model4_3")

error_nn <-  as.data.frame(error_nn)
error_nn
##visualize the best model
ggplot(series_3_1, aes(Time)) + 
  geom_line(aes(y = `Expected Output`, colour = "Expected Output")) + 
  geom_line(aes(y = `Neural Net Output`, colour = "Neural Net Output"))+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

cor_all <- rbind(cor(pred_2_1, usd_eur_test_2$data_out),
      cor(pred_2_2, usd_eur_test_2$data_out),
      cor(pred_2_3, usd_eur_test_2$data_out),
      cor(pred_2_4, usd_eur_test_2$data_out),
      cor(pred_3_1, usd_eur_test_3$data_out),
      cor(pred_3_2, usd_eur_test_3$data_out),
      cor(pred_3_3, usd_eur_test_3$data_out),
      cor(pred_3_4, usd_eur_test_3$data_out))

row.names(cor_all) <- c("cor_model1_2", "cor_model2_2", "cor_model3_2", "cor_model4_2",
          "cor_model1_3", "cor_model2_3", "cor_model1_3", "cor_model1_3")

cor_all

