ncol(data_train)
nrow(data_train)
x_train <- data_train[, 7:ncol(data_train)]
y_train <- data_train[, 5]
y_train[1:10]
y_train <- to_categorical(y_train)
dim(x_train)


x_test <- data_test[, 7:ncol(data_test)]
y_test <- data_test[, 5]
y_test[1:10]
y_test <- to_categorical(y_test)
dim(x_test)

# 
# model <- keras_model_sequential() 
# model %>% 
#   layer_dense(units = 20, activation = 'relu', input_shape = c(4)) %>% 
#   layer_dropout(rate = 0.4) %>% 
#   layer_dense(units = 6, activation = 'sigmoid') %>%
#   layer_dropout(rate = 0.3) %>%
#   layer_dense(units = 3, activation = 'softmax')
# 
# summary(model)

ncol(data_train) - 6  #872 columns
model_5 <- keras_model_sequential() 
model_5 %>% 
  layer_dense(units = 150, activation = 'relu', input_shape = c(872)) %>% 
  layer_dense(units = 2, activation = 'softmax')

summary(model_5)

model_5 %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model_5 %>% fit(
  x_train, y_train, 
  epochs = 130, batch_size = 32, 
  validation_split = 0.2
)

model_5 %>% evaluate(x_test, y_test)

pred_5 <- model_5 %>% predict_classes(x_test)
pred_proba_5 <- model_5 %>% predict_proba(x_test)
table(Predicted = pred_5, Actual = data_test[, 5])
confusionMatrix(pred_5, data_test[, 5])

table_5 <- cbind(pred_proba_5, Predicted = pred_5, Actual = data_test[, 5])
head(table_5)

insult_true <- table_5[, 2]
head(insult_true)

pred_proba_5_test <- model_5 %>% predict_proba(for_test)
insult_true_test <- pred_proba_5_test[, 2]
head(insult_true_test)