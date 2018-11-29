ncol(data_train)
nrow(data_train)
x_train <- data_train[, 7:ncol(data_train)]
y_train <- data_train[, 2]
y_train[1:10]
y_train <- to_categorical(y_train)
dim(x_train)


x_test <- data_test[, 7:ncol(data_test)]
y_test <- data_test[, 2]
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
model_2 <- keras_model_sequential() 
model_2 %>% 
  layer_dense(units = 150, activation = 'relu', input_shape = c(872)) %>% 
  layer_dense(units = 2, activation = 'softmax')

summary(model_2)

model_2 %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model_2 %>% fit(
  x_train, y_train, 
  epochs = 130, batch_size = 32, 
  validation_split = 0.2
)

model_2 %>% evaluate(x_test, y_test)

pred_2 <- model_2 %>% predict_classes(x_test)
pred_proba_2 <- model_2 %>% predict_proba(x_test)
table(Predicted = pred_2, Actual = data_test[, 2])
confusionMatrix(pred_2, data_test[, 2])

table_2 <- cbind(pred_proba_2, Predicted = pred_2, Actual = data_test[, 2])
head(table_2)

severe_toxic_true <- table_2[, 2]
head(severe_toxic_true)
