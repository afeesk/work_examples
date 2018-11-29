ncol(data_train)
nrow(data_train)
x_train <- data_train[, 7:ncol(data_train)]
y_train <- data_train[, 3]
y_train[1:10]
y_train <- to_categorical(y_train)
dim(x_train)


x_test <- data_test[, 7:ncol(data_test)]
y_test <- data_test[, 3]
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
model_3 <- keras_model_sequential() 
model_3 %>% 
  layer_dense(units = 150, activation = 'relu', input_shape = c(872)) %>% 
  layer_dense(units = 2, activation = 'softmax')

summary(model_3)

model_3 %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model_3 %>% fit(
  x_train, y_train, 
  epochs = 130, batch_size = 32, 
  validation_split = 0.2
)

model_3 %>% evaluate(x_test, y_test)

pred_3 <- model_3 %>% predict_classes(x_test)
pred_proba_3 <- model_3 %>% predict_proba(x_test)
table(Predicted = pred_3, Actual = data_test[, 3])
confusionMatrix(pred_3, data_test[, 3])

table_3 <- cbind(pred_proba_3, Predicted = pred_3, Actual = data_test[, 3])
head(table_3)

obscene_true <- table_3[, 2]
head(obscene_true)

pred_proba_3_test <- model_3 %>% predict_proba(for_test)
obscene_true_test <- pred_proba_3_test[, 2]
head(obscene_true_test)