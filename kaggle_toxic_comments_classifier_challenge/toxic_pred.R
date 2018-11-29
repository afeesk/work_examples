ncol(data_train)
nrow(data_train)
x_train <- data_train[, 7:ncol(data_train)]
y_train <- data_train[, 1]
y_train[1:10]
y_train <- to_categorical(y_train)
dim(x_train)


x_test <- data_test[, 7:ncol(data_test)]
y_test <- data_test[, 1]
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
model_1 <- keras_model_sequential() 
model_1 %>% 
  layer_dense(units = 150, activation = 'relu', input_shape = c(872)) %>% 
  layer_dense(units = 2, activation = 'softmax')

summary(model_1)

model_1 %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model_1 %>% fit(
  x_train, y_train, 
  epochs = 130, batch_size = 32, 
  validation_split = 0.2
)

model_1 %>% evaluate(x_test, y_test)

pred_1 <- model_1 %>% predict_classes(x_test)
pred_proba_1 <- model_1 %>% predict_proba(x_test)
table(Predicted = pred_1, Actual = data_test[, 1])
confusionMatrix(pred_1, data_test[, 1])

table_1 <- cbind(pred_proba_1, Predicted = pred_1, Actual = data_test[, 1])
head(table_1)

toxic_true <- table_1[, 2]
head(toxic_true)

pred_proba_1_test <- model_1 %>% predict_proba(for_test)
toxic_true_test <- pred_proba_1_test[, 2]
head(toxic_true_test)
