ncol(data_train)
nrow(data_train)
x_train <- data_train[, 7:ncol(data_train)]
y_train <- data_train[, 6]
y_train[1:10]
y_train <- to_categorical(y_train)
dim(x_train)


x_test <- data_test[, 7:ncol(data_test)]
y_test <- data_test[, 6]
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
model_6 <- keras_model_sequential() 
model_6 %>% 
  layer_dense(units = 150, activation = 'relu', input_shape = c(872)) %>% 
  layer_dense(units = 2, activation = 'softmax')

summary(model_6)

model_6 %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model_6 %>% fit(
  x_train, y_train, 
  epochs = 130, batch_size = 32, 
  validation_split = 0.2
)

model_6 %>% evaluate(x_test, y_test)

pred_6 <- model_6 %>% predict_classes(x_test)
pred_proba_6 <- model_6 %>% predict_proba(x_test)
table(Predicted = pred_6, Actual = data_test[, 6])
confusionMatrix(pred_6, data_test[, 6])

table_6 <- cbind(pred_proba_6, Predicted = pred_6, Actual = data_test[, 6])
head(table_6)

identity_hate_true <- table_6[, 2]
head(identity_hate_true)

pred_proba_6_test <- model_6 %>% predict_proba(for_test)
identity_hate_true_test <- pred_proba_6_test[, 2]
head(identity_hate_true_test)