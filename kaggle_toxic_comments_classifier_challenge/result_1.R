#toxic severe_toxic obscene threat insult identity_hate
result_1_1 <- as.data.frame(cbind(toxic_true_test, severe_toxic_true_test, 
                                  obscene_true_test, threat_true_test,
                                  insult_true_test, identity_hate_true_test))


result_1_1$sum <- result_1_1[, 1] + result_1_1[, 2] + result_1_1[, 3] + result_1_1[, 4] +
                   result_1_1[, 5] + result_1_1[, 6]


result_1_1$toxic <- result_1_1[, 1]/result_1_1$sum
result_1_1$severe_toxic <- result_1_1[, 2]/result_1_1$sum
result_1_1$obscene <- result_1_1[, 3]/result_1_1$sum
result_1_1$threat <- result_1_1[, 4]/result_1_1$sum
result_1_1$insult <- result_1_1[, 5]/result_1_1$sum
result_1_1$identity_hate <- result_1_1[, 6]/result_1_1$sum


result_1 <- result_1_1[, 8:13]
nrow(result_1)
nrow(test_id)
class(test_id)
head(test_id)
head(result_1)
result_1 <- cbind(test_id, result_1)
#result_1_keep <- result_1
#rownames(result_1) <- NULL
head(result_1)
write.csv(result_1, file = "submission_1.csv", row.names = FALSE)


