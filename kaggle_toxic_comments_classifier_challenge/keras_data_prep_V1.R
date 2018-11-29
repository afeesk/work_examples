#c("toxic", "severe_toxic", "obscene", "threat", "insult", "identity_hate")
#install.packages("neuralnet")
##keras
#install.packages("keras")
library(keras)
#install_keras()
########################################################
library(tidyverse)
library(tm)
library(SnowballC)
library(keras)
library(caret)

train_data <- read_csv("train.csv")
dim(train_data)
str(train_data)
colnames(train_data)

test_data <- read_csv("test.csv")
dim(test_data)
str(test_data)
colnames(test_data)
test_id <- test_data[, 1]

#store classes for training set
train_classes <- train_data[, 3:8]
head(train_classes)

c_data <- rbind(train_data[, 1:2], test_data)

#clean up to gain more memory
rm(train_data, test_data)

dim(c_data)
str(c_data)


# build a corpus, and specify the source to be character vectors 
myCorpus <- VCorpus(VectorSource(c_data$comment_text))
# remove anything other than English letters or space 
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct)) 
# convert to lower case 
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove stopwords 
myCorpus <- tm_map(myCorpus, removeWords, stopwords('english')) 

# remove extra whitespace 
myCorpus <- tm_map(myCorpus, stripWhitespace)

#stem document
myCorpus   <- tm_map(myCorpus, stemDocument)

# keep a copy for stem completion later 
#myCorpusCopy <- myCorpus

######
ls(myCorpus)
inspect(myCorpus)
# Tokenize bigrams 


# Create term document matrix with tf-idf weighting
tdm_2 <- TermDocumentMatrix(myCorpus, control = list( weighting = weightTfIdf))
tdm_2
rm(myCorpus, removeNumPunct, c_data)
#write.table(tdm_2$dimnames$Terms, "term_1_100.txt")
#tdm_2_keep <- tdm_2


##Trying different sparcity removal
tdm_2_n <- removeSparseTerms(tdm_2, 0.995) #memory issue, consider memory issues 
tdm_2_n

#free memory
rm(tdm_2)

#tdm_2_n <- removeSparseTerms(tdm_2, 0.99) 
#tdm_2_n
# tdm_2_n_2 <- removeSparseTerms(tdm_2, 0.993)
# 
# tdm_2_n_3 <- removeSparseTerms(tdm_2, 0.995)
# 
# tdm_2_n_4 <- removeSparseTerms(tdm_2, 0.997)
# 
# tdm_2_n_5 <- removeSparseTerms(tdm_2, 0.998)
# tdm_2_n_5$dimnames

##dtm
dtm_2_n <- tm::as.DocumentTermMatrix(tdm_2_n)
dtm_2_n
#free memory
rm(tdm_2_n)
#write.table(dtm_2_n$dimnames$Terms, "term_1_0.99.txt")

# dtm_2_n_2 <- tm::as.DocumentTermMatrix(tdm_2_n_2)
# write.table(dtm_2_n_2$dimnames$Terms, "term_1_0.993.txt")
# 
# dtm_2_n_3 <- tm::as.DocumentTermMatrix(tdm_2_n_3)
# write.table(dtm_2_n_3$dimnames$Terms, "term_1_0.995.txt")
# 
# dtm_2_n_4 <- tm::as.DocumentTermMatrix(tdm_2_n_4)
# write.table(dtm_2_n_4$dimnames$Terms, "term_1_0.997.txt")
# 
# dtm_2_n_5 <- tm::as.DocumentTermMatrix(tdm_2_n_5)
# write.table(dtm_2_n_5$dimnames$Terms, "term_1_0.998.txt")


#convert to matrix and later to dataframe: data frame will be usefule for our model building
m_1 <- as.matrix(dtm_2_n)
# m_2 <- as.matrix(dtm_2_n_2)
# #m_3 <- as.matrix(dtm_2_n_3)
# #m_4 <- as.matrix(dtm_2_n_4)
# #m_5 <- as.matrix(tdm_2_n_5)


###TRAIN AND TEST SET
for_train <- m_1[1:159571, ]
for_train <- cbind(train_classes, for_train)
for_train <- as.matrix(for_train)
dimnames(for_train) <- NULL
str(for_train)

for_test <- m_1[159572:312735, ]
dimnames(for_test) <- NULL
for_train[1:10, 1:5]
for_test[1:10, 1:5]
str(for_test)
#PREPARE DATA FOR KERAS
##free some meory
rm(dtm_2_n, m_1)

#set seed
set.seed(3211)
# define an 75%/25% train/test split of the dataset
split=0.75
trainIndex <- createDataPartition(for_train[, 1], p=split, list=FALSE)
data_train <- for_train[ trainIndex,]
data_test <- for_train[-trainIndex,]
