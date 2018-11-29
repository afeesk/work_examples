# install.packages("RCurl")
# install.packages("RJSONIO")
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("twitteR")
# install.packages("ROAuth")
# install.packages("tm")
# install.packages("wordcloud")
# install.packages("topicmodels")

# require(devtools)

library(plyr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tm)
library(twitteR)
library(ROAuth)
library(topicmodels)



tweet <- read.csv("USA_TWEETS.csv", encoding="UTF-8", header = T, sep = "\t")
head(tweet)



#############
#####  1  ###
#############
###word cloud to represent the most commonly used words in the dataset 

# build a corpus, and specify the source to be character vectors 
myCorpus <- Corpus(VectorSource(tweet$text)) 

# remove URLs 
removeURL <- function(x) gsub("http[^[:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL)) 

# remove anything other than English letters or space 
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct)) 

# convert to lower case 
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

# remove stopwords 
myCorpus <- tm_map(myCorpus, removeWords, stopwords('english')) 

# remove extra whitespace 
myCorpus <- tm_map(myCorpus, stripWhitespace)

# keep a copy for stem completion later 
myCorpusCopy <- myCorpus
######

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf))) 
tdm

tdm <- removeSparseTerms(tdm, 0.99) 
m <- as.matrix(tdm) 
# calculate the frequency of words and sort it by frequency 
word.freq <- sort(rowSums(m), decreasing = T) 
# colors 
pal <- brewer.pal(9, "BuGn")[-(1:4)]

# plot word cloud 
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)



############
#####  2 ###
############
##Plot hourly tweet
#check if any column has missing value
colSums(is.na(tweet))
#extract time hours and add to dataframe
tweet$hour <- tweet$standard_date$hour

#standardizing the date
tweet$standard_date <- strptime(tweet$date, "%a %b %d %H:%M:%S %z %Y", tz = "")
#check
head(tweet$standard_date)

##head(tweet)

#check
# head(tweet$hour)
head(tweet)
hourlyTweet <- table(tweet$hour)
#plot bar chart of houly tweets
barplot(hourlyTweet, main = "Number of tweets posted per hour") 



############
#####  3 ###
############
# The top 10 most active US twitter users
#create frequency table
tweeterUsersTable <- table(tweet$username)
#convert the table to dataframe
tweeterUsersDF <- as.data.frame(tweeterUsersTable)
#check
head(tweeterUsersDF)
#rename Var1 to username and Freq to count
tweeterUsersDFr <- rename(tweeterUsersDF, username = Var1, count = Freq)
#check
head(tweeterUsersDFr)
#sum all indivual counts
tweeterUsers <- ddply(tweeterUsersDFr, "username", summarize, total = sum(count))
#check
head(tweeterUsers)

#sort the result in descending order
tweeterUsers_sort <- arrange(tweeterUsers, desc(total))
#check
head(tweeterUsers_sort)

#visualize the top 10 tweet by each US state
top10_tweeterUsers <- tweeterUsers_sort[c(1:10), ] 
top10_tweeterUsers
#bar plot
windows(8,8) #To plot in a new window
ggplot(top10_tweeterUsers, aes(x = reorder(username, -total), y = total)) + 
  geom_bar(stat = "identity")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs( x = "username", y = "Total", main = "Top 10 tweets by twitter user") 


############
#####  4 ###
############
# The total number of tweets posted by each US state
head(tweet$place)
#create frequency table
tweetByUSstateTable <- table(tweet$place)
#convert the table to dataframe
tweetByUSstateDF <- as.data.frame(tweetByUSstateTable)
#check
head(tweetByUSstateDF)
#rename Var1 to state and Freq to count
tweetByUSstateDFr <- rename(tweetByUSstateDF, state = Var1, count = Freq)
#check
head(tweetByUSstateDFr)
#sum all indivual counts
tweetByUSstate <- ddply(tweetByUSstateDFr, "state", summarize, total = sum(count))
#check
head(tweetByUSstate)

#sort the result in descending order
tweetByUSstate_sort <- arrange(tweetByUSstate, desc(total))
#check
head(tweetByUSstate_sort)

#visualize the top 5 tweet by each US state
top5_tweetByUSstate <- tweetByUSstate_sort[c(1:5), ] 
top5_tweetByUSstate
#bar plot
ggplot(top5_tweetByUSstate, aes(x = reorder(state, -total), y = total)) + 
  geom_bar(stat = "identity")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs( x = "US state", y = "Total", main = "Top 5 tweets by US state") 



#############
#####  5  ###
#############
 
 ### Develop a topic model to identify several relevant items of news for Monday’s publication.  

#Extract day name
tweet$day_name <- format(tweet$standard_date, '%A')
#check
head(tweet$day_name)
head(tweet) 

 #preprocessing
 monday_tweet <- subset(tweet, day_name=='Monday', select=c(date, standard_date, text, day_name))
 head(monday_tweet)
 
 # build a corpus, and specify the source to be character vectors 
 monday_myCorpus <- Corpus(VectorSource(monday_tweet$text)) 
 
 # remove URLs 
 removeURL <- function(x) gsub("http[^[:space:]]*", "", x) 
 monday_myCorpus <- tm_map(monday_myCorpus, content_transformer(removeURL)) 
 
 # remove anything other than English letters or space 
 removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
 monday_myCorpus <- tm_map(monday_myCorpus, content_transformer(removeNumPunct)) 
 
 # convert to lower case 
 monday_myCorpus <- tm_map(monday_myCorpus, content_transformer(tolower))
 
 # remove stopwords 
 monday_myCorpus <- tm_map(monday_myCorpus, removeWords, stopwords('english')) 
 
 # remove extra whitespace 
 monday_myCorpus <- tm_map(monday_myCorpus, stripWhitespace)
 
 # keep a copy for stem completion later 
 monday_myCorpusCopy <- monday_myCorpus
 ######
 
 dtm_monday <- DocumentTermMatrix(monday_myCorpus, control = list(wordLengths = c(1, Inf))) 
 dtm_monday
 dtm_monday <- removeSparseTerms(dtm_monday, 0.99) 
 
 mx_monday <- as.matrix(dtm_monday) 
 mx_monday <- mx_monday[which(rowSums(mx_monday) > 0),]  
 rownames(mx_monday) <- 1:nrow(mx_monday) 
 
 ##Topic Modelling
 lda <- LDA(mx_monday, k = 10) # find 10 topics 
  
 terms(lda, 8) # first 8 terms of the 10 topic 
 perplexity(lda)
 

 
 ################################################################
 
 # get hastags for monday tweet
 
 ht = grep("^#[a-zA-Z0-9]+$", monday_tweet$text,ignore.case = TRUE, value = T)
 tail(ht)
 
 hashtags <- Corpus(VectorSource(ht)) 
 # remove extra whitespace 
 hashtags <- tm_map(hashtags, stripWhitespace)
 tdm_ht <- TermDocumentMatrix(hashtags, control = list(wordLengths = c(1, Inf))) 
 tdm_ht
 
 # inspect frequent words 
 (freq.terms <- findFreqTerms(tdm_ht, lowfreq = 20))
 term.freq <- rowSums(as.matrix(tdm_ht)) 
 term.freq <- subset(term.freq, term.freq >= 20) 
 df <- data.frame(term = names(term.freq), freq = term.freq)
 
 library(ggplot2) ggplot(df, aes(x=term, y=freq)) + 
   geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + 
   coord_flip() + theme(axis.text=element_text(size=7))
 
 
 


 
 

 ########################################################
 