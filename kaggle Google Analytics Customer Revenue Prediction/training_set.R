library(tidyverse)
library(doSNOW) 
library(foreach)
library(rlist)
library(DataExplorer)
library(caret)
library(kernlab)


library(rpart)
library(gbm)

library(xgboost)
customer_rev <- read_csv("train_v2.csv")
#View(customer_rev)
length(unique(customer_rev$date))
max(customer_rev$date)
min(customer_rev$date)
##Parallel program to flatten JSON column
jsontodf <- function(col) {
  cores <- as.numeric(Sys.getenv('NUMBER_OF_PROCESSORS'))
  cl<-makeCluster(cores)
  registerDoSNOW(cl)
  nr <- length(col)
  chunk <- floor(nr/cores)
  s <- rep(1:cores, each = chunk)
  s <- c(s, rep(cores,nr-length(s)))
  for(i in 1:cores) {
    col.core <- col[s==i]
    clusterExport(cl[i], 'col.core', envir = environment())
  }
  col.df <- foreach(i=1:cores, .combine = 'bind_rows', .noexport = 'col.core', .packages = c('jsonlite', 'rlist', 'magrittr')) %dopar% {
    list.stack(lapply(col.core, function(j){
      as.list(unlist(fromJSON(j)))}) , fill=TRUE)
  }
  stopCluster(cl)
  return(as.data.frame(col.df))
}

###
#All JSON coded variabes are: device, geoNetwork, totals, trafficSource
###
#Flaten "device" JSON 
device_f <- jsontodf(customer_rev$device)
#dim(device_f)

#Flaten "geoNetwork" JSON
geoNetwork_f <- jsontodf(customer_rev$geoNetwork)
#dim(geoNetwork_f)

#Flaten "totals" JSON
totals_f <- jsontodf(customer_rev$totals)
#dim(totals_f)
#head(totals_f)

#Flaten "trafficSource" JSON
trafficSource_f <- jsontodf(customer_rev$trafficSource)
#dim(trafficSource_f)

#########################
##Remove vaiables stage 1
##########################
#To be removed from customer_rev
customer_rev$date <- lubridate::ymd(customer_rev$date)
customer_rev <- customer_rev[ ,c(-3,-5,-8,-9)]

#BIND ALL DATASET TOGETHER
customer_train <- as.data.frame(cbind(customer_rev, device_f, geoNetwork_f, totals_f, trafficSource_f))
rm(customer_rev, geoNetwork_f, device_f, trafficSource_f, totals_f)
#REMOVE IDs
#fullVisitorId, sessionId, visitId
cols_1 <- c("fullVisitorId", "sessionId", "visitId")
customer_train_clean_1 <- customer_train %>% select(-one_of(cols_1))
#names(customer_train_clean_1)
rm(customer_train)

#REMOVE (ONLY ONE VALUE THROUGHOUT THE DATASET)
cols_2 <- c("socialEngagementType","browserVersion","browserSize","operatingSystemVersion",
            "mobileDeviceBranding","mobileDeviceModel","mobileInputSelector","mobileDeviceInfo",
            "mobileDeviceMarketingName","flashVersion","language","screenColors","screenResolution",
            "cityId","latitude","longitude","networkLocation","adwordsClickInfo.criteriaParameters",
            "visits")
customer_train_clean_1 <- customer_train_clean_1 %>% select(-one_of(cols_2))
#names(customer_train_clean_1)

###############
## REVISIT THIS PART WHEN MORE INFO
###############
#REMOVE (USELESS IF THERE IS NO STOP TIME)?
#visitStartTime

#I don’t think so, some values are not missing: It may worth  trying to remove them anyway.
#Remove seems okay when checked only transaction revenue
#"campaignCode (all missing except 1)

#adContent, adwordsClickInfo.isVideoAd, adwordsClickInfo.adNetworkType, 
#adwordsClickInfo.slot, adwordsClickInfo.page, adwordsClickInfo.gclId ##(All have missing values wherever there is a transaction)

cols_3 <- c("visitStartTime", "campaignCode", "adContent", "adwordsClickInfo.isVideoAd", 
            "adwordsClickInfo.adNetworkType", "adwordsClickInfo.slot", 
            "adwordsClickInfo.page", "adwordsClickInfo.gclId")

customer_train_clean_1 <- customer_train_clean_1 %>% select(-one_of(cols_3))
#names(customer_train_clean_1)
#dim(customer_train_clean_1)

#keep
customer_train_clean_2 <- customer_train_clean_1
#if  pageview is.na(pageviews), then it is zero view
customer_train_clean_2$pageviews[is.na(customer_train_clean_2$pageviews)] <- 0
#table(customer_train_clean_2$pageviews)
rm(customer_train_clean_1)
#if bounces == 1, "YES", ELSE "NO"
customer_train_clean_2$bounces[!is.na(customer_train_clean_2$bounces)] <- "YES"
#unique(customer_train_clean_2$bounces)
customer_train_clean_2$bounces[is.na(customer_train_clean_2$bounces)] <- "NO"
#unique(customer_train_clean_2$bounces)


#if newVisit == 1, "NEW", "NOT NEW"
customer_train_clean_2$newVisits[!is.na(customer_train_clean_2$newVisits)] <- "NEW"
#unique(customer_train_clean_2$newVisits)
customer_train_clean_2$newVisits[is.na(customer_train_clean_2$newVisits)] <- "NOT NEW"
#unique(customer_train_clean_2$newVisits)

#TO BE REMOVED FOR NOW (OTHERWISE NEEDS EXPERT REVIEW)
#unique(missing_variables$keyword) #if keyword== "(not provided)", "NO", ELSE "YES"
#unique(missing_variables$isTrueDirect) #TRUE AND NA? REMOVE?
#unique(missing_variables$referralPath) #YES, NA? REMOVE?

cols_4 <- c("keyword", "isTrueDirect", "referralPath")
customer_train_clean_2 <- customer_train_clean_2 %>% select(-one_of(cols_4))
#names(customer_train_clean_2)
#dim(customer_train_clean_2)


#	Extract features from date (Year, Month, Weekday, etc.) 
#year
customer_train_clean_2$year <- lubridate::year(customer_train_clean_2$date)

#Month
customer_train_clean_2$month <- lubridate::month(customer_train_clean_2$date, label = T, abbr = T)
#names(customer_train_clean_2)

#Weekday
customer_train_clean_2$weekday <- lubridate::wday(customer_train_clean_2$date, label = T, abbr = T)
#names(customer_train_clean_2)

anyNA(customer_train_clean_2)
#plot_missing(customer_train_clean_2)

#REPLACE NA WITH ZERO IN TRANSACTION REVENUE
customer_train_clean_2$transactionRevenue[is.na(customer_train_clean_2$transactionRevenue)] <- 0
anyNA(customer_train_clean_2)

##REMOVE DATE
#names(customer_train_clean_2)
customer_train_clean_2 <- customer_train_clean_2[, -2]
#names(customer_train_clean_2)
#write.csv(customer_train_clean_2, file = "checker.csv")

##CHECK VARIABLE IMPORTANCE USING RANDOM FOREST
#str(customer_train_clean_2)
#unique(customer_train_clean_2$pageviews)
#Integers but am going to code it numeric because of scaling and or normalization
customer_train_clean_2$visitNumber <- as.numeric(customer_train_clean_2$visitNumber)
customer_train_clean_2$hits <- as.numeric(customer_train_clean_2$hits)
customer_train_clean_2$pageviews <- as.numeric(customer_train_clean_2$pageviews)
#numeric
customer_train_clean_2$transactionRevenue <- as.numeric(customer_train_clean_2$transactionRevenue)

cols_c <- c("channelGrouping", "browser", "operatingSystem", "isMobile",
            "deviceCategory", "continent", "subContinent", "country",
            "region", "metro", "city", "networkDomain", "bounces", "newVisits",
            "campaign", "source","medium", "year" )


customer_train_clean_2[cols_c] <- lapply(customer_train_clean_2[cols_c], factor)
str(customer_train_clean_2)
rm(cols_1,cols_2,cols_3,cols_4,cols_c)

#REDUCING SOME HIGHER DIMENTIONALITY AND NON LINEAR SIGNIFICANT
#THIS CAN BE REVIEWD LATER
cols_n <- c("country","region","metro","city","networkDomain","campaign","source","year","month")
customer_train_clean_2 <- customer_train_clean_2 %>% select(-one_of(cols_n))
#names(customer_train_clean_2)
#str(customer_train_clean_2)
rm(cols_n)

##SELECT VARIABLE RECOMMENDED BY BACKWARD STEPWISE REGRESSION
##Result from backward stepwise regression
#transactionRevenue ~ channelGrouping + visitNumber + operatingSystem + 
#  hits + pageviews + bounces + newVisits + weekday
# Remove operatingSystem also
customer_train_clean_3 <- customer_train_clean_2 %>% 
  select("transactionRevenue", "channelGrouping", "visitNumber",
         "hits", "pageviews", "bounces", "newVisits", "weekday")

str(customer_train_clean_3)

########
### VISUALIZE
#######
str(customer_train_clean_3)
barplot(table(customer_train_clean_3$channelGrouping))
barplot(table(customer_train_clean_3$bounces))
barplot(table(customer_train_clean_3$newVisits))
barplot(table(customer_train_clean_3$weekday))

############
## FEETURE EXTRACTION? on customer_train_clean_3
####################
#channelGrouping
channel_group <- data.frame(Reduce(cbind, 
                                   lapply(levels(customer_train_clean_3$channelGrouping), 
                                          function(x){(customer_train_clean_3$channelGrouping == x) * 1})))

names(channel_group) <- levels(customer_train_clean_3$channelGrouping)
str(channel_group)
customer_train_clean_3 <- cbind(customer_train_clean_3, channel_group)
customer_train_clean_3 <- subset(customer_train_clean_3, select = -channelGrouping)
str(customer_train_clean_3)


#bounces
bounces_f <- data.frame(Reduce(cbind, 
                               lapply(levels(customer_train_clean_3$bounces), 
                                      function(x){(customer_train_clean_3$bounces == x) * 1})))

names(bounces_f) <- levels(customer_train_clean_3$bounces)
str(bounces_f)
customer_train_clean_3 <- cbind(customer_train_clean_3, bounces_f)
customer_train_clean_3 <- subset(customer_train_clean_3, select = -bounces)
str(customer_train_clean_3)

#newVisits
new_visits <- data.frame(Reduce(cbind, 
                                lapply(levels(customer_train_clean_3$newVisits), 
                                       function(x){(customer_train_clean_3$newVisits == x) * 1})))

names(new_visits) <- levels(customer_train_clean_3$newVisits)
str(new_visits)
customer_train_clean_3 <- cbind(customer_train_clean_3, new_visits)
customer_train_clean_3 <- subset(customer_train_clean_3, select = -newVisits)
str(customer_train_clean_3)

#weekday
week_day <- data.frame(Reduce(cbind, 
                              lapply(levels(customer_train_clean_3$weekday), 
                                     function(x){(customer_train_clean_3$weekday == x) * 1})))

names(week_day) <- levels(customer_train_clean_3$weekday)
str(week_day)
customer_train_clean_3 <- cbind(customer_train_clean_3, week_day)
customer_train_clean_3 <- subset(customer_train_clean_3, select = -weekday)
str(customer_train_clean_3)

rm(bounces_f, channel_group, new_visits, week_day)

#Rename and make good R names
str(customer_train_clean_3)
customer_train_clean_3 <- rename(customer_train_clean_3, other = `(Other)`)
colnames(customer_train_clean_3) <- make.names(names(customer_train_clean_3))

#Keep transaction revenue value for denormalization
transaction_revenue_for_denorm <- customer_train_clean_3$transactionRevenue
#save(transaction_revenue_for_denorm, file = "transaction_revenue_for_denorm.RData")
#NORMALIZATION
#visitNumber, hits, pageviews, transactionRevenue

# Build normalize function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
# Normalize the customer_train_clean_3 data
customer_train_clean_3[,1:4] <- as.data.frame(lapply(customer_train_clean_3[1:4], normalize))

#save(transaction_revenue_for_denorm, customer_train_clean_2, 
#     customer_train_clean_3, file = "training_data.RData")

