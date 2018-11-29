library(tidyverse)
library(doSNOW) 
library(foreach)
library(rlist)
library(DataExplorer)
library(caret)

customer_rev <- read_csv("test.csv")

length(unique(customer_rev$date))
max(customer_rev$date)
min(customer_rev$date)

dim(customer_rev)
names(customer_rev)
fullVisitorId_test <- customer_rev$fullVisitorId
#Parallel program to flatten JSON column
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
customer_test <- as.data.frame(cbind(customer_rev, device_f, geoNetwork_f, totals_f, trafficSource_f))
rm(customer_rev, geoNetwork_f, device_f, trafficSource_f, totals_f)
#REMOVE IDs
#fullVisitorId, sessionId, visitId
cols_1 <- c("fullVisitorId", "sessionId", "visitId")
customer_test_clean_1 <- customer_test %>% select(-one_of(cols_1))
#names(customer_test_clean_1)
rm(customer_test)

#REMOVE (ONLY ONE VALUE THROUGHOUT THE DATASET)
cols_2 <- c("socialEngagementType","browserVersion","browserSize","operatingSystemVersion",
            "mobileDeviceBranding","mobileDeviceModel","mobileInputSelector","mobileDeviceInfo",
            "mobileDeviceMarketingName","flashVersion","language","screenColors","screenResolution",
            "cityId","latitude","longitude","networkLocation","adwordsClickInfo.criteriaParameters",
            "visits")
customer_test_clean_1 <- customer_test_clean_1 %>% select(-one_of(cols_2))
names(customer_test_clean_1)

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

cols_3 <- c("visitStartTime", "adContent", "adwordsClickInfo.isVideoAd", 
            "adwordsClickInfo.adNetworkType", "adwordsClickInfo.slot", 
            "adwordsClickInfo.page", "adwordsClickInfo.gclId")

customer_test_clean_1 <- customer_test_clean_1 %>% select(-one_of(cols_3))
#names(customer_test_clean_1)
#dim(customer_test_clean_1)

#keep 
customer_test_clean_2 <- customer_test_clean_1 
#filter pageview where is.na(pageviews)
customer_test_clean_2$pageviews[is.na(customer_test_clean_2$pageviews)] <- 0
#table(customer_test_clean_2$pageviews)
rm(customer_test_clean_1)
#if bounces == 1, "YES", ELSE "NO"
customer_test_clean_2$bounces[!is.na(customer_test_clean_2$bounces)] <- "YES"
#unique(customer_test_clean_2$bounces)
customer_test_clean_2$bounces[is.na(customer_test_clean_2$bounces)] <- "NO"
#unique(customer_test_clean_2$bounces)


#if newVisit == 1, "NEW", "NOT NEW"
customer_test_clean_2$newVisits[!is.na(customer_test_clean_2$newVisits)] <- "NEW"
#unique(customer_test_clean_2$newVisits)
customer_test_clean_2$newVisits[is.na(customer_test_clean_2$newVisits)] <- "NOT NEW"
#unique(customer_test_clean_2$newVisits)

#TO BE REMOVED FOR NOW (OTHERWISE NEEDS EXPERT REVIEW)
#unique(missing_variables$keyword) #if keyword== "(not provided)", "NO", ELSE "YES"
#unique(missing_variables$isTrueDirect) #TRUE AND NA? REMOVE?
#unique(missing_variables$referralPath) #YES, NA? REMOVE?

cols_4 <- c("keyword", "isTrueDirect", "referralPath")
customer_test_clean_2 <- customer_test_clean_2 %>% select(-one_of(cols_4))
#names(customer_test_clean_2)
#dim(customer_test_clean_2)


#	Extract features from date (Year, Month, Weekday, etc.) 
#year
customer_test_clean_2$year <- lubridate::year(customer_test_clean_2$date)

#Month
customer_test_clean_2$month <- lubridate::month(customer_test_clean_2$date, label = T, abbr = T)
#names(customer_test_clean_2)

#Weekday
customer_test_clean_2$weekday <- lubridate::wday(customer_test_clean_2$date, label = T, abbr = T)
#names(customer_test_clean_2)

anyNA(customer_test_clean_2)
#plot_missing(customer_test_clean_2)

#No transaction

##REMOVE DATE
names(customer_test_clean_2)
customer_test_clean_2 <- customer_test_clean_2[, -2]
#names(customer_test_clean_2)
#write.csv(customer_test_clean_2, file = "checker.csv")

##CHECK VARIABLE IMPORTANCE USING RANDOM FOREST
#str(customer_test_clean_2)
#unique(customer_test_clean_2$pageviews)
#Integers but am going to code it numeric because of scaling and or normalization
customer_test_clean_2$visitNumber <- as.numeric(customer_test_clean_2$visitNumber)
customer_test_clean_2$hits <- as.numeric(customer_test_clean_2$hits)
customer_test_clean_2$pageviews <- as.numeric(customer_test_clean_2$pageviews)

cols_c <- c("channelGrouping", "browser", "operatingSystem", "isMobile",
            "deviceCategory", "continent", "subContinent", "country",
            "region", "metro", "city", "networkDomain", "bounces", "newVisits",
            "campaign", "source","medium", "year" )


customer_test_clean_2[cols_c] <- lapply(customer_test_clean_2[cols_c], factor)
str(customer_test_clean_2)
rm(cols_1,cols_2,cols_3,cols_4,cols_c)

#REDUCING SOME HIGHER DIMENTIONALITY AND NON LINEAR SIGNIFICANT
#THIS CAN BE REVIEWD LATER
cols_n <- c("country","region","metro","city","networkDomain","campaign","source","year","month")
customer_test_clean_2 <- customer_test_clean_2 %>% select(-one_of(cols_n))
#names(customer_test_clean_2)
#str(customer_test_clean_2)
rm(cols_n)


##SELECT VARIABLE RECOMMENDED BY BACKWARD STEPWISE REGRESSION
##Result from backward stepwise regression
#transactionRevenue ~ channelGrouping + visitNumber + 
#  hits + pageviews + bounces + newVisits + weekday
customer_test_clean_3 <- customer_test_clean_2 %>% 
  select( "channelGrouping", "visitNumber",
         "hits", "pageviews", "bounces", "newVisits", "weekday")

str(customer_test_clean_3)

########
### VISUALIZE
#######
dim(customer_test_clean_3)
barplot(table(customer_test_clean_3$channelGrouping))
barplot(table(customer_test_clean_3$bounces))
barplot(table(customer_test_clean_3$newVisits))
barplot(table(customer_test_clean_3$weekday))

############
## FEETURE EXTRACTION? on customer_test_clean_3
####################
#channelGrouping
channel_group <- data.frame(Reduce(cbind, 
                                   lapply(levels(customer_test_clean_3$channelGrouping), 
                                          function(x){(customer_test_clean_3$channelGrouping == x) * 1})))

names(channel_group) <- levels(customer_test_clean_3$channelGrouping)
str(channel_group)
customer_test_clean_3 <- cbind(customer_test_clean_3, channel_group)
customer_test_clean_3 <- subset(customer_test_clean_3, select = -channelGrouping)
str(customer_test_clean_3)

#bounces
bounces_f <- data.frame(Reduce(cbind, 
                               lapply(levels(customer_test_clean_3$bounces), 
                                      function(x){(customer_test_clean_3$bounces == x) * 1})))

names(bounces_f) <- levels(customer_test_clean_3$bounces)
str(bounces_f)
customer_test_clean_3 <- cbind(customer_test_clean_3, bounces_f)
customer_test_clean_3 <- subset(customer_test_clean_3, select = -bounces)
str(customer_test_clean_3)

#newVisits
new_visits <- data.frame(Reduce(cbind, 
                                lapply(levels(customer_test_clean_3$newVisits), 
                                       function(x){(customer_test_clean_3$newVisits == x) * 1})))

names(new_visits) <- levels(customer_test_clean_3$newVisits)
str(new_visits)
customer_test_clean_3 <- cbind(customer_test_clean_3, new_visits)
customer_test_clean_3 <- subset(customer_test_clean_3, select = -newVisits)
str(customer_test_clean_3)

#weekday
week_day <- data.frame(Reduce(cbind, 
                              lapply(levels(customer_test_clean_3$weekday), 
                                     function(x){(customer_test_clean_3$weekday == x) * 1})))

names(week_day) <- levels(customer_test_clean_3$weekday)
str(week_day)
customer_test_clean_3 <- cbind(customer_test_clean_3, week_day)
customer_test_clean_3 <- subset(customer_test_clean_3, select = -weekday)
str(customer_test_clean_3)

rm(bounces_f, channel_group, new_visits, week_day)

#Rename and make good R names
str(customer_test_clean_3)
customer_test_clean_3 <- rename(customer_test_clean_3, other = `(Other)`)
colnames(customer_test_clean_3) <- make.names(names(customer_test_clean_3))

#NORMALIZATION
#visitNumber, hits, pageviews, transactionRevenue

# Build normalize function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
# Normalize the customer_test_clean_3 data
customer_test_clean_3[,1:3] <- as.data.frame(lapply(customer_test_clean_3[1:3], normalize))


#save(customer_test_clean_2, customer_test_clean_3, fullVisitorId_test, file = "test_data.RData")
#save(fullVisitorId_test, file = "fullVisitorId_test.RData")
