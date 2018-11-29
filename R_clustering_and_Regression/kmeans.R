 # install.packages("NbClust")
 # install.packages("fpc")
 # install.packages("flexclust")
 # install.packages("tidyverse")
library(tidyverse)
library(NbClust)
library(fpc)
library(MASS)
library(flexclust)

 #Load dataset
white_wine <- readxl::read_xlsx("Whitewine.xlsx", sheet = 1)

#Check the variable properties
str(white_wine)
summary(white_wine)

#check if there is any NA's
anyNA(white_wine)

#Plot histogram of variables except for quality
windows(8,8) #To plot in a new window
ggplot(data = reshape2::melt(white_wine[, -12]), mapping = aes(x = value)) + 
  geom_histogram(bins = 20, fill = "dodgerblue4") + facet_wrap(~variable, scales = 'free_x')+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Bar chart of quality
barplot((table(white_wine$quality)), col= "dodgerblue4" )

#how many quality classes
unique(white_wine$quality)
 
length(unique(white_wine$quality))

#table of quality
table(white_wine$quality)

#check the percentage
round(100*prop.table(table(white_wine$quality)), 2)
paste0(round(100*prop.table(table(white_wine$quality)), 2), "%")

#Plot box plot of variables except for quality
#Box plot makes it easy to see outliers and distributions

par( mfrow = c( 2, 6 ) )
boxplot(white_wine$`fixed acidity`, col= "dodgerblue4", pch=19)
mtext("fixed acidity", cex=0.8, side=1, line=2)

boxplot(white_wine$`volatile acidity`, col= "dodgerblue4", pch=19)
mtext("volatile acidity", cex=0.8, side=1, line=2)

boxplot(white_wine$`citric acid`, col= "dodgerblue4", pch=19)
mtext("citric acid", cex=0.8, side=1, line=2)

boxplot(white_wine$`residual sugar`, col= "dodgerblue4", pch=19)
mtext("residual sugar", cex=0.8, side=1, line=2)

boxplot(white_wine$chlorides, col= "dodgerblue4", pch=19)
mtext("chlorides", cex=0.8, side=1, line=2)

boxplot(white_wine$`free sulfur dioxide`, col= "dodgerblue4", pch=19)
mtext("free sulfur dioxide", cex=0.8, side=1, line=2)

boxplot(white_wine$`total sulfur dioxide`, col= "dodgerblue4", pch=19)
mtext("total sulfur dioxide", cex=0.8, side=1, line=2)

boxplot(white_wine$density, col= "dodgerblue4", pch=19)
mtext("density", cex=0.8, side=1, line=2)

boxplot(white_wine$pH, col= "dodgerblue4", pch=19)
mtext("pH", cex=0.8, side=1, line=2)

boxplot(white_wine$sulphates, col= "dodgerblue4", pch=19)
mtext("sulphates", cex=0.8, side=1, line=2)

boxplot(white_wine$alcohol, col= "dodgerblue4", pch=19)
mtext("alcohol", cex=0.8, side=1, line=2)

#Summary statistics to support outliers
install.packages("psych")
library("psych")
psych::describe(white_wine[, -12])
summary(white_wine[, -12])

#pearson and spearman rank correlation
round(cor(white_wine[, -12], method = "pearson"), 2)

#plot pairwise scatterplot
pairs(white_wine[, -12], gap=0, pch=19, cex=0.4, col="darkblue")

###


######
#FUNCTION TO REMOVE OUTLIER
######
remove_outliers <- function(x) {
  qnt <- quantile(x, probs=c(.25, .75))
  checker <- 1.5 * IQR(x)
  x[x < (qnt[[1]] - checker)] <- NA
  x[x > (qnt[[2]] + checker)] <- NA
  x
}

white_wine_11 <- as.data.frame(lapply(white_wine[, -12], remove_outliers))
nrow(white_wine_11)
summary(white_wine_11)

white_wine_new <- cbind(white_wine_11, white_wine[ , 12])
nrow(white_wine_new)
summary(white_wine_new)

white_wine_newer <- white_wine_new %>% drop_na()
nrow(white_wine_newer)
summary(white_wine_newer)


#Predictor selector
white_wine_input <- white_wine_newer[, -12]
names(white_wine_input)
length(white_wine_input)

white_wine_output <- as.factor(white_wine_newer$quality)
str(white_wine_output)

#scale
white_wine_train <- scale(white_wine_input)
summary(white_wine_train)

#Model fitting
#determine best number of cluster using NBClust fuction:
nc <- NbClust(white_wine_train,
              min.nc=2, max.nc=15,
              method="kmeans")

# Table
table(nc$Best.n[1,])

#plot it in barchart
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters",
        ylab="Number of Criteria",
        main="Number of Clusters Chosen by 24 Criteria")

# Another method to determine the best number of clusters
wss <- 0
for (i in 1:15){
  wss[i] <-
    sum(kmeans(white_wine_train, centers=i)$withinss)
}
plot(1:15,
     wss,
     type="b",     ### "b" for both
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")


#Fit kmeans for k = 2
set.seed(3211)
fit_km_2 <- kmeans(white_wine_train, 2)

#Then interpret the result
fit_km_2$centers
ls(fit_km_2)
# Centroid Plot against 1st 2 discriminant functions
#To draw discriminant projection plot
plotcluster(white_wine_train, fit_km_2$cluster)

#we draw parallel coordinates plot to see how variables contributed in each cluster
windows(8,8) #To plot in a new window
parcoord(white_wine_train, fit_km_2$cluster)

# We calculate cluster means
aggregate(white_wine_input,by=list(fit_km_2$cluster),FUN=mean)

#compare to the column 12
confuseTable_km <- table(white_wine_output, fit_km_2$cluster)
confuseTable_km
randIndex(confuseTable_km)


##############
## For k = 3
##############
#Fit kmeans for k = 3
set.seed(3211)
fit_km_3 <- kmeans(white_wine_train, 3)

#Then interpret the result
fit_km_3

# Centroid Plot against 1st 2 discriminant functions
#To draw discriminant projection plot
plotcluster(white_wine_train, fit_km_3$cluster)

#we draw parallel coordinates plot to see how variables contributed in each cluster
windows(8,8) #To plot in a new window
parcoord(white_wine_train, fit_km_3$cluster)

# We calculate cluster means
aggregate(white_wine_input,by=list(fit_km_3$cluster),FUN=mean)

#compare to the column 12
confuseTable_km_3 <- table(white_wine_output, fit_km_3$cluster)
confuseTable_km_3
randIndex(confuseTable_km_3)


##############
## For k = 4
##############
#Fit kmeans for k = 4
set.seed(3211)
fit_km_4 <- kmeans(white_wine_train, 4)

#Then interpret the result
fit_km_4

# Centroid Plot against 1st 2 discriminant functions
#To draw discriminant projection plot
plotcluster(white_wine_train, fit_km_4$cluster)

#we draw parallel coordinates plot to see how variables contributed in each cluster
windows(8,8) #To plot in a new window
parcoord(white_wine_train, fit_km_4$cluster)

# We calculate cluster means
aggregate(white_wine_input,by=list(fit_km_4$cluster),FUN=mean)

#compare to the column 12
confuseTable_km_4 <- table(white_wine_output, fit_km_4$cluster)
confuseTable_km_4
randIndex(confuseTable_km_4)


##############
## For k = 5
##############
#Fit kmeans for k = 5
set.seed(3211)
fit_km_5 <- kmeans(white_wine_train, 5)

#Then interpret the result
fit_km_5

# Centroid Plot against 1st 2 discriminant functions
#To draw discriminant projection plot
plotcluster(white_wine_train, fit_km_5$cluster)

#we draw parallel coordinates plot to see how variables contributed in each cluster
windows(8,8) #To plot in a new window
parcoord(white_wine_train, fit_km_5$cluster)

# We calculate cluster means
aggregate(white_wine_input,by=list(fit_km_5$cluster),FUN=mean)

#compare to the column 12
confuseTable_km_5 <- table(white_wine_output, fit_km_5$cluster)
confuseTable_km_5
randIndex(confuseTable_km_5)

##############
## For k = 6
##############
#Fit kmeans for k = 6
set.seed(3211)
fit_km_6 <- kmeans(white_wine_train, 6)

#Then interpret the result
fit_km_6

# Centroid Plot against 1st 2 discriminant functions
#To draw discriminant projection plot
plotcluster(white_wine_train, fit_km_6$cluster)

#we draw parallel coordinates plot to see how variables contributed in each cluster
windows(8,8) #To plot in a new window
parcoord(white_wine_train, fit_km_6$cluster)

# We calculate cluster means
aggregate(white_wine_input,by=list(fit_km_6$cluster),FUN=mean)

#compare to the column 12
confuseTable_km_6 <- table(white_wine_output, fit_km_6$cluster)
confuseTable_km_6
randIndex(confuseTable_km_6)

##############
## For k = 7
##############
#Fit kmeans for k = 7
set.seed(3211)
fit_km_7 <- kmeans(white_wine_train, 7)

#Then interpret the result
fit_km_7

# Centroid Plot against 1st 2 discriminant functions
#To draw discriminant projection plot
plotcluster(white_wine_train, fit_km_7$cluster)

#we draw parallel coordinates plot to see how variables contributed in each cluster
windows(8,8) #To plot in a new window
parcoord(white_wine_train, fit_km_7$cluster)

# We calculate cluster means
aggregate(white_wine_input,by=list(fit_km_7$cluster),FUN=mean)

#compare to the column 12
confuseTable_km_7 <- table(white_wine_output, fit_km_7$cluster)
confuseTable_km_7
randIndex(confuseTable_km_7)

##############
## For k = 8
##############
#Fit kmeans for k = 8
set.seed(3211)
fit_km_8 <- kmeans(white_wine_train, 8)

#Then interpret the result
fit_km_8

# Centroid Plot against 1st 2 discriminant functions
#To draw discriminant projection plot
plotcluster(white_wine_train, fit_km_8$cluster)

#we draw parallel coordinates plot to see how variables contributed in each cluster
windows(8,8) #To plot in a new window
parcoord(white_wine_train, fit_km_8$cluster)

# We calculate cluster means
aggregate(white_wine_input,by=list(fit_km_8$cluster),FUN=mean)

#compare to the column 12
confuseTable_km_8 <- table(white_wine_output, fit_km_8$cluster)
confuseTable_km_8
randIndex(confuseTable_km_8)

##############
## For k = 9
##############
#Fit kmeans for k = 9
set.seed(3211)
fit_km_9 <- kmeans(white_wine_train, 9)

#Then interpret the result
fit_km_9

# Centroid Plot against 1st 2 discriminant functions
#To draw discriminant projection plot
plotcluster(white_wine_train, fit_km_9$cluster)

#we draw parallel coordinates plot to see how variables contributed in each cluster
windows(8,8) #To plot in a new window
parcoord(white_wine_train, fit_km_9$cluster)

# We calculate cluster means
aggregate(white_wine_input,by=list(fit_km_9$cluster),FUN=mean)

#compare to the column 12
confuseTable_km_9 <- table(white_wine_output, fit_km_9$cluster)
confuseTable_km_9
randIndex(confuseTable_km_9)


