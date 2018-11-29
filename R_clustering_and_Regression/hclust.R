
 #install.packages("NbClust")
# install.packages("fpc")
# install.packages("flexclust")

install.packages("tidyverse")

 install.packages("dendextend")
# install.packages("colorspace")
 install.packages("gplots")
# install.packages("cluster")
#  library(MASS)
#  library(flexclust)
# #library(colorspace)
#  #library(fpc)
#  
#  
library(tidyverse)
library(dendextend)
library(gplots)
library(cluster)
library(NbClust)



white_wine <- readxl::read_xlsx("Whitewine.xlsx", sheet = 1)

#Check the variable properties
str(white_wine)
summary(white_wine)
#install.packages("Hmisc")
#library(Hmisc)
Hmisc::describe(white_wine)

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
#library("psych")
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

# First we create the distance matrix based on the Euclidean distance.
d <- dist(white_wine_train, method = "euclidean")

# Then we use hierarchical clustering, with the option single. But before that we
# need to determine what is the number of clusters that later should be considered
NbClust(white_wine_train, distance = "euclidean", min.nc=2, max.nc=10, 
        method = "single", index = "all", alphaBeale = 0.1) 

fit_single <- hclust(d, method="single")

windows(8,8) #To plot in a new window
plot(fit_single)
rect.hclust(fit_single, k=2, border="red")
#cut tree into 2 clusters
groups_single <- cutree(fit_single, k=2)
table(groups_single)
table(white_wine_output,groups_single)

# Look a the five obtained clusters
by(white_wine_input, groups_single, summary)

round(aggregate(white_wine_input,by=list(groups_single),FUN=mean), 2)


windows(8,8) #To plot in a new window
plot(fit_single)
rect.hclust(fit_single, k=5, border="red")
#cut tree into 2 clusters
groups_single <- cutree(fit_single, k=5)
table(groups_single)
table(white_wine_output,groups_single)

round(aggregate(white_wine_input,by=list(groups_single),FUN=mean), 2)

# Look a the five obtained clusters
by(white_wine_input, groups_single, summary)





windows(8,8) #To plot in a new window
plot(fit_single)
rect.hclust(fit_single, k=20, border="red")
#cut tree into 2 clusters
groups_single <- cutree(fit_single, k=20)
table(groups_single)
table(white_wine_output,groups_single)

# Look a the five obtained clusters
round(aggregate(white_wine_input,by=list(groups_single),FUN=mean), 2)

##################################################################
#COMPLETE METHOD
# First we create the distance matrix based on the Euclidean distance.
d <- dist(white_wine_train, method = "euclidean")

# Then we use hierarchical clustering, with the option single. But before that we
# need to determine what is the number of clusters that later should be considered
NbClust(white_wine_train, distance = "euclidean", min.nc=2, max.nc=10, 
        method = "complete", index = "all", alphaBeale = 0.1) 

fit_complete <- hclust(d, method="complete")

windows(8,8) #To plot in a new window
plot(fit_complete)
rect.hclust(fit_complete, k=2, border="red")
#cut tree into 2 clusters
groups_complete <- cutree(fit_complete, k=2)
table(groups_complete)
table(white_wine_output,groups_complete)

# Look a the five obtained clusters
by(white_wine_input, groups_complete, summary)

round(aggregate(white_wine_input,by=list(groups_complete),FUN=mean), 2)

##3
windows(8,8) #To plot in a new window
plot(fit_complete)
rect.hclust(fit_complete, k=3, border="red")
#cut tree into 2 clusters
groups_complete <- cutree(fit_complete, k=3)
table(groups_complete)
table(white_wine_output,groups_complete)

round(aggregate(white_wine_input,by=list(groups_complete),FUN=mean), 2)
# Look a the five obtained clusters
by(white_wine_input, groups_complete, summary)



##4
windows(8,8) #To plot in a new window
plot(fit_complete)
rect.hclust(fit_complete, k=4, border="red")
#cut tree into 2 clusters
groups_complete <- cutree(fit_complete, k=4)
table(groups_complete)
table(white_wine_output,groups_complete)

# Look a the five obtained clusters
by(white_wine_input, groups_complete, summary)

round(aggregate(white_wine_input,by=list(groups_complete),FUN=mean), 2)


##$$$$
fit_average <- hclust(d, method="average")
windows(8,8) #To plot in a new window
plot(fit_average)
rect.hclust(fit_average, k=2, border="red")
#cut tree into 2 clusters
groups_average <- cutree(fit_average, k=2)
table(groups_average)
table(white_wine_output,groups_average)

round(aggregate(white_wine_input,by=list(groups_average),FUN=mean), 2)
# Look a the five obtained clusters
by(white_wine_input, groups_average, summary)



##3
windows(8,8) #To plot in a new window
plot(fit_average)
rect.hclust(fit_average, k=3, border="red")
#cut tree into 2 clusters
groups_average <- cutree(fit_average, k=3)
table(groups_average)
table(white_wine_output,groups_average)

round(aggregate(white_wine_input,by=list(groups_average),FUN=mean), 2)
# Look a the five obtained clusters
by(white_wine_input, groups_average, summary)



##4
windows(8,8) #To plot in a new window
plot(fit_average)
rect.hclust(fit_average, k=4, border="red")
#cut tree into 2 clusters
groups_average <- cutree(fit_average, k=4)
table(groups_average)
table(white_wine_output,groups_average)

# Look a the five obtained clusters
by(white_wine_input, groups_average, summary)

round(aggregate(white_wine_input,by=list(groups_average),FUN=mean), 2)


#########centroid---- DID NOT PERFORM THIS, NO NEED TO
##$$$$
fit_centroid <- hclust(d, method="centroid")
windows(8,8) #To plot in a new window
plot(fit_centroid)
rect.hclust(fit_centroid, k=2, border="red")
#cut tree into 2 clusters
groups_centroid <- cutree(fit_centroid, k=2)
table(groups_centroid)
table(white_wine_output,groups_centroid)

round(aggregate(white_wine_input,by=list(groups_centroid),FUN=mean), 2)
# Look a the five obtained clusters
by(white_wine_input, groups_centroid, summary)



##3
windows(8,8) #To plot in a new window
plot(fit_centroid)
rect.hclust(fit_centroid, k=3, border="red")
#cut tree into 2 clusters
groups_average <- cutree(fit_centroid, k=3)
table(groups_centroid)
table(white_wine_output,groups_centroid)

round(aggregate(white_wine_input,by=list(groups_centroid),FUN=mean), 2)
# Look a the five obtained clusters
by(white_wine_input, groups_centroid, summary)



##4
windows(8,8) #To plot in a new window
plot(fit_centroid)
rect.hclust(fit_average, k=4, border="red")
#cut tree into 2 clusters
groups_average <- cutree(fit_centroid, k=4)
table(groups_centroid)
table(white_wine_output,groups_centroid)

# Look a the five obtained clusters
by(white_wine_input, groups_centroid, summary)

round(aggregate(white_wine_input,by=list(groups_centroid),FUN=mean), 2)

##############################################################
#Similarity/difference between various clustering algorithms
hclust_methods <- c("ward.D", "single", "complete", "average", "mcquitty", 
                    "median", "centroid", "ward.D2")
wine_dendlist <- dendlist()
for(i in seq_along(hclust_methods)) {
  hc_wine <- hclust(d, method = hclust_methods[i])   
  wine_dendlist <- dendlist(wine_dendlist, as.dendrogram(hc_wine))
}
names(wine_dendlist) <- hclust_methods
wine_dendlist

#Next, we can look at the cophenetic correlation between each clustering result using cor.dendlist
wine_dendlist_cor <- cor.dendlist(wine_dendlist)
wine_dendlist_cor
#the plot
corrplot::corrplot(wine_dendlist_cor, "pie", "lower")

##############################################################
#In addition, we can find a coefficient measuring the amount of cluster structure, 
#the “agglomerative coefficient”, ac:
#library(cluster)
wine_agnes_s <- agnes(d, method = "single")
wine_agnes_a <- agnes(d, method = "average")
wine_agnes_c <- agnes(d, method = "complete")
cbind(wine_agnes_s$ac, wine_agnes_a$ac, wine_agnes_c$ac)



