#Load dataset
link = "http://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt"
banknote <- read.table(link, header = FALSE, sep = ",")

colnames(banknote) <- c("variance", "skewness", "curtosis", "entropy", "class")

#View(banknote)

#Check the variable properties
str(banknote)

banknote$class<- as.factor(banknote$class)

str(banknote)

summary(banknote)

#check if there is any NA's
anyNA(banknote)

########################
###                  ###
###   VISUALIZATION  ###
###   UNIVARIATE     ###
###   ANALYSIS       ###
###                  ###
########################
# loading packages
library(ggplot2)
library(magrittr)

#Plot histogram of variables except for class
ggplot(data = reshape2::melt(banknote[, -5]), mapping = aes(x = value)) + 
  geom_histogram(bins = 20, fill = "dodgerblue4") + facet_wrap(~variable, scales = 'free_x')+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#Plot box plot of variables except for class variable
#Box plot makes it easy to see outliers and distributions

par( mfrow = c( 2, 2 ) )
boxplot(banknote$variance, col= "dodgerblue4", pch=19)
mtext("variance of Wavelet Transformed image", cex=0.8, side=1, line=2)

boxplot(banknote$skewness, col= "dodgerblue4", pch=19)
mtext("skewness of Wavelet Transformed image", cex=0.8, side=1, line=2)

boxplot(banknote$curtosis, col= "dodgerblue4", pch=19)
mtext("curtosis of Wavelet Transformed image", cex=0.8, side=1, line=2)

boxplot(banknote$entropy, col= "dodgerblue4", pch=19)
mtext("entropy of image", cex=0.8, side=1, line=2)

#Bar chart of class
barplot(table(banknote$class), col = "dodgerblue4", main = "class")

#table of class
table(banknote$class)

#check the percentage
round(100*prop.table(table(banknote$class)), 2)
paste0(round(100*prop.table(table(banknote$class)), 2), "%")

########################
###                  ###
###   VISUALIZATION  ###
###   BIVARIATE      ###
###   ANALYSIS       ###
###                  ###
########################
# variance vs skewness
banknote %>%
  ggplot(aes(x=variance, y=skewness, color=class)) +
  geom_point()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# variance vs curtosis
banknote %>%
  ggplot(aes(x=variance, y=curtosis, color=class)) +
  geom_point()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


# variance vs entropy
banknote %>%
  ggplot(aes(x=variance, y=entropy, color=class)) +
  geom_point()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


# skewness vs curtosis
banknote %>%
  ggplot(aes(x=skewness, y=curtosis, color=class)) +
  geom_point()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


# skewness vs entropy
banknote %>%
  ggplot(aes(x=skewness, y=entropy, color=class)) +
  geom_point()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


# curtosis vs entropy
banknote %>%
  ggplot(aes(x=curtosis, y=entropy, color=class)) +
  geom_point()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


#put all charts together
#plot pairwise scatterplot
pairs(banknote[, -5], gap=0, pch=19, cex=0.4, col=banknote[,5])


#pearson correlation
round(cor(banknote[, -5], method = "pearson"), 2)

########################
###                  ###
###   NORMALIZATION  ###
###   OF NUMERICAL   ###
###   VARIABLES      ###
###                  ###
########################

# Build normalize function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
# Normalize the `banknote` data
banknote_norm <- as.data.frame(lapply(banknote[1:4], normalize))

#join the normalized continuous variables with class variable
banknote_norm <- cbind(banknote_norm, banknote[5])

# Summarize `banknote_norm`
summary(banknote_norm)
