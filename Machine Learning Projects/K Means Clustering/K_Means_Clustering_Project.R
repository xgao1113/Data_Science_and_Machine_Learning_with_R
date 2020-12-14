# K Means Clustering Project
# Use the data from UCI to attempt to predict if the wine is red or white
# http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/


# Get the data:
df1 <- read.csv('winequality-red.csv',sep = ";")
df2 <- read.csv('winequality-white.csv',sep = ";")

df1$label <- 'red'
df2$label <- 'white'

head(df1)
head(df2)

wine <- rbind(df1,df2)
head(wine)
str(wine)


# EDA
# Create a histogram of residual.sugar, colored by label
library(ggplot2)
ggplot(wine,aes(residual.sugar,fill=label)) + geom_histogram(color='black',bins = 50) + ggtitle("Distribution of Residual Sugar") + theme_bw() + scale_fill_manual(values = c('#621514','#fff8e7'))
# Create a histogram of citric.acid, colored by label
ggplot(wine,aes(citric.acid,fill=label)) + geom_histogram(color='black',bins = 50) + ggtitle("Distribution of Citric Acid") + theme_bw() + scale_fill_manual(values = c('#621514','#fff8e7'))
# Create a histogram of alcohol, colored by label
ggplot(wine,aes(alcohol,fill=label)) + geom_histogram(color='black',bins = 50) + ggtitle("Distribution of Alcohol") + theme_bw() + scale_fill_manual(values = c('#621514','#fff8e7'))
# creae a a scatterplot of residual.sugar vs citric.acid, colored by label
ggplot(wine,aes(citric.acid,residual.sugar,color=label)) + geom_point(alpha=0.2,size=2) + ggtitle("Residual Sugar vs Citric Acid") + theme_dark() + scale_color_manual(values = c('#621514','#fff8e7'))
# Create a scatterplot of residual.ssugar vs volatile.acidity,colored by label
ggplot(wine,aes(volatile.acidity,residual.sugar,color=label)) + geom_point(alpha=0.2,size=2) + ggtitle("Residual Sugar vs Volatile Acidity") + theme_dark() + scale_color_manual(values = c('#621514','#fff8e7'))

# Get the wine data without the red/white label:
library(dplyr)
clus.data <- select(wine,-label)
head(clus.data)
str(clus.data)


# Building the Clusters
wine.cluster <- kmeans(clus.data,centers = 2)
print(wine.cluster$centers)


# Evaluating the Clusters
table(wine$label,wine.cluster$cluster)

# Looks like it's easier to cluster red together,...
# ...as there are a lot of noises with white wines based off the EDA visualizations
