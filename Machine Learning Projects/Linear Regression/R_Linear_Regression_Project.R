# Linear Regression Project
# Bike Sharing Demand Kaggle Challenge: https://www.kaggle.com/c/bike-sharing-demand/data
# Build a model to predict # bike sharing used based on a series of metrics 


# Get the data:
# setwd("C:/Users/Qing/Documents/07_Udemy/R_for_DS_ML_Bootcamp/Training Excercises/Projects/Machine Learning Projects")
bike <- read.csv('bikeshare.csv')
head(bike)
summary(bike)


# EDA:
# Create a scatter plot of count vs. temp.
library(ggplot2)
ggplot(bike,aes(x=temp,y=count)) + geom_point(aes(color=temp),alpha=0.2,size=2) + theme_bw() + ggtitle('Count vs. Temperature')

# Create a scatter plot of count vs. datetime with a color gradient based on temp.
# Convert the datetime column into POSIXct
bike$datetime <- as.POSIXct(bike$datetime,format="%Y-%m-%d %H:%M:%S") 
ggplot(bike,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5) + scale_color_gradient(low = "#00b159",high = "#f37735") + ggtitle("Count vs Datetime")

# Correlation between temp and count
cor(bike[,c('temp','count')])

# Create a boxplot showing counts for each season
ggplot(bike,aes(season,count)) + geom_boxplot(aes(color=factor(season))) + ggtitle('Count vs Season')


# Feature Engineering:
# Create an "hour" column based on datetime column:
bike$hour <- sapply(bike$datetime, function(x){format(x,"%H")}) 

# Create a scatterplot of count vs hour, with color scale based on temperature
# Only use bike data where workingday == 1
library(dplyr)
pl <- ggplot(filter(bike,workingday==1),aes(hour,count)) + geom_point(aes(color=temp),alpha=0.5,position = position_jitter(w=1,h=1))
pl <- pl + scale_color_gradientn(colours = c("#0d34c1","#4bf624","#fbe816","#ff5c04","#fd1111"))
pl <- pl + theme_bw() + ggtitle("Count vs Hour")
print(pl)

# Create the same plot for non-working day
npl <- ggplot(filter(bike,workingday==0),aes(hour,count)) + geom_point(aes(color=temp),alpha=0.5,position = position_jitter(w=1,h=1))
npl <- npl + scale_color_gradientn(colours = c("#0d34c1","#4bf624","#fbe816","#ff5c04","#fd1111"))
npl <- npl + theme_bw() + ggtitle("Count vs Hour")
print(npl)


# Building the Model
# Use lm() to build a model to predict count based solely on the temp feature
temp.model <- lm(count~temp,bike)
summary(temp.model)

# The predicted bike rentals when temp == 25 is:
predict(temp.model,data.frame(temp=c(25)))

# Build a model that attemps to predict bike rentals based off the following features:
# season, holidy, workingday, weather, temp, humidity, windspeed, hour(factor)
bike$hour <- as.numeric(bike$hour)
model <- lm(count ~ . -datetime -atemp -casual - registered, bike)
summary(model)


