# KNN Project
# Using Iris data set to attempt to predict the species of an iris flower


# Get the Data:
library(ISLR)
print(head(iris))
print(str(iris))

# Standardize Data:
new.iris <- scale(iris[1:4])
head(new.iris)
var(new.iris[,1])
var(new.iris[,4])

final.iris <- cbind(new.iris,iris[5])
head(final.iris)

# Train and Test Splits
library(caTools)
set.seed(101)
sample <- sample.split(final.iris$Species,SplitRatio = 0.7)
train <- subset(final.iris, sample==TRUE)
test <- subset(final.iris, sample==FALSE)

# Build a KNN model
library(class)
predicted.species <- knn(train[1:4],test[1:4],train$Species,k=1)
print(predicted.species)

missclass.rate <- mean(test$Species != predicted.species)
print(missclass.rate)

# Choosing a K value
predicted.species <- NULL
error.rate <- NULL

for (i in 1:20){
  set.seed(101)
  predicted.species <- knn(train[1:4],test[1:4],train$Species,k=i)
  error.rate[i] <- mean(test$Species != predicted.species)
}
print(predicted.species)
print(error.rate)

# Plot the error rate for k values ranging from 1 to 20:
library(ggplot2)
error.df <- data.frame(k.values=1:20,error.rate)
ggplot(error.df,aes(k.values,error.rate)) + geom_point() + geom_line(lty="dotted",color='red') + ggtitle("Error Rates vs K Values")
