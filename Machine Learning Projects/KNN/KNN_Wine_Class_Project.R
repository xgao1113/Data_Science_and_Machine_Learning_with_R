# KNN Project with UCI Wine data: http://archive.ics.uci.edu/ml/datasets/Wine

# Get the data:
names <- c('Class','Alcohol','Malic acid','Ash','Alcalinity of ash','Magnesium','Total phenols','Flavanoids','Nonflavanoid phenols','Proanthocyanins','Color intensity','Hue','OD280/OD315 of diluted wines','Proline')
wine <- read.table("wine.data",sep = ",",col.names = names)
head(wine)
str(wine)

wine$Class <- as.factor(wine$Class)
str(wine)

any(is.na(wine))

# Standardize Data:
new.wine <- scale(wine[2:14])
var(new.wine[,2])
var(new.wine[,13])

final.wine <- cbind(new.wine,wine[1])
head(final.wine)
str(final.wine)

# Train and Test Splits
library(caTools)
set.seed(101)
sample <- sample.split(final.wine$Class,SplitRatio = 0.7)
train <- subset(final.wine,sample==TRUE)
test <- subset(final.wine,sample==FALSE)

# Build the KNN model
library(class)
predicted.classes <- knn(train[1:13],test[1:13],train$Class,k=1)
misclass.error <- mean(test$Class != predicted.classes)
print(misclass.error)

# Choose a K value
predicted.classes <- NULL
error.rate <- NULL
for (i in 1:20){
  predicted.classes <- knn(train[1:13],test[1:13],train$Class,k=i)
  error.rate[i] <- mean(test$Class != predicted.classes)
}
print(predicted.classes)
print(error.rate)

# Plot the error rate vs k values
library(ggplot2)
error.df <- data.frame(k.values=1:20,error.rate)
ggplot(error.df,aes(k.values,error.rate)) + geom_point() + geom_line(lty='dotted',color='red') + ggtitle("Error Rates vs K values (Wine Class)")

# Based on the plot it seems that errors flat when k=5
predicted.classes <- knn(train[1:13],test[1:13],train$Class,k=5)
misclass.error <- mean(test$Class != predicted.classes)
print(misclass.error)
# The error rate when k=5 is 0.0754717



