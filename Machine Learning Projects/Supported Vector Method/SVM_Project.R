# Support Vector Machines
# Use data from LendingClub.com to create a model to classify...
# ...and predict whether or not the borrower paid back their loan in full

# Get the data:
loans <- read.csv('loan_data.csv')
head(loans)
summary(loans)
str(loans)
# Convert the categorical features from int to factor:
loans$credit.policy <- as.factor(loans$credit.policy)
loans$inq.last.6mths <- as.factor(loans$inq.last.6mths)
loans$delinq.2yrs <- as.factor(loans$delinq.2yrs)
loans$pub.rec <- as.factor(loans$pub.rec)
loans$not.fully.paid <- as.factor(loans$not.fully.paid)
str(loans)


# EDA
library(ggplot2)
# Create a histogram of fico scores, colored by not.fully.paid
ggplot(loans,aes(fico)) + geom_histogram(aes(fill=not.fully.paid),color='black',bins=40) + ggtitle("Distribution of FICO Scores") + theme_bw()
# Create a barplot of purposes counts, colored by not.fully.paid
ggplot(loans,aes(purpose)) + geom_bar(aes(fill=not.fully.paid),position = 'dodge') + ggtitle("Distribution of Purposes") + theme_bw()+ theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1)) 
# Create a scatterplot of fico scores vs int.rate
ggplot(loans,aes(int.rate,fico)) + geom_point(aes(color=not.fully.paid),alpha=0.5) + ggtitle("Fico Scores vs Interest Rates") + theme_bw()


# Building the model:
# Train Test Split:
library(caTools)
sample <- sample.split(loans$not.fully.paid,SplitRatio = 0.7)
train <- subset(loans,sample ==TRUE)
test <- subset(loans,sample == FALSE)

library(e1071)
model <- svm(not.fully.paid ~ .,data = train)
summary(model)

# predict new values from test set using the model
predicted.values <- predict(model,test[1:13])
table(predicted.values,test[,14])


# Tuning the Model:
tuned.results <- tune(svm,train.x = not.fully.paid~.,data = train,kernel='radial',ranges=list(cost=c(1,5,10),gamma=c(0.1,1,1.5)))

model <- svm(not.fully.paid ~.,data=train,cost=10,gamma=0.1)
predicted.values <- predict(model,test[1:13])
table(predicted.values,test[,14])











