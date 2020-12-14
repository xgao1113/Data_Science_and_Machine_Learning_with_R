# Tree Methods Project
# Use the College data frame from the ISLR library to classify schools as Private or Public
library(ISLR)
head(College)
str(College)
any(is.na(College))

# EDA
# Scatter plot of Grad.Rate vs Room.Board, colored by Private
library(ggplot2)
ggplot(College,aes(Room.Board,Grad.Rate)) + geom_point(aes(color=Private)) + ggtitle("Grad.Rate vs Room.Board")

# Histogram of full time undergrad students, colored by Private
ggplot(College,aes(F.Undergrad)) + geom_histogram(bins=50,aes(fill=Private),color='black') + ggtitle("Distribution of Full-Time Undergrads")

# Histogram of Grad.Rate colored by Private
ggplot(College,aes(Grad.Rate)) + geom_histogram(bins = 50,aes(fill=Private),color='black') + ggtitle("Distribution of Grad Rate")

# Change the grad rate>100% to 100%:
College$Grad.Rate <- ifelse(College$Grad.Rate>100,100,College$Grad.Rate)
table(College$Grad.Rate)

# Train Test Split
library(caTools)
set.seed(101)
sample <- sample.split(College$Private,SplitRatio = 0.7)
train <- subset(College,sample == TRUE)
test <- subset(College,sample == FALSE)


### Build Decision Tree Model:
library(rpart)
tree <- rpart(Private ~ .,method='class',data=train)

predicted.private <- predict(tree,test)
head(predicted.private)

# based on the No and Yes columns, create a Predicted Private column
tree.pred <- as.data.frame(predicted.private)

private_label <- function(x){
  if(x>0.5){
    return('Yes')
  }else{
    'No'
  }
}

tree.pred$Private <- sapply(tree.pred$Yes,private_label)

# Create the confusion matrix of the tree model:
table(tree.pred$Private,test$Private)

# Use the rpart.plot and prp() to plot out the tree model
library(rpart.plot)
prp(tree)


### Random Forest Model
library(randomForest)
rf.model <- randomForest(Private ~ .,data = train,importance=TRUE)
rf.model$confusion
rf.model$importance

# Predictions
rf.pred <- predict(rf.model,test)
head(rf.pred)
rf.pred <- as.data.frame(rf.pred)
head(rf.pred)

# Confusion Matrix:
table(rf.pred$rf.pred,test$Private)
