# Neural Nets Project
# Use the Bank Authentication data set from UCI to predict whether a Bank Note was authentic
# https://archive.ics.uci.edu/ml/datasets/banknote+authentication
# The data set consists of statistical info on images,...
# ...hence is not easy to interpret per the conventional data visualizations 

# Get the data
bank.note <- read.csv("bank_note_data.csv")
head(bank.note)
str(bank.note)


# Train Test Split
library(caTools)
set.seed(101)
sample <- sample.split(bank.note$Class,SplitRatio = 0.7)
train <- subset(bank.note,sample==TRUE)
test <- subset(bank.note,sample==FALSE)

str(train)


# Building the Neural Nets
library(neuralnet)
nn <- neuralnet(formula = Class ~ Image.Var + Image.Skew + Image.Curt + Entropy, data = train, hidden = 10, linear.output = FALSE)
plot(nn)


# Predictions
predicted.class <- compute(nn,test[1:4])
head(predicted.class$net.result)
predictions <- sapply(predicted.class$net.result, round)
head(predictions)
# Create a confusion matrix of the predictions vs the real values
table(predictions,test$Class)


# Comparing against Random Forest Model
library(randomForest)
bank.note$Class <- as.factor(bank.note$Class)

set.seed(101)
sample <- sample.split(bank.note$Class,SplitRatio = 0.7)
train <- subset(bank.note,sample==TRUE)
test <- subset(bank.note,sample==FALSE)

rf.model <- randomForest(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy, data = train)
rf.predictions <- predict(rf.model,test)

table(rf.predictions,test$Class)

# The neural net model works better than the random forest one