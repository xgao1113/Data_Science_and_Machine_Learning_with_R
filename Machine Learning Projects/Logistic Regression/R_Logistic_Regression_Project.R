# Logistic Regression Project
# This project aims at using the UCI adult dataset to attempt to predict
# if people in this data set belong in a certain salary class i.e. <=50K or >50K/year

## Read the Data
adult <- read.csv('adult_sal.csv')
print(head(adult))

library(dplyr)
adult <- select(adult,-X)
print(head(adult))
str(adult)
summary(adult)


## Clean the data
# There are many columns with categorical factors
# Try to clean these columns and by reducing the number of factors

# type_employer column:
table(adult$type_employer)
# there are 1836 Null values, 7 Never-Worked, and 14 Without-pay
# combine the "Never-worked" and "Without-pay" to "Unemployed":
adult$type_employer <- ifelse(adult$type_employer=='Never-worked' | adult$type_employer=='Without-pay','Unemployed',adult$type_employer)
# combine the "Local-gov" and "State-gov" to "SL-gov":
adult$type_employer <- ifelse(adult$type_employer=='Local-gov' | adult$type_employer=='State-gov','SL-gov',adult$type_employer)
# combine the "Self-emp-inc" and "Self-emp-not-inc" to "Self-emp":
adult$type_employer <- ifelse(adult$type_employer=='Self-emp-inc' | adult$type_employer=='Self-emp-not-inc','Self-emp',adult$type_employer)
table(adult$type_employer)

# Marital Column:
table(adult$marital)
# Regroup marital status into 3 groups:
# Married, Not-married, Never-married
group_marital <- function(mar){
  mar <- as.character(mar)
  if(mar=='Divorced' | mar=='Separated' | mar=='Widowed'){
    return('Not-married')
  }else if(mar=='Never-married'){
    return('Never-married')
  }else{
    return('Married')
  }
}
adult$marital <- sapply(adult$marital,group_marital)
table(adult$marital)

# Country Column:
table(adult$country)
Asia <- c('Cambodia','China','Hong','India','Iran','Japan','Laos','Philippines','South','Taiwan','Thailand','Vietnam')
North.America <- c('Canada','United-States')
Lat.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador','El-Salvador','Guatemala','Haiti','Honduras','Jamaica','Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru','Puerto-Rico','Trinadad&Tobago')
Europe <- c('England','France','Germany','Greece','Holand-Netherlands','Hungary','Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

group_country <- function(ctry){
  ctry <- as.character(ctry)
  if(ctry %in% Asia){
    return('Asia')
  }else if(ctry %in% North.America){
    return('North.America')
  }else if(ctry %in% Lat.America){
    return('Lat.America')
  }else if(ctry %in% Europe){
    return('Europe')
  }else{
    return('Other')
  }
}
adult$country <- sapply(adult$country,group_country)
table(adult$country)

str(adult)

# convert the type_employer,marital,country columns to factors
adult$type_employer <- as.factor(adult$type_employer)
adult$marital <- as.factor(adult$marital)
adult$country <- as.factor(adult$country)

str(adult)

table(adult$education_num)
table(adult$occupation)
# will not process the education column to factors as we already have edu_num column
# too many occupation categories, can keep using type_employer


## Missing Data:
library(Amelia)
adult[adult == '?'] <- NA
table(adult$occupation)
table(adult$type_employer)

adult$type_employer <- factor(adult$type_employer)
adult$marital <- factor(adult$marital)
adult$country <- factor(adult$country)
adult$income <- factor(adult$income)
table(adult$type_employer)
table(adult$marital)
table(adult$country)

missmap(adult,col = c('yellow','black'),legend = FALSE)

# missing values of occupation align with type_employer
# it's hard to infer these 2 factors based on other metrics
# therefore, drop the NA values
adult <- na.omit(adult)
str(adult)
missmap(adult,col = c('yellow','black'),legend = FALSE)


## EDA:
library(ggplot2)
# Plot histogram of ages, coloered by income
ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),binwidth = 1,color='black') + ggtitle("Distribution of Age") + theme_bw()
# Plot histogram of hours worked per week:
ggplot(adult,aes(hr_per_week)) + geom_histogram() + ggtitle("Distribution of Hours Worked per Week") + theme_bw()
# Rename country column to region
adult <- rename(adult,region = country)
# Create a barplot of region, colored by income
ggplot(adult,aes(region)) + geom_bar(aes(fill=income),color='black') + ggtitle("Distribution of Regions") + theme_bw() + theme(axis.text.x = element_text(angle = 90,hjust = 1))


## Building the Model:
print(head(adult))
# Train Test Split:
library(caTools)
set.seed(101)

sample <- sample.split(adult$income,SplitRatio = 0.7)

train <- subset(adult,sample==TRUE)
test <- subset(adult,sample==FALSE)

# Training the model:
model <- glm(income ~ ., family = binomial(logit),data=train)
summary(model)

# Try to eliminate features that are no important by using step()
new.model <- step(model)
summary(new.model)

# Create a confusion matrix:
test$predicted.income <- predict(model,newdata = test,type = "response")
table(test$income,test$predicted.income > 0.5)

# Accuracy of the model:
(6370+1425)/(6370+1425+550+870) # 0.8459034

# Recall of the model:
6370/(6370+550) # 0.9205202

# precision of the model:
6370/(6370+870)
 # 0.8798343

