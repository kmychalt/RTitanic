#set working directory and import data files
setwd("~/RStudio/Titanic")
library(readr)
train <- read_csv("~/RStudio/Titanic/train.csv")
view(train)
library(readr)
test <- read_csv("~/RStudio/Titanic/test.csv")
View(test)
str(train)
#shows all entries of the Survived column
train$Survived
#sorts the entries of the Survived column into number of entries
table(train$Survived)
#shows percentages of each Survived outcome
prop.table(table(train$Survived))
#creates a Survived entry in the test table 419 times
test$Survived <- rep(0, 418)
#create the new dataframe, submit, with PassengerId and Survived columns 
#of test file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
#write table to file, row numbers excluded for submission
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
#score so far: .62679

#convert character to factor so that summary will work properly
train$Sex <- as.factor(train$Sex)
summary(train$Sex)
#comparison table of Sex and Survived
prop.table(table(train$Sex, train$Survived))
#need a better comparison, one that is row-wise and shows proportion by groups
prop.table(table(train$Sex, train$Survived),1)
#set the Survived column of test to 0
test$Survived <- 0
#if females is the Sex of the entry, Survived = 1
test$Survived[test$Sex == 'female'] <- 1
#write to file to submit
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
#score so far: .76555

#need to take into account age
#create Child entry in train table
train$Child <- 0
#if younger than 18, then is child
train$Child[train$Age < 18] <- 1
#any passengers with unknown age, NA, will have 0 as child
#supplies table with Survived as target variable and Child and Sex variables
#included, train is the dataset, and the function (FUN) is sum
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
#need not just number but proportion. We make our own function to do so
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

#there is not much change in survival based on whether a child
#or not. need to take into account ticket fare.
#make a variable for different fare levels and divide them
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'
#find proportion of survivors based on this new variable
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
#make new predictionn with this information and submit
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
#score now: .77990