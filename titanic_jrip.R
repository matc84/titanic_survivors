library(gmodels) ## for CrossTable
library(class) ## for kNN



setwd("C:/Users/matth_000/Desktop/kaggle/Titanic")
train <- read.csv("train.csv")

##Impute NAs in the age column

## Find the rows with NA in the Age column
NAs <- is.na(train$Age)

## Find the average age for people with "Miss" in their name (this is a decent indicator of age)

miss <- grepl("Miss", train$Name, ignore.case = FALSE)
miss_avg_age = mean(train[miss,]$Age, na.rm = TRUE)

## Replace the NAs for people named "Miss with the average age of the other people named "Miss"
train[miss & NAs,]$Age <- miss_avg_age


## Find the remaining rows with NAs in the Age column and find the average age for people with "Master" 
NAs <- is.na(train$Age)
master <- grepl("Master", train$Name, ignore.case = FALSE)
master_avg_age = mean(train[master,]$Age, na.rm = TRUE)
train[master & NAs,]$Age <- master_avg_age

## Find the remaining rows with NAs in the Age column and replace NAs for people named "Mrs."
NAs <- is.na(train$Age)
mrs <- grepl("Mrs.", train$Name, ignore.case = FALSE)
mrs_avg_age = mean(train[mrs,]$Age, na.rm = TRUE)
train[mrs & NAs,]$Age <- mrs_avg_age

## Find the remaining rows with NAs in the Age column and replace NAs for people named "Mr."
NAs <- is.na(train$Age)
mr <- grepl("Mr.", train$Name, ignore.case = FALSE)
mr_avg_age = mean(train[mr,]$Age, na.rm = TRUE)
train[mr & NAs,]$Age <- mr_avg_age

## Find the remaining rows with NAs in the Age column and replace NAs for people named "Dr."
NAs <- is.na(train$Age)
dr <- grepl("Dr.", train$Name, ignore.case = FALSE)
dr_avg_age = mean(train[dr,]$Age, na.rm = TRUE)
train[dr & NAs,]$Age <- dr_avg_age



train <- train[,-11] ## Remove cabin feature
train <- train[,-4]  ## Remove name feature
train <- train[,-8]  ## Remove ticket feature
train <- train[,-1]  ## Remove passenger id 



## turn age into a factor variable
under_5 <- train$Age < 5
under_10 <- train$Age >= 5 & train$Age < 10
under_15 <- train$Age >= 10 & train$Age < 15
under_20 <- train$Age >= 15 & train$Age < 20
under_25 <- train$Age >= 20 & train$Age < 25
under_30 <- train$Age >= 25 & train$Age < 30
under_40 <- train$Age >= 30 & train$Age < 40
under_50 <- train$Age >= 40 & train$Age < 50
under_60 <- train$Age >= 50 & train$Age < 60
over_60 <- train$Age >= 60

train[under_5,]$Age <- "A"
train[under_10,]$Age <- "B"
train[under_15,]$Age <- "C"
train[under_20,]$Age <- "D"
train[under_25,]$Age <- "E"
train[under_30,]$Age <- "F"
train[under_40,]$Age <- "G"
train[under_50,]$Age <- "H"
train[under_60,]$Age <- "I"
train[over_60,]$Age <- "J"

## categorize fares by quartiles
summary(train$Fare)

zero <- train$Fare < 1
under_ten_dollars <- train$Fare >= 1 & train$Fare < 10
under_twenty_dollars <- train$Fare >= 10 & train$Fare < 20
under_thirty_dollars <- train$Fare >= 20 & train$Fare < 30
under_fifty_dollars <- train$Fare >= 30 & train$Fare < 50
under_one_hundred_dollars <- train$Fare >= 50 & train$Fare < 100
under_two_hundred_dollars <- train$Fare >= 100 & train$Fare < 200
over_two_hundred_dollars <- train$Fare >= 200


train[zero,]$Fare <- "A"
train[under_ten_dollars,]$Fare <- "B"
train[under_twenty_dollars,]$Fare <- "C"
train[under_thirty_dollars,]$Fare <- "D"
train[under_fifty_dollars,]$Fare <- "E"
train[under_one_hundred_dollars,]$Fare <- "F"
train[under_two_hundred_dollars,]$Fare <- "G"
train[over_two_hundred_dollars,]$Fare <- "H"

## make the other variables into factors

train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Age <- as.factor(train$Age)
train$SibSp <- as.factor(train$SibSp)
train$Parch <- as.factor(train$Parch)
train$Fare <- as.factor(train$Fare)

## make the model
titanic_jrip <- JRip(Survived ~ ., data = train[-8])

##inspect the model
titanic_jrip
summary(titanic_jrip)



######################################
## make adjustments on the test set ##
######################################

test <- read.csv("test.csv")

##Impute NAs in the age column

## Find the rows with NA in the Age column
NAs <- is.na(test$Age)

## Find the average age for people with "Miss" in their name (this is a decent indicator of age)

miss <- grepl("Miss", test$Name, ignore.case = FALSE)
miss_avg_age = mean(test[miss,]$Age, na.rm = TRUE)

## Replace the NAs for people named "Miss with the average age of the other people named "Miss"
test[miss & NAs,]$Age <- miss_avg_age


## Find the remaining rows with NAs in the Age column and find the average age for people with "Master" 
NAs <- is.na(test$Age)
master <- grepl("Master", test$Name, ignore.case = FALSE)
master_avg_age = mean(test[master,]$Age, na.rm = TRUE)
test[master & NAs,]$Age <- master_avg_age

## Find the remaining rows with NAs in the Age column and replace NAs for people named "Mrs."
NAs <- is.na(test$Age)
mrs <- grepl("Mrs.", test$Name, ignore.case = FALSE)
mrs_avg_age = mean(test[mrs,]$Age, na.rm = TRUE)
test[mrs & NAs,]$Age <- mrs_avg_age

## Find the remaining rows with NAs in the Age column and replace NAs for people named "Mr."
NAs <- is.na(test$Age)
mr <- grepl("Mr.", test$Name, ignore.case = FALSE)
mr_avg_age = mean(test[mr,]$Age, na.rm = TRUE)
test[mr & NAs,]$Age <- mr_avg_age

## Find the remaining rows with NAs in the Age column and replace NAs for people named "Dr."
NAs <- is.na(test$Age)
dr <- grepl("Dr.", test$Name, ignore.case = FALSE)
dr_avg_age = mean(test[dr,]$Age, na.rm = TRUE)
test[dr & NAs,]$Age <- dr_avg_age

## Find the remaining rows with NAs in the Age column and replace NAs for people named "Ms."
NAs <- is.na(test$Age)
ms <- grepl("Ms.", test$Name, ignore.case = FALSE)
ms_avg_age = mean(test[ms,]$Age, na.rm = TRUE)
test[ms & NAs,]$Age <- ms_avg_age

## Doesn't work because there's only on Ms.
## But since there's only one, we can look her up
## She was 21.25

test[ms & NAs,]$Age <- 21.25



### Impute NAs for $Fare
test_mean_fare <- mean(test$Fare, na.rm = TRUE)
NAs2 <- is.na(test$Fare)
test$Fare[NAs2] <- test_mean_fare

test <- test[,-10] ## Remove cabin feature
test <- test[,-3]  ## Remove name feature
test <- test[,-7]  ## Remove ticket feature
test <- test[,-1]  ## Remove Passenger id


## turn age into a factor variable
under_5 <- test$Age < 5
under_10 <- test$Age >= 5 & test$Age < 10
under_15 <- test$Age >= 10 & test$Age < 15
under_20 <- test$Age >= 15 & test$Age < 20
under_25 <- test$Age >= 20 & test$Age < 25
under_30 <- test$Age >= 25 & test$Age < 30
under_40 <- test$Age >= 30 & test$Age < 40
under_50 <- test$Age >= 40 & test$Age < 50
under_60 <- test$Age >= 50 & test$Age < 60
over_60 <- test$Age >= 60

test[under_5,]$Age <- "A"
test[under_10,]$Age <- "B"
test[under_15,]$Age <- "C"
test[under_20,]$Age <- "D"
test[under_25,]$Age <- "E"
test[under_30,]$Age <- "F"
test[under_40,]$Age <- "G"
test[under_50,]$Age <- "H"
test[under_60,]$Age <- "I"
test[over_60,]$Age <- "J"

## categorize fares 
summary(test$Fare)

zero <- test$Fare < 1
under_ten_dollars <- test$Fare >= 1 & test$Fare < 10
under_twenty_dollars <- test$Fare >= 10 & test$Fare < 20
under_thirty_dollars <- test$Fare >= 20 & test$Fare < 30
under_fifty_dollars <- test$Fare >= 30 & test$Fare < 50
under_one_hundred_dollars <- test$Fare >= 50 & test$Fare < 100
under_two_hundred_dollars <- test$Fare >= 100 & test$Fare < 200
over_two_hundred_dollars <- test$Fare >= 200


test[zero,]$Fare <- "A"
test[under_ten_dollars,]$Fare <- "B"
test[under_twenty_dollars,]$Fare <- "C"
test[under_thirty_dollars,]$Fare <- "D"
test[under_fifty_dollars,]$Fare <- "E"
test[under_one_hundred_dollars,]$Fare <- "F"
test[under_two_hundred_dollars,]$Fare <- "G"
test[over_two_hundred_dollars,]$Fare <- "H"

## make the other variables into factors

test$Survived <- as.factor(test$Survived)
test$Pclass <- as.factor(test$Pclass)
test$Age <- as.factor(test$Age)
test$SibSp <- as.factor(test$SibSp)
test$Parch <- as.factor(test$Parch)
test$Fare <- as.factor(test$Fare)

## make the other variables into factors

test$Survived <- as.factor(test$Survived)
test$Pclass <- as.factor(test$Pclass)
test$Age <- as.factor(test$Age)
test$SibSp <- as.factor(test$SibSp)
test$Parch <- as.factor(test$Parch)
test$Fare <- as.factor(test$Fare)

## Make a prediction table
titanic_test_predictions <- predict(titanic_jrip, test[-7])
surv_col <- as.data.frame(titanic_test_predictions)
passenger_id <- as.data.frame(seq(892,1309))

my_predicted_data <- cbind(passenger_id, surv_col)
names(my_predicted_data) <- c("PassengerId", "Survived")
write.csv(my_predicted_data, "my_predictions_jrip_2.csv", row.names = FALSE)

