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


## Impute Embarked column
train[62,]$Embarked <- "C"
train[830,]$Embarked <- "S"


#################################################
###########Do same with test data ###############
#################################################

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




train <- train[,-11] ## Remove cabin feature
train <- train[,-4]  ## Remove name feature
train <- train[,-8]  ## Remove ticket feature
train <- train[,-1]  ## Remove passenger id 



test <- test[,-10] ## Remove cabin feature
test <- test[,-3]  ## Remove name feature
test <- test[,-7]  ## Remove ticket feature
test <- test[,-1]  ## Remove Passenger id



## Make sex and embarked numeric
train$Sex <- as.numeric(train$Sex) - 1
train$Embarked <- as.numeric(train$Embarked) - 2


test$Embarked <- as.numeric(test$Embarked) - 1
test$Sex <- as.numeric(test$Sex) - 1


## Try normalizing
normalize <- function(x) {
        return((x - min(x)) / (max(x) - min(x)))
}


train <- as.data.frame(lapply(train, normalize))
test <- as.data.frame(lapply(test, normalize))


## kNN
tit_test_pred <- knn(train = train[,2:6],
                     test = test[,1:5],
                     cl = train[,1],
                     k = 21,
                     l = 0,
                     prob = TRUE)



table(tit_test_pred)
surv_col <- as.data.frame(tit_test_pred)
passenger_id <- as.data.frame(seq(892,1309))

my_predicted_data <- cbind(passenger_id, surv_col)
names(my_predicted_data) <- c("PassengerId", "Survived")
write.csv(my_predicted_data, "my_predictions12.csv")






