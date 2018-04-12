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

# Two rows have no data for $embarked
# use "S" as it is by far the most common

table(train$Embarked)
train[train$Embarked != "C" & train$Embarked != "S" & train$Embarked != "Q",]$Embarked = "S"


model <- lm(Survived ~ ., data = train)

model
summary(model)

# the model tells us that the Parch, Fare, and Embarked columns are not statistically meaningful
# our adjusted r-squared is 0.3961, meaning we've only accounted for less than 40% of the variance
# try again

model <- lm(Survived ~ Pclass + Sex + Age + SibSp, data = train)
model
summary(model)


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

## make predictions

surv_col <- as.data.frame(predict(model, test))
surv_col$`predict(model, test)` <- round(surv_col$`predict(model, test)`)
passenger_id <- as.data.frame(seq(892,1309))

my_predicted_data <- cbind(passenger_id, surv_col)
names(my_predicted_data) <- c("PassengerId", "Survived")
write.csv(my_predicted_data, "my_predictions_lm_1.csv", row.names = FALSE)
