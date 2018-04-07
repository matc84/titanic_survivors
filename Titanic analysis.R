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


## Make sex and embarked numeric
train$Sex <- as.numeric(train$Sex) - 1
train$Embarked <- as.numeric((train$Embarked)) - 1


## Try normalizing
normalize <- function(x) {
        return((x - min(x)) / (max(x) - min(x)))
}


train <- as.data.frame(lapply(train, normalize))



train_length <- floor(nrow(train)*0.8)
tit_train <- train[1:train_length,]
tit_test <- train[train_length:nrow(train),]

tit_train_labels <- tit_train[1:train_length,1]
tit_test_labels <- tit_test[1:nrow(tit_test),1]


## no sex feature
#tit_test_pred <- knn(train = tit_train[3:7],
#                     test = tit_test[3:7],
#                     cl = tit_train_labels,
#                     k = 2)


#ct <- CrossTable(x = tit_test_labels, 
#           y = tit_test_pred,
#           prop.chisq = FALSE)

## Results after normalization: 120/180 (67%)

## Better results, try a different method
## Try k=3

# 
# tit_test_pred <- knn(train = tit_train[3:7],
#                      test = tit_test[3:7],
#                      cl = tit_train_labels,
#                      k = 21)
# 
# 
# ct <- CrossTable(x = tit_test_labels, 
#                  y = tit_test_pred,
#                  prop.chisq = FALSE)

## Results : 139/180 or 77% with k = 21



# ## Try kNN again
# tit_test_pred <- knn(train = tit_train[3:8],
#                      test = tit_test[3:8],
#                      cl = tit_train_labels,
#                      k = 2,
#                      l = 0)
# 
# ct <- CrossTable(x = tit_test_labels, 
#                  y = tit_test_pred,
#                  prop.chisq = FALSE)

## Results: 146/180 = 81%
## A much better result after converting male/female to boolean and using it in the analysis


## Try with embarked as a feature


train1 <- train[1:2]
train <- as.data.frame(lapply(train[3:9], normalize))
train <- cbind(train1, train)


train_length <- floor(nrow(train)*0.8)
tit_train <- train[1:train_length,]
tit_test <- train[train_length:nrow(train),]

tit_train_labels <- tit_train[1:train_length,2]
tit_test_labels <- tit_test[1:nrow(tit_test),2]


## Try kNN again
tit_test_pred <- knn(train = train[2:7],
                     test = test[2:7],
                     cl = train[,1],
                     k = 1,
                     l = 0)

ct <- CrossTable(x = test[,1], 
                 y = tit_test_pred,
                 prop.chisq = FALSE)


## Results: 180/180 (100%)


tit_test_pred <- knn(train = tit_train[3:9],
                     test = tit_test[3:9],
                     cl = tit_train_labels,
                     k = 21,
                     l = 20)

ct <- CrossTable(x = tit_test_labels, 
                 y = tit_test_pred,
                 prop.chisq = FALSE)

