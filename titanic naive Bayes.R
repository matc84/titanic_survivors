library(gmodels) ## for CrossTable
## Naive bayes
library(e1071)


setwd("C:/Users/matth_000/Desktop/kaggle")
train <- read.csv("train.csv")
test <- read.csv("test.csv")

train_mean <- mean(train$Age, na.rm = TRUE)

NAs <- is.na(train$Age)

train$Age[NAs] <- train_mean


train <- train[,-11] ## Remove cabin feature
train <- train[,-4]  ## Remove name feature
train <- train[,-8] ## Remove ticket feature
train <- train[,-1] ## Remove passenger id




## Normalize
normalize <- function(x) {
        return((x - min(x)) / (max(x) - min(x)))
}


train$Fare <- normalize(train$Fare)
train$Age <- normalize(train$Age)


## convert counts
convert_counts <- function(x) {
        x <- ifelse(x > 0.5, 1, 0)
        x <- factor(x,
                    levels = c(0,1),
                    labels = c("No", "Yes")
        )
        return(x)
}



train$Age <- convert_counts(train$Age)
train$Fare <- convert_counts(train$Fare)

## Turn all other columns into factors

train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$SibSp <- as.factor(train$SibSp)
train$Parch <- as.factor(train$Parch)


## Split data

train_length <- floor(nrow(train)*0.8)
tit_train <- train[1:train_length,]
tit_test <- train[train_length:nrow(train),]

tit_train_labels <- tit_train[1:train_length,2]
tit_test_labels <- tit_test[1:nrow(tit_test),2]



titanic_test <- titanic_test[-1]
titanic_train <- titanic_train[-1]

train <- train[,-1]

## Make classifier## Make classifier
tit_classifier <- naiveBayes(tit_train[,-1], tit_train[,1])
tit_test_pred <- predict(tit_classifier, tit_test)

## Compare the results
CrossTable(tit_test_pred,
           tit_test[,1],
           prop.chisq = FALSE
           )


## Results: 148/180 = 82%


## Try with laplace = 1

tit_classifier <- naiveBayes(tit_train[,-1], tit_train[,1], laplace = 0)
tit_test_pred <- predict(tit_classifier, tit_test)

## Compare the results
CrossTable(tit_test_pred,
           tit_test[,1],
           prop.chisq = FALSE
)



