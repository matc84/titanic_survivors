library(gmodels) ## for CrossTable
library(class) ## for kNN



setwd("C:/Users/matth_000/Desktop/kaggle")
train <- read.csv("train.csv")
test <- read.csv("test.csv")


## Impute missing data with the average of the column

test_mean <- mean(train$Age, na.rm = TRUE)
NAs <- is.na(test$Age)
test$Age[NAs] <- test_mean

test_mean_fare <- mean(test$Fare, na.rm = TRUE)
NAs2 <- is.na(test$Fare)
test$Fare[NAs2] <- test_mean_fare


test <- test[,-10] ## Remove cabin feature
test <- test[,-3]  ## Remove name feature
test <- test[,-7]  ## Remove ticket feature
test <- test[,-1]  ## Remove Passenger id



## Make sex numeric
## Try with embarked as a feature


test$Embarked <- as.numeric((test$Embarked)) - 1
test$Sex <- as.numeric(test$Sex) - 1


## Normalize
normalize <- function(x) {
        return((x - min(x)) / (max(x) - min(x)))
}


test <- as.data.frame(lapply(test[1:7], normalize))


## Try kNN again
tit_test_pred <- knn(train = train[,2:8],
                     test = test,
                     cl = train[,1],
                     k = 21,
                     l = 0,
                     prob = TRUE)



table(tit_test_pred)
surv_col <- as.data.frame(tit_test_pred)
passenger_id <- as.data.frame(seq(892,1309))

my_predicted_data <- cbind(passenger_id, surv_col)
names(my_predicted_data) <- c("PassengerId", "Survived")
write.csv(my_predicted_data, "my_predictions2.csv")


## Keep improving
train_set <- train[1:712,]
test_set <- train[713:891,]

tit_test_pred <- knn(train = train_set[,2:8],
                     test = test_set[,2:8],
                     cl = train_set[,1],
                     k = 50,
                     l = 0,
                     prob = TRUE)

CrossTable(x = test_set[,1], 
           y = tit_test_pred,
           prop.chisq = FALSE)

