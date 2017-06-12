library(caret)
library(ggplot2)
library(dplyr)
library(h2o)
library(rpart.plot)
set.seed(0)


download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "trainingdata.csv")
download.file(url = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "testdata.csv")


traindata <- read.csv("trainingdata.csv", na.strings = c("NA", "", "#DIV/0!"))
testdata <- read.csv("testdata.csv", na.strings = c("NA", "", "#DIV/0!"))
data <- traindata

# Complete cases in training data
table(complete.cases(data))
sum(colSums(is.na(data))==0) #Columns where all rows are populated
table(sapply(data, function(x) sum(!is.na(x))))
#Except for the 60 columns where all rows are populated, the remaining columns are very sparsely populated (<3%). So we will drop all such variables for our analysis 


# Drop columns where any of the rows are NA
data <- data[,colSums(is.na(data))==0]

# Drop columns that are not metrics "X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window"              
data <- data[,-1:-7]

#index1 <- createDataPartition(data$classe, p = 0.8, list = FALSE)
#temp <- data[index1,]
#validate <- data[-index1,]
index <- createDataPartition(data$classe, p = 0.8, list = FALSE)
train <- data[index,]
test <- data[-index,]


## Building the model
# Random Forests
model1 <- train(classe ~ ., data = train, method = "rf", trControl = trainControl(method = "cv", number = 5))
predicted_classe1 <- predict(model1, test)
confusionMatrix(predicted_classe1, test$classe)

table(train$classe)

# Trees
model2 <- rpart(classe ~ ., data = train, method = "class")
predicted_classe2 <- predict(model2, test)
predicted_classe2 <- colnames(predicted_classe2)[apply(predicted_classe2,1,which.max)]
confusionMatrix(predicted_classe2, test$classe)
rpart.plot(model2)

# Bagging
model3 <- train(classe ~ ., data = train, method = "treebag", trControl = trainControl(method = "cv", number = 5))
predicted_classe3 <- predict(model3, test)
confusionMatrix(predicted_classe3, test$classe)

#Quiz
testdata1 <- testdata[,colSums(is.na(data))==0]
testdata1 <- testdata1[,-1:-7]
predict(model1, testdata1)
