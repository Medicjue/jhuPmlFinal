# load the required packages
install.packages("caret")
install.packages("rattle")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("repmis")
install.packages("e1071")

library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(randomForest)
library(repmis)
library(e1071)


training <- read.csv("pml-training.csv", na.strings = c("NA", ""))
testing <- read.csv("pml-testing.csv", na.strings = c("NA", ""))

training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]

trainData <- training[, -c(1:7)]
testData <- testing[, -c(1:7)]

set.seed(7826) 
inTrain <- createDataPartition(trainData$classe, p = 0.7, list = FALSE)
train <- trainData[inTrain, ]
valid <- trainData[-inTrain, ]



control <- trainControl(method = "cv", number = 5)
fitRpart <- train(classe ~ ., data = train, method = "rpart", 
                   trControl = control)
print(fitRpart, digits = 4)

# predict outcomes using validation set
predictRpart <- predict(fitRpart, valid)


fancyRpartPlot(fitRpart$finalModel)

# Show prediction result
confRpart <- confusionMatrix(valid$classe, predict_rpart)
## Confusion Matrix and Statistics


accuracyRpart <- confRpart$overall[1]


##### Using Random Forest... #####
fitRf <- train(classe ~ ., data = train, method = "rf", trControl = control)
print(fitRf, digits = 4)

# predict outcomes using validation set
predictRf <- predict(fitRf, valid)
# Show prediction result
confRf <- confusionMatrix(valid$classe, predictRf)

accuracyRf <- confRf$overall[1]

predict(fitRf, testData)
## Result: B A B A A E D B A A B C B A E E A B B B