library(caret)
set.seed(1)
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")          ##load the data into traning and testing sets
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]  ##After looking at the data, there are many coloums with all NA's so we get rid of them.
trainData <- training[, -c(1:7)]  ##also get rid of their names and other columns which aren't useful for prediction. 
testData <- testing[, -c(1:7)]
inTrain <- createDataPartition(trainData$classe, p = 0.8, list = FALSE)
train <- trainData[inTrain, ]
valid <- trainData[-inTrain, ]  ##create validation set to estimate the out of sample error.

mod_rf <- train(classe ~ ., data = train, method = "rf", trControl = trainControl(method = "cv",number=4))
## train using random forest with cross validation of 4 fold.
predict_rf <- predict(mod_rf, valid)  #predict using valid data set.
compare <- confusionMatrix(valid$classe, predict_rf)  #compare with real classe data.

result <- predict(mod_rf, testData) #using our model to predict the classe for test data.

print(result)
