library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

library(randomForest)
library(knitr)

rm(list = ls())

download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "pml-training.csv")
download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "pml-testing.csv")


training <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testing <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))


training <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))  
testing <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

training <-training[,colSums(is.na(training)) == 0]
testing <-testing[,colSums(is.na(testing)) == 0]

training   <-training[,-c(1:7)]
testing <-testing[,-c(1:7)]

head(training,n=5)
head(testing,n=5)

tail(training, n=5)
tail(testing, n=5)

dim(training)
dim(testing)

inTrain <- createDataPartition(y = training$classe, p=0.75, list = FALSE)
training <- training[inTrain, ]
testing <- training[-inTrain, ]

set.seed(311411)

fitData <- rpart(classe ~ .,training, method="class")
# Normal plot
rpart.plot(fitData)

#Use model to predict classe in validation testing set
predData <- predict(fitData, testing, type = "class")

#Estimate the errors of the prediction algorithm in the Decision Tree model
confusionMatrixData <-confusionMatrix(as.factor(testing$classe), predData)

confusionMatrixData

# Accuracy plot
plot(confusionMatrixData$table, col = confusionMatrixData$byClass, 
     main = paste("Decision Tree Confusion Matrix: Accuracy =", round(confusionMatrixData$overall['Accuracy'], 4)))


randomForestModel <- randomForest(as.factor(classe)~., data=training)
# Summary of the model
randomForestModel

varImpPlot(randomForestModel)

# Confusion matrix with testing
predTesting <- predict(randomForestModel, testing)
randomForestConfMatrix  <- confusionMatrix(as.factor(testing$classe), predTesting)
randomForestConfMatrix

plot(rfcfm$table, col = rfcfm$byClass, main = paste("Random Forest Confusion Matrix: Accuracy =", round(rfcfm$overall['Accuracy'], 4)))


plot(randomForestModel)




