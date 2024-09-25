library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(ggplot2)
library(randomForest)
library(knitr)

rm(list = ls())

if (!file.exists("pml-training.csv")) {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "pml-training.csv")
}

if (!file.exists("pml-testing.csv")) {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "pml-testing.csv")
}

training_st <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testing_st <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

training_st <-training_st[,colSums(is.na(training_st)) == 0]
testing_st <-testing_st[,colSums(is.na(testing_st)) == 0]

training_st   <-training_st[,-c(1:7)]
testing_st <-testing_st[,-c(1:7)]

dim(training_st)
dim(testing_st)

inTrain <- createDataPartition(y = training_st$classe, p=0.75, list = FALSE)
training <- training_st[inTrain, ]
testing <- training_st[-inTrain, ]

set.seed(311411)

fitData <- rpart(classe ~ .,training, method="class")
rpart.plot(fitData)

#Use model to predict classe in validation testing set
predData <- predict(fitData, testing, type = "class")

#Estimate the errors of the prediction algorithm in the Decision Tree model
confusionMatrixData <-confusionMatrix(as.factor(testing$classe), predData)
confusionMatrixData

# Accuracy plot

randomForestModel <- randomForest(as.factor(classe)~., data=training)
# Summary of the model
randomForestModel

predTesting <- predict(randomForestModel, testing)
rfcfm  <- confusionMatrix(as.factor(testing$classe), predTesting)
rfcfm

# Convert confusion matrix to a data frame for ggplot
cm_df <- as.data.frame(rfcfm$table)

# Plot heatmap-style confusion matrix
g <- ggplot(data = cm_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = paste("Random Forest Confusion Matrix: Accuracy =", 
                     round(rfcfm$overall['Accuracy'], 4)), fill = "Frequency") +
  theme_minimal()

print(g)

varImpPlot(randomForestModel)


plot(randomForestModel)


rfPredict <- predict(randomForestModel, testing_st,type= "class")
rfPredict

