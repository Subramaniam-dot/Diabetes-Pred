
# Importing libraries
library(RCurl) 
library(randomForest)
library(caret)

# Importing the Iris data set
acute <- read.csv(text = getURL("https://raw.githubusercontent.com/Subramaniam-dot/Diabetes-Pred/master/diabdata.csv") )

# Performs stratified random split of the data set

TrainingIndex <- caret::createDataPartition(acute$diabetes, p=0.8, list = FALSE)
TrainingSet <- acute[TrainingIndex,] # Training Set
TestingSet <- acute[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

# Building Random forest model

model <- randomForest(as.factor(diabetes) ~ ., data = TrainSet, ntree = 500, mtry = 8, importance = TRUE)

# Save model to RDS file
saveRDS(model, "model.rds")

