
# sample_code.R
# Sample code for the Practice Fusion Diabetes Classification Competition.
# This codes provides an example of how to flatten the data set features for
# diagnoses, medications, and labs and computes a basic random forest benchmark
# for a transformed dataset with 2 diagnoses, 5 medications and 3 labs.
#
# Requires the provided SQLite database.
# Requires file sample_code_library.R
# 7-July-2012
#
# ================================================================================= #

library(RSQLite)
library(randomForest)
# Assumes sample_code_library.R is in current working directory
setwd("C:/Users/kathy/Documents/My Documents/Coursework/Fall_2015_Stats_229/Project")
source(paste0(getwd(),"/CS-229-Project/sample_code_library.R"))


# ================================================================================= #
# open connections
n <- dbDriver("SQLite")
con <- dbConnect(n, dbname="compData.db")


# ================================================================================= #
# Create dataset with Ndiagnosis, Nmedication, Nlab, AddTranscript
train <- create_flattenedDataset(con, "training", 10, 0, 0, 1)
#test <- create_flattenedDataset(con, "test", 2, 5, 3)

# ================================================================================= #
# Summary Statistics
summary(train)

# ================================================================================= #

## Wrap all of this in cross-validation to produce MSE estimates


rf <- randomForest(train[,3:ncol(train)], train$dmIndicator)
rf_result <- predict(rf, test[,2:ncol(test)], type="response")

## Boosting?
## SVM?

## Error analysis
## Confusion matrix
## What kind of examples is it getting wrong?

#myPred <- data.frame(test$PatientGuid, rf_result)
#write.table(myPred[order(myPred$test.PatientGuid),], "sample.csv", sep=',', row.names=FALSE, quote=TRUE, col.names=FALSE)





