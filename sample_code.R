
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
library(plyr)
library(psych)
setwd("C:/Users/kathy/Documents/My Documents/Coursework/Fall_2015_Stats_229/Project")
source(paste0(getwd(),"/CS-229-Project/sample_code_library.R"))


# ================================================================================= #
# open connections
n <- dbDriver("SQLite")
con <- dbConnect(n, dbname="compData.db")

# ================================================================================= #
# Create dataset with Ndiagnosis, Nmedication, Nlab, AddTranscript
train <- create_flattenedDataset(con, "training", 25, 0, 0, 1, 25)

# Assign training and validation set
set.seed(100)
train.frac = 0.75
obs.T <- sample(1:nrow(train), size = floor(train.frac*nrow(train)))


# ================================================================================= #
# Summary Statistics
summary(train[obs.T,])
table(train[,"dmIndicator"])/nrow(train)
## good summary of numeric variables
desc.stats <- describeBy(train, "dmIndicator",mat=TRUE)
write.csv(desc.stats, "desc.stats.csv")

## possibly plot things
require(ggplot2)
ggplot(train[obs.T,], aes(x=ct.401.1_5digit, colour=as.factor(dmIndicator))) + geom_density()
ggplot(train[obs.T,], aes(x=SystolicBP.med, y=DiastolicBP.med, colour=as.factor(dmIndicator))) + geom_point()

# ================================================================================= #

## Random Forest
## important:   make sure all predictors are numeric or factors
rf <- randomForest(x=train[obs.T,3:ncol(train)], y=train$dmIndicator[obs.T], 
                   ntree = 500, importance = TRUE)

## Boosting?
## SVM?

## Error analysis
## Confusion matrix
## What kind of examples is it getting wrong?

#myPred <- data.frame(test$PatientGuid, rf_result)
#write.table(myPred[order(myPred$test.PatientGuid),], "sample.csv", sep=',', row.names=FALSE, quote=TRUE, col.names=FALSE)





