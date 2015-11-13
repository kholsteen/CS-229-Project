
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
# Clear work space
rm(list = ls(all = TRUE)) 

# Load Libraries
library(RSQLite)
library(randomForest)
library(plyr)
library(psych)
library(ROCR)
library(doBy)
library(cvTools)
library(MASS)
library(GGally)
library(ggplot2)

# Set up working directory 
# # Katherine
# setwd("C:/Users/kathy/Documents/My Documents/Coursework/Fall_2015_Stats_229/Project")
# source(paste0(getwd(),"/CS-229-Project/sample_code_library.R"))

# # Haju
# setwd("C:/Users/Haju Kim/Dropbox/Stanford/2015-2016/1Q/CS 229/Project")
# source("C:/Users/Haju Kim/Dropbox/Stanford/2015-2016/1Q/CS 229/Project/sample_code_library.R")

# ================================================================================= #
# Open connections
n <- dbDriver("SQLite")
con <- dbConnect(n, dbname="compData.db")
dbListTables(con) # Display the list of all data tables 

# ================================================================================= #
# Create dataset with Ndiagnosis, Nmedication, Nlab, AddTranscript
train <- create_flattenedDataset(con, "training", 25, 0, 0, 1, 25)


# ================================================================================= #
# Split the data into training and test sets
df = train
set.seed(123)
train.ind = sample(1:nrow(df), 0.75*nrow(df))
df.train = df[train.ind,]
df.test = df[-train.ind,]



# Descriptive Statistics


# ================================================================================= #
# Summary Statistics
summary(df.train)

# Display P(dm = 1), P(dm = 0)
prop.table(table(df.train$dmIndicator))

# Summary of numeric variables
desc.stats <- describeBy(df.train, "dmIndicator",mat=TRUE)
write.csv(desc.stats, "desc.stats.csv")

## Pairs plot to check if there's any variable that seems highly correlated
ggpairs(df.train)

## possibly plot things
require(ggplot2)
ggplot(df.train, aes(x=ct.401.1_5digit, colour=as.factor(dmIndicator))) + geom_density()
ggplot(df.train, aes(x=SystolicBP.med, y=DiastolicBP.med, colour=as.factor(dmIndicator))) + geom_point()

# ================================================================================= #

## Random Forest
## important:   make sure all predictors are numeric or factors
#rf <- randomForest(x=train[obs.T,3:ncol(train)], y=train$dmIndicator[obs.T], 
                   ntree = 500, importance = TRUE)

## Note: "df.train[,3:ncol(df.train)]" should be replaced with our final feature matrix X
bestmtry = tuneRF(df.train[,3:ncol(df.train)],df.train$dmIndicator, ntreeTry=100, 
                  stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)

df.train.rf = randomForest(dmIndicator~.,data=df.train[,3:ncol(df.train)], mtry=2, ntree=1000, 
                           keep.forest=TRUE, importance=TRUE, test=df.test)

df.train.rf.pr = predict(df.train.rf, type="prob", newdata=df.test)[,2]

df.train.pred = prediction(df.train.rf.pr, df.test$dmIndicator)
df.train.perf = performance(df.train.pred,"tpr","fpr")

# Plot ROC Curve
plot(df.train.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

# Variable Importance Plot
importance(df.train.rf)
varImpPlot(df.train.rf)

# Error rate plot
plot(df.train.rf)


# CROSS VALIDATION ===========================================================================
## Wrap all of this in cross-validation to produce MSE estimates
# Note: For now, just use the train set to divide training/test set and see what the outcome is like ==========

# Run cross validation to get the estimate 

# Fit a linear model using all covariates to predict quality (Baseline model)
# [Example]: model = lm(quality ~ ., data = train)

# Use cross validation to get an estimate for the prediction error (K = 10, Replication = 10)
# model = MODEL_GOES_HERE
model.cv = cvFit(model, data = df.train, y = df.train$dmIndicator, cost = mspe, K = 10, R = 10)


# Out-of-Sample Test Error ==============================================================
# Compute the prediction loss from the fitted values
mspe(train$Y, predict(model), includeSE = TRUE)


## Boosting?
## SVM?

## Error analysis
## Confusion matrix
## What kind of examples is it getting wrong?

#myPred <- data.frame(test$PatientGuid, rf_result)
#write.table(myPred[order(myPred$test.PatientGuid),], "sample.csv", sep=',', row.names=FALSE, quote=TRUE, col.names=FALSE)





