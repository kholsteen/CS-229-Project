
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


# CROSS VALIDATION ===========================================================================
## Wrap all of this in cross-validation to produce AUC

# ============ INSTALLATON =========================#
install.packages("pROC")

# ============= LIBRARIES =========================#
library(pROC)

#====================== CV for Random Forest=========================#
# ASSUMPTION: COLUMN1: BINARY OUTCOME VARIABLE; REST: FEATURE MATRIX

# Set Variable name
train = df.train[,-1]

# Set the number of folds (k)
k = 10 

# Initialize
n = floor(nrow(train)/k)
err.vect = rep(NA, k)

# Loop over different folds to compute cross-validation error
for(i in 1:k) {
  index_start = ((i - 1) * n + 1) # starti index of the subset
  index_end = (i * n) # end index of the subset
  subset = index_start:index_end # range of the subset
  
  cv.train = train[-subset, ]
  cv.test = train[subset, ]
  
  # TODO: Add bestmtry to find a better fit for the random forest
  bestmtry = tuneRF(cv.train[, -1], cv.train[, 1], ntreeTry=100, 
                  stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)
  # Run the random forest on the train set
  fit = randomForest(x = cv.train[, -1], y = as.factor(cv.train[, 1]), mtry=bestmtry, ntree=1000, keep.forest=TRUE, importance=TRUE)
  
  # Make predictions on the test set
  prediction = predict(fit, newdata = cv.test[, -1], type = "prob")[, 2] #
  
  # Calculate the model's accuracy for the ith fold
  err.vect[i] = auc(cv.test[,1], prediction)
  print(paste("AUC (fold ", i, "): ", err.vect[i]))
}

print(paste("Avg. AUI: ", mean(err.vect)))


# Out-of-Sample Test Error ==============================================================
# Compute the prediction loss from the fitted values
#mspe(train$Y, predict(model), includeSE = TRUE)


## Boosting?
## SVM?

## Error analysis
## Confusion matrix
## What kind of examples is it getting wrong?

#myPred <- data.frame(test$PatientGuid, rf_result)
#write.table(myPred[order(myPred$test.PatientGuid),], "sample.csv", sep=',', row.names=FALSE, quote=TRUE, col.names=FALSE)





