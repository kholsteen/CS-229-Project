
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
library(reshape2)

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
train0 <- create_flattenedDataset(con, "training", Ndiagnosis = 25, AddMedication = 1, 
                                  Nlab = 0, AddTranscript = 1, nDrTypes = 25)
## assign patient Guid to row name
rownames(train0) <- train0[,1]

# ================================================================================= #
# Split the data into training and test sets
set.seed(123)
train.ind = sample(1:nrow(train0), 0.75*nrow(train0))
## remove column 1 (patient guid)
df.train = train0[train.ind,2:ncol(train0)]
df.test = train0[-train.ind,2:ncol(train0)]

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
#                   ntree = 500, importance = TRUE)


# CROSS VALIDATION ===========================================================================
## Wrap all of this in cross-validation to produce AUC

# ============ INSTALLATON =========================#
install.packages("pROC")

# ============= LIBRARIES =========================#
library(pROC)

#====================== CV for Random Forest=========================#
# ASSUMPTION: COLUMN1: BINARY OUTCOME VARIABLE; REST: FEATURE MATRIX

# Set Variable name


# Set the number of folds (k)
k = 10 
ntrees = 500
# Initialize
n = floor(nrow(df.train)/k)
err.vect.test = rep(NA, k)
err.vect.train = rep(NA, k)

## randomize 

# Loop over different folds to compute cross-validation error
for(i in 1:k) {
  index_start = ((i - 1) * n + 1) # starti index of the subset
  index_end = (i * n) # end index of the subset
  subset = index_start:index_end # range of the subset
  
  cv.train = df.train[-subset, ]
  cv.test = df.train[subset, ]
  
  # TODO: Add bestmtry to find a better fit for the random forest
  #bestmtry = tuneRF(cv.train[, -1], cv.train[, 1], ntreeTry=100, 
  #                stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)
  # Run the random forest on the train set
  
  fit = randomForest(x = cv.train[, -1], y = as.factor(cv.train[, 1]), ntree=ntrees, do.trace=TRUE, keep.forest=TRUE, importance=TRUE)
    
  # Make predictions on the test set    
  fitted = predict(fit, newdata = cv.train[, -1], type = "prob")[, 2]
  prediction = predict(fit, newdata = cv.test[, -1], type = "prob")[, 2]
  
  # Calculate the model's accuracy for the ith fold
  err.vect.test[i] = auc(cv.test[,1], prediction)
  err.vect.train[i] = auc(cv.train[,1], fitted)
  
  print(paste("Test AUC (fold ", i, "): ", err.vect.test[i]))
  print(paste("Trainin AUC (fold ", i, "): ", err.vect.train[i]))
}

print(paste("Avg. Test AUI: ", mean(err.vect.test)))
print(paste("Avg. Training AUI: ", mean(err.vect.train)))

fit.all = randomForest(x = df.train[, -1], y = as.factor(df.train[, 1]), ntree=100, 
                       do.trace=TRUE, keep.forest=TRUE, importance=TRUE)
## training ROC
fitted = predict(fit.all, newdata = df.train[, -1], type = "prob")[, 2]
roc.train <- roc(df.train[, 1], fitted)
plot.roc(smooth(roc.train), main = "Training and Test ROC", lty=2)

## test ROC on same plot
oob.pred = predict(fit.all, type = "prob")[, 2]
roc.test <- roc(df.train[, 1], oob.pred)
plot.roc(smooth(roc.test), add=TRUE, lty=1)
legend("bottomright", legend = c("Training", "Test"), lty = c(2, 1))

# variable importance plot
varImpPlot(fit.all, n.var=20, type=1, main = "Variable Importance in Random Forest")


# Out-of-Sample Test Error ==============================================================
# Compute the prediction loss from the fitted values
#mspe(train$Y, predict(model), includeSE = TRUE)


## Boosting?
## SVM?

## Error analysis
## Confusion matrix
fit.all$confusion

## What kind of examples is it getting wrong?

#myPred <- data.frame(test$PatientGuid, rf_result)
#write.table(myPred[order(myPred$test.PatientGuid),], "sample.csv", sep=',', row.names=FALSE, quote=TRUE, col.names=FALSE)





