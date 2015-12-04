# Updated the GBM codes and the required libraries
# sample_code.R
# Sample code for the Practice Fusion Diabetes Classification Competition.
# This codes provides an example of how to flatten the data set features for
# diagnoses, medications, and labs and computes a basic random forest benchmark
# for a transformed dataset with 2 diagnoses, 5 medications and 3 labs.
#
# Requires the provided SQLite database.
# Requires file sample_code_library.R

# ================================================================================= #
# Clear work space
rm(list = ls(all = TRUE)) 

# # Install Packages
# install.packages("RSQLite")
# install.packages("randomForest")
# install.packages("plyr")
# install.packages("psych")
# install.packages("ROCR")
# install.packages("doBy")
# install.packages("cvTools")
# install.packages("GGally")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("e1071")
# install.packages("pROC")
# install.packages("gbm")
# install.packages("caret")

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
library(e1071)
library(pROC)
library(gbm)
library(caret)
library(caTools)
library(PRROC)

# Set up working directory 
# # Katherine
# setwd("C:/Users/kathy/Documents/My Documents/Coursework/Fall_2015_Stats_229/Project")
# source(paste0(getwd(),"/CS-229-Project/sample_code_library.R"))
# ccs_path = ("C:/Users/kathy/Documents/My Documents/Coursework/Fall_2015_Stats_229/Project/CS-229-Project")

## Katherine 2
setwd("/E/holsteen/Kathy/CS229Project/")
source(paste0(getwd(),"/sample_code_library.R"))
ccs_path = ("/E/holsteen/Kathy/CS229Project/")


# # Haju
# setwd("C:/Users/Haju Kim/Dropbox/Stanford/2015-2016/1Q/CS 229/Project")
# source("C:/Users/Haju Kim/Dropbox/Stanford/2015-2016/1Q/CS 229/Project/sample_code_library.R")

# # Haju 2
# setwd("//afs/userhome/Desktop/CS 229/CS-229-Project-master/")
# source(paste0(getwd(),"/sample_code_library.R"))
# ccs_path = ("//afs/userhome/Desktop/CS 229/CS-229-Project-master/")

# ================================================================================= #
# Open connections
n <- dbDriver("SQLite")
con <- dbConnect(n, dbname="compData.db")
dbListTables(con) # Display the list of all data tables 

# ================================================================================= #
# Create dataset with Ndiagnosis, Nmedication, Nlab, AddTranscript
train0 <- create_flattenedDataset(con, Ndiagnosis = 50, AddMedication = 1, 
                                  AddLab = 1, AddTranscript = 1, nDrTypes = 5,
                                  AddSmoke = 1)
saveRDS(train0, "train0.rds")

# ================================================================================= #
# Read in dataset 
train0 <- readRDS("train0.rds")
## assign patient Guid to row name
rownames(train0) <- train0[,1]
# Split the data into training and test sets
set.seed(123)
train.ind = sample(1:nrow(train0), 0.75*nrow(train0))
## remove column 1 (patient guid)
df.train = train0[train.ind,2:ncol(train0)]
df.test = train0[-train.ind,2:ncol(train0)]

df.train.even = rbind(df.train[df.train$dmIndicator == 1,], 
                      df.train[sample(x=which(df.train$dmIndicator == 0),
                                      size = sum(df.train$dmIndicator == 1)),])

# Descriptive Statistics

# ================================================================================= #
# Summary Statistics
summary(df.train)

# Display P(dm = 1), P(dm = 0)
prop.table(table(df.train$dmIndicator))

# Summary of numeric variables
summary.by <- by(df.train, df.train$dmIndicator, summary)
tapply(df.train, df.train$dmIndicator, summary)
desc.stats <- describeBy(df.train, "dmIndicator",mat=TRUE)
write.csv(desc.stats, "desc.stats.csv")

image(as.matrix(df.train.even[df.train.even$dmIndicator==1, 
                              c("ct.ccs.9899", "ct.ccs.53", "ct.ccs.205")][1:100,]),
      useRaster=TRUE, xlim = range())

table(df.train.even[, c("dmIndicator", "ct.ccs.9899", "ct.ccs.53", "ct.ccs.205")])

## Pairs plot to check if there's any variable that seems highly correlated
ggpairs(df.train)        
        
## ========================================================================================
## Create overlaid density plots for key predictor variables

df.train$Age <- 2009-df.train$YearOfBirth
df.train$dmIndicator <- factor(df.train$dmIndicator, levels = c("1", "0"))
vars.to.plot <- c("Age", "BMI.med", "ct.ccs.9899","Weight.med",  "ct.Transcripts")
vars.labels <- c("Age", "BMI", "Hypertension Diagnosis Count", "Weight (lbs)", 
                 "Transcript Count")
max.x <- c(max(df.train[,"Age"]), max(df.train[,"BMI.med"]), 25, max(df.train[,"Weight.med"]),
           100))
for (i in 1:length(vars.to.plot)) {
  i = length(vars.to.plot)
  print(ggplot(df.train, aes_string(x=vars.to.plot[i])) + xlab(vars.labels[i]) +
          geom_density(aes(group=dmIndicator, colour = dmIndicator, fill=dmIndicator), alpha = 0.3) +
          theme(legend.title=element_blank(), 
                legend.text = element_text(size = 16, face = "bold"),
                axis.text.x = element_text(size = 16, face = "bold"),
                axis.title.x = element_text(size = 16, face = "bold"),
                axis.ticks = element_blank(), axis.title.y = element_blank(),
                axis.text.y = element_blank())  +
          scale_x_continuous(limits = c(0,max.x[i])) +
          scale_colour_discrete(labels=c("Diabetes +", "Diabetes -")) + 
          scale_fill_discrete(labels=c("Diabetes +", "Diabetes -") ) 
  )
}

# ================================================================================= #

## Random Forest
## important:   make sure all predictors are numeric or factors
#rf <- randomForest(x=train[obs.T,3:ncol(train)], y=train$dmIndicator[obs.T], 
#                   ntree = 500, importance = TRUE)

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

### CV may not be necessary for random forest because we can use OOB error as test error
# Loop over different folds to compute cross-validation error
# for(i in 1:k) {
#   index_start = ((i - 1) * n + 1) # starti index of the subset
#   index_end = (i * n) # end index of the subset
#   subset = index_start:index_end # range of the subset
#   
#   cv.train = df.train[-subset, ]
#   cv.test = df.train[subset, ]
#   
#   # TODO: Add bestmtry to find a better fit for the random forest
#   #bestmtry = tuneRF(cv.train[, -1], cv.train[, 1], ntreeTry=100, 
#   #                stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)
#   # Run the random forest on the train set
#   
#   fit = randomForest(x = cv.train[, -1], y = as.factor(cv.train[, 1]), ntree=ntrees, do.trace=TRUE, keep.forest=TRUE, importance=TRUE)
#     
#   # Make predictions on the test set    
#   fitted = predict(fit, newdata = cv.train[, -1], type = "prob")[, 2]
#   prediction = predict(fit, newdata = cv.test[, -1], type = "prob")[, 2]
#   
#   # Calculate the model's accuracy for the ith fold
#   err.vect.test[i] = auc(cv.test[,1], prediction)
#   err.vect.train[i] = auc(cv.train[,1], fitted)
#   
#   print(paste("Test AUC (fold ", i, "): ", err.vect.test[i]))
#   print(paste("Trainin AUC (fold ", i, "): ", err.vect.train[i]))
# }
# 
# print(paste("Avg. Test AUI: ", mean(err.vect.test)))
# print(paste("Avg. Training AUI: ", mean(err.vect.train)))

bestmtry = tuneRF(df.train.even[, -1], df.train.even[, 1], ntreeTry=500, 
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)

# Try tuning the M or the number of trees?
fit.all = randomForest(x = df.train.even[, -1], y = as.factor(df.train.even[, 1]), 
                       ntree=ntrees,  
                       do.trace=TRUE, keep.forest=TRUE, importance=TRUE)
## perhaps we don't need the ratios and the counts? does one perform better?
fit.all = randomForest(x = df.train.even[, -c(1, 66:115)], y = as.factor(df.train.even[, 1]), 
                       ntree=ntrees,  
                       do.trace=TRUE, keep.forest=TRUE, importance=TRUE)
## training ROC based on fitted values
fitted = predict(fit.all, newdata = df.train.even[, -1], type = "prob")[, 2]
roc.train <- roc(df.train.even[, 1], fitted)
#plot.roc(smooth(roc.train), main = "Training and Test ROC", lty=2)
plot.roc(roc.train, main = "Training and Test ROC", lty=2)

## test ROC on same plot based on OOB error
oob.pred = predict(fit.all, type = "prob")[, 2]
roc.test <- roc(df.train.even[, 1], oob.pred)
plot.roc(smooth(roc.test), add=TRUE, lty=1)
legend("bottomright", legend = c("Training", "Test (OOB)"), lty = c(2, 1))

## Add AUC text
(auc.train <- auc(df.train.even[,1], fitted))
(auc.test <- auc(df.train.even[,1], oob.pred))
text(x=c(.65, .4), y=c(.9, .65), label=c(paste0('Training AUC = ', round(auc.train, 2)), paste0('Test AUC = ', round(auc.test, 2))))

# variable importance plot
varImpPlot(fit.all, n.var=15, type=1, main = "Variable Importance in Random Forest")

####===============================================================================
## Test the different sample sizes to create error vs. sample size plot
####===============================================================================

obs <- seq(from = 400, to = 2800, by = 400)
method <- "svm"
error.obs <- sapply(obs, function(nobs) {
  
  obs.touse <- sample(x = 1:nrow(df.train.even), size = nobs)
  print(nobs)
  df.try <- df.train.even[obs.touse,]
  
  if (method == "rf") {
    fit.all = randomForest(x = df.try[, -1], y = as.factor(df.try[, 1]), 
                           ntree=ntrees)
    
    ## training ROC based on fitted values
    fitted = predict(fit.all, newdata = df.try[, -1], type = "prob")[, 2]
    auc.train <- as.numeric(roc(df.try[, 1], fitted)$auc)

    oob.pred = predict(fit.all, type = "prob")[, 2]
    auc.test <- as.numeric(roc(df.try[, 1], oob.pred)$auc)
    
    allnew.pred <- predict(fit.all, newdata = df.test[, -1], 
                           type = "response")
    allnew.error <- sum(df.test[, 1] != allnew.pred)/nrow(df.test)
    
    return(c(auc.train, auc.test, fit.all$err.rate[ntrees], allnew.error))
  } else if (method == "svm"){    
    
    svm.fit <- svm(dmIndicator ~., data = df.try, 
                   kernel = "linear", cost = 1, cross = 5)
    train.error <- sum(svm.fit$fitted != df.try$dmIndicator)/nrow(df.try)
    test.error <- 1 - (svm.fit$tot.accuracy)/100
    return(c(train.error, test.error))
  }
}
)

plot(obs, error.obs[1,], pch = 1, ylim = range(0, .4), 
     main = "Error vs. Sample Size\n SVM with Linear Kernel",
     ylab = "Classification Error", xlab = "Sample Size")
points(obs, error.obs[2,], pch = 2, col = "red")
legend("bottomright", legend = c("Training Error", "Test (CV) Error"), 
       pch = c(1,2), col = c("black", "red"))


## ==========================================GBM==================================
# Preprocess the data to fit into the GBM model
train = df.train.even
dmIndicator = train$dmIndicator
train = subset(train, select = c(-dmIndicator))

#parameters
GBM_ITERATIONS = 2500
GBM_LEARNING_RATE = 0.01
GBM_DEPTH = 10
GBM_MINOBS = 10

# Construct the Model 
set.seed(123)

model = gbm.fit(x=train
                , y = as.vector(dmIndicator)
                , distribution = "bernoulli"
                , n.trees = GBM_ITERATIONS
                , shrinkage = GBM_LEARNING_RATE # Set it to a low value for computation
                , interaction.depth = GBM_DEPTH # Not too large to prevent overfitting
                , n.minobsinnode = GBM_MINOBS # Not too small to prevent overfitting
                , nTrain = round(nrow(train), 0.8)
                , verbose = TRUE
)

# Relative influence among the variables can be used in varaible selection
summary(model)
# If one variable is more important than all of the rest, this may be a evidence of overfitting


# Optimal number of trees based upon OOB
ntrees_optimal = gbm.perf(model, method = "OOB",  oobag.curve = TRUE)

# Predictions (In-sample)
fitted = predict(model, newdata = train, n.trees = ntrees_optimal, type="link")
pROC::auc(dmIndicator, fitted)
# Area under the curve: 0.9052

# Predictions (Out-of-sample)
prediction = predict(model, newdata = df.test[,-1], n.trees = ntrees_optimal, type="link")
pROC::auc(df.test[,1], prediction)
# Area under the curve: 0.843


# Confusion matrix
confusion = function(prediction, actual){
  tbl = table(prediction > 0, actual)
  mis = 1 - sum(diag(tbl))/sum(tbl)
  list(table = tbl, misclass.prob = mis)
}


confusion(fitted, dmIndicator)
# $misclass.prob
# [1] 0.1799148


confusion(prediction, df.test[,1])
# $misclass.prob
# [1] 0.2649779

# varImp_gbm = data.frame(summary(model))
# barplot(varImp_gbm$var, varImp_gbm$rel.inf)
# write.csv(varImp_gbm, "varImp_gbm.csv")

####===================================================================
### USE CARET TO TUNE AND GENERATE MODELS ###########################
####===================================================================

train = df.train.even
train$dmIndicator = make.names(train$dmIndicator)

### A) FULL FEATURE SET ===============================================
########## RANDOM FOREST ==============================================

# Tune parameters (#10-fold cv)
rfControl = trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)
# Specify candidate models (Tune parameters)
rfGrid =  expand.grid(mtry = c(4, 6, 8, 10, 12, 14))
nrow(rfGrid)
set.seed(1995)
rf_model = train(dmIndicator ~ ., data = train,
                 method = "rf",
                 ntree = 500,
                 trControl = rfControl,
                 do.trace = TRUE,
                 tuneGrid = rfGrid,
                 metric = "ROC")
saveRDS(rf_model, "rf_model_full.RDS")

########## GBM ========================================================

# Tune parameters (#10-fold cv)
fitControl = trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)
# Specify candidate models (Tune parameters)
gbmGrid =  expand.grid(interaction.depth = c(1, 5, 10, 15),
                        n.trees = (1:30)*50,
                        shrinkage = 0.01,
                        n.minobsinnode = 10)
nrow(gbmGrid)
set.seed(1105)
gbm_model = train(dmIndicator ~ ., data = train,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = TRUE,
                 tuneGrid = gbmGrid,
                 metric = "ROC")
saveRDS(gbm_model, "gbm_model_full.RDS")

######## SVM =====================================================

## 1.  Linear Kernel
svm.fitControl = trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)
svmLinearGrid =  expand.grid(C = c(0.001, 0.01, 0.1, 1, 5, 10, 100))
svmLinearTune <- train(dmIndicator ~ ., 
                       data = train,
                       method = "svmLinear",
                       verbose = TRUE,
                       tuneGrid = svmLinearGrid,
                       metric = "ROC",
                       trControl = svm.fitControl)
saveRDS(svmLinearTune, "svmLinearTune.rds")

## 2.  Polynomial Kernel
svmPolyGrid =  expand.grid( degree = c(2, 3), 
                            scale = c(0.001, 0.01, 0.1, 1, 10, 100), 
                            C = c(0.001, 0.01, 0.1, 1, 10, 100))
svmPolyTune <- train(dmIndicator ~ ., data = train, 
                     method = "svmPoly",
                     verbose = TRUE,
                     tuneGrid = svmPolyGrid,
                     metric = "ROC",
                     trControl = svm.fitControl)
saveRDS(svmPolyTune, "svmPolyTune.rds")

#########===================================================================
## GET VARIABLE IMPORTANCE 
#########===================================================================

rf_model <- readRDS("rf_model_full.RDS")
gbm_model <- readRDS("gbm_model_full.RDS")
svmLinear <- readRDS("svmLinearTune.rds")
svmPoly <- readRDS("svmPolyTune.rds")

# Generate a plot that summarizes the result for using different parameters
ggplot(rf_model, metric='ROC')
ggplot(gbm_model, metric='ROC')
ggplot(svmLinear, metric = "ROC")
ggplot(svmPoly, metric = "ROC")

## Plot and save variable importance

rfImp = varImp(rf_model, scale = TRUE)
ggplot(rfImp, top = 20) + geom_bar(stat = "identity", fill = "forestgreen") +
  ggtitle("Variable Importance:  Random Forest") 
rfVarImp = data.frame(rfImp$importance) 
rfVarImp = rfVarImp[order(-rfVarImp$Overall), ,drop = FALSE]
write.csv(rfVarImp, "rfVarImp.csv")
rfVarImp$varnames <- rownames(rfVarImp)
rfVarImp$rank.rf <- 1:nrow(rfVarImp)
colnames(rfVarImp)[1] <- "imp.rf"

gbmImp = varImp(gbm_model, scale = TRUE)
ggplot(gbmImp, top = 20) + geom_bar(stat = "identity", fill = "deepskyblue4") +
  ggtitle("Boosted Trees")
gbmVarImp = data.frame(gbmImp$importance) 
gbmVarImp = gbmVarImp[order(-gbmVarImp$Overall), ,drop = FALSE]
write.csv(gbmVarImp, "gbmVarImp.csv")
gbmVarImp$varnames <- rownames(gbmVarImp)
gbmVarImp$rank.gbm <- 1:nrow(gbmVarImp)
colnames(gbmVarImp)[1] <- "imp.gbm"

svmLinearImp = varImp(svmLinear, scale = TRUE)
ggplot(svmLinearImp, top = 20) + geom_bar(stat = "identity", fill = "tomato3") +
  ggtitle("SVM with Linear Kernel")
svmLinearVarImp = data.frame(svmLinearImp$importance)
svmLinearVarImp = svmLinearVarImp[order(-svmLinearVarImp[,1]),]
write.csv(svmLinearVarImp, "svmLinearImp.csv")
svmLinearVarImp = subset(svmLinearVarImp, select = c(-2))
svmLinearVarImp$varnames <- rownames(svmLinearVarImp)
svmLinearVarImp$rank.svm <- 1:nrow(svmLinearVarImp)
colnames(svmLinearVarImp)[1] <- "imp.svm"

## same as linear..
svmPolyImp = varImp(svmPoly, scale = TRUE)
ggplot(svmPolyImp, top = 20) + ggtitle("Variable Importance:  Polynomial SVM")
svmPolyImp = data.frame(svmPolyImp$importance)
svmPolyImp = svmPolyImp[order(-svmPolyImp[,1]),]
write.csv(svmPolyImp, "svmPolyImp.csv")

## Combine variable importance metrics, calculate average ranking
totalVarImp <- merge(rfVarImp, gbmVarImp, all = TRUE, by = "varnames")
totalVarImp <- merge(totalVarImp, svmLinearVarImp, all = TRUE, by = "varnames")
totalVarImp$avgRank <- rowSums(totalVarImp[, c("rank.svm", "rank.rf", "rank.gbm")])/
  rowSums(!is.na(totalVarImp[, c("rank.svm", "rank.rf", "rank.gbm")]))
totalVarImp <- totalVarImp[order(totalVarImp$avgRank),]
write.csv(totalVarImp, "totalVarImp.csv")

#######==================================================
### B) REDUCED FEATURE SET for each model based on variable importance

# Random Forest
set.seed(1776)
rf_model15 = train(dmIndicator ~ ., 
                 data = train[, c("dmIndicator", rownames(rfVarImp)[1:15])],
                 method = "rf",
                 ntree = 500,
                 trControl = rfControl,
                 do.trace = TRUE,
                 tuneGrid = rfGrid,
                 metric = "ROC")
saveRDS(rf_model15, "rf_model15.RDS")

# GBM
set.seed(3545)
gbm_model20 = train(dmIndicator ~ ., 
                    data = train[, c("dmIndicator", rownames(gbmVarImp)[1:20])],
                    method = "gbm",
                    trControl = fitControl,
                    verbose = TRUE,
                    tuneGrid = gbmGrid,
                    metric = "ROC")
saveRDS(gbm_model20, "gbm_model20.rds")

# Linear Kernel
set.seed(987)
svmLinearTune11 <- train(dmIndicator ~ ., 
                       data = train[, c("dmIndicator", 
                                        rownames(svmLinearImp)[1:11])],
                       method = "svmLinear",
                       verbose = TRUE,
                       tuneGrid = svmLinearGrid,
                       metric = "ROC",
                       trControl = svm.fitControl)
saveRDS(svmLinearTune11, "svmLinearTune11.rds")

##  Polynomial Kernel
## Can't get this code to complete .... 
# set.seed(725)
# svmPolyTune11 <- train(dmIndicator ~ ., 
#                        data = train[, c("dmIndicator", 
#                                         rownames(svmPolyImp)[1:11])], 
#                      method = "svmPoly",
#                      verbose = TRUE,
#                      tuneGrid = svmPolyGrid,
#                      metric = "ROC",
#                      trControl = svm.fitControl)
# saveRDS(svmPolyTune11, "svmPolyTune11.rds")

###==========================================================================
##### EVALUATE MODELS ###############################
###==========================================================================

## A)  Full Feature Set =====================================================

# Test set: Evaluate the model on the test set
pred.rf = predict(rf_model, newdata = df.test, type = "prob")[,2]
pred.svmLinear = predict(svmLinear, newdata = df.test, type = "prob")[,2]
pred.svmPoly = predict(svmPoly, newdata = df.test, type = "prob")[,2]
pred.gbm = predict(gbm_model, newdata = df.test, type = "prob")[,2]

## Plot ROC (output AUC)
roc.test.svmLinear <- roc(df.test[, 1], pred.svmLinear)
plot.roc(roc.test.svmLinear, main = "ROC:  Full Feature Set", lty=3,  lwd = 3,col = "tomato3")
# Area under the curve: 0.8113
roc.test.svmPoly <- roc(df.test[, 1], pred.svmPoly)
plot.roc(roc.test.svmPoly, add = TRUE, lty = 4, lwd = 3, col = "darkorchid3")
# Area under the curve: 0.8187
roc.test.gbm <- roc(df.test[, 1], pred.gbm)
plot.roc(roc.test.gbm, add = TRUE, lty = 1,  lwd = 3,col = "deepskyblue4")
#Area under the curve: 0.8495
roc.test.rf <- roc(df.test[, 1], pred.rf)
plot.roc(roc.test.rf, add = TRUE, lty = 2, lwd = 3, col = "forestgreen")
# Area under the curve: 0.8359
legend("bottomright", 
       legend = c("Boosted Trees (AUC = .850)", 
                                 "Random Forest (AUC = .836)",
                                 "SVM Polynomial (AUC = .819)", 
                                 "SVM Linear (AUC = .811)"),
       lty = c(1, 2, 4, 3), lwd = 3, cex = 0.75,
       col = c("deepskyblue4", "forestgreen", "darkorchid3", "tomato3"))

## Plot PRC (output AUC)
(pr.svmLinear<-pr.curve(scores.class0 = pred.svmLinear[df.test$dmIndicator == 0], 
             scores.class1 = pred.svmLinear[df.test$dmIndicator == 1], 
             curve = TRUE))
plot(pr.svmLinear, auc.main = FALSE, 
     main = "PR Curve:  Full Feature Set", lty = 3, col = "tomato3", lwd = 3)
#Area under curve (Integral):
#  0.6597257 
(pr.svmPoly<-pr.curve(scores.class0 = pred.svmPoly[df.test$dmIndicator == 0], 
                     scores.class1 = pred.svmPoly[df.test$dmIndicator == 1], 
                       curve = TRUE))
plot(pr.svmPoly, add = TRUE, col = "darkorchid3", lwd = 3, lty = 4)
#Area under curve (Integral):
#  0.6556906
(pr.gbm<-pr.curve(scores.class0 = pred.gbm[df.test$dmIndicator == 0], 
                 scores.class1 = pred.gbm[df.test$dmIndicator == 1], 
                       curve = TRUE))
plot(pr.gbm, add = TRUE, col = "deepskyblue4", lty = 1, lwd = 3)
#Area under curve (Integral):
#  0.643346 
pr.rf<-pr.curve(scores.class0 = pred.rf[df.test$dmIndicator == 0], 
                 scores.class1 = pred.rf[df.test$dmIndicator == 1], 
                 curve = TRUE)
plot(pr.rf, add = TRUE, col = "forestgreen", lty = 2, lwd = 3)
#   Area under curve (Integral):
# 0.6487595
legend("bottomright", 
       legend = c("Boosted Trees (AUC = .643)", 
                  "Random Forest (AUC = .649)",
                  "SVM Polynomial (AUC = .656)", 
                  "SVM Linear (AUC = .660)"),
       lty = c(1, 2, 4, 3), lwd = 3, cex = 0.75,
       col = c("deepskyblue4", "forestgreen", "darkorchid3", "tomato3"))

##===========================================================

## B)  Reduced Feature Sets =================================

rf_model15 <- readRDS("rf_model15.RDS")
svmLinear11 <- readRDS("svmLinearTune11.rds")
gbm_model20 <- readRDS("gbm_model20.rds")

# Test set: Evaluate the model on the test set
pred.rfR = predict(rf_model15, newdata = df.test, type = "prob")[,2]
pred.svmLinearR = predict(svmLinear11, newdata = df.test, type = "prob")[,2]
#pred.svmPoly = predict(svmPoly, newdata = df.test, type = "prob")[,2]
pred.gbmR = predict(gbm_model20, newdata = df.test, type = "prob")[,2]

## Plot ROC (output AUC)
roc.test.svmLinearR <- roc(df.test[, 1], pred.svmLinearR)
plot.roc(roc.test.svmLinearR, main = "ROC:  Reduced Feature Set", lty=3,
         lwd = 3, col = "tomato3")
# Area under the curve: 0.7967
#roc.test.svmPoly <- roc(df.test[, 1], pred.svmPoly)
#plot.roc(roc.test.svmPoly, add = TRUE, lty = 2)
roc.test.gbmR <- roc(df.test[, 1], pred.gbmR)
plot.roc(roc.test.gbmR, add = TRUE, lty = 1, lwd = 3, col = "deepskyblue4")
#Area under the curve: 0.8366
roc.test.rfR <- roc(df.test[, 1], pred.rfR)
plot.roc(roc.test.rfR, add = TRUE, lty = 2, lwd = 3, col = "forestgreen")
# Area under the curve:  0.8189
legend("bottomright", 
       legend = c("Boosted Trees (AUC = .837)", 
                  "Random Forest (AUC = .819)",
                  "SVM Linear (AUC = .797)"),
       lty = c(1, 2, 3), lwd = 3, cex = 0.75,
       col = c("deepskyblue4", "forestgreen", "tomato3"))

## Plot PRC (output AUC)
## PRC AUCs appear to be slightly higher for reduced than for full feature sets?

(pr.svmLinearR<-pr.curve(scores.class0 = pred.svmLinearR[df.test$dmIndicator == 0], 
                        scores.class1 = pred.svmLinearR[df.test$dmIndicator == 1], 
                        curve = TRUE))
plot(pr.svmLinearR, auc.main = FALSE, 
     main = "PR Curve:  Reduced Feature Set", lty = 3, col = "tomato3", lwd = 3)
#Area under curve (Integral):
#  0.6665106 
#(pr.svmPoly<-pr.curve(scores.class0 = pred.svmPoly[df.test$dmIndicator == 0], 
#                      scores.class1 = pred.svmPoly[df.test$dmIndicator == 1], 
#                      curve = TRUE))
#plot(pr.svmPoly, add = TRUE, lty = 3, col = "red")

(pr.gbmR<-pr.curve(scores.class0 = pred.gbmR[df.test$dmIndicator == 0], 
                  scores.class1 = pred.gbmR[df.test$dmIndicator == 1], 
                  curve = TRUE))
plot(pr.gbmR, add = TRUE, col = "deepskyblue4", lwd = 3, lty = 1)
#    Area under curve (Integral):
#  0.6488368 
pr.rfR<-pr.curve(scores.class0 = pred.rfR[df.test$dmIndicator == 0], 
                scores.class1 = pred.rfR[df.test$dmIndicator == 1], 
                curve = TRUE)
plot(pr.rfR, add = TRUE, col = "forestgreen", lwd = 3, lty = 2)
#   Area under curve (Integral):
#   0.6560269 
legend("bottomright", 
       legend = c("Boosted Trees (AUC = .649)", 
                  "Random Forest (AUC = .656)",
                  "SVM Linear    (AUC = .667)"),
       lty = c(1, 2, 3), lwd = 3, cex = 0.75,
       col = c("deepskyblue4", "forestgreen", "tomato3"))

