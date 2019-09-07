rm(list = ls()); gc()
library(caret)
library(xgboost)
library(quantmod)
source('tests/AutomatedSystems/IBPaperTradingIndicators.R')

# AUDUSD = prepareForexData(ib.duration = "6 Y", ib.barsize = "1 day", Cur1 = "AUD", Cur2 = "USD",
#                           oanda.granularity = 'D', QuandlSymbol1 = 'AUS', QuandlSymbol2 = 'USA', ibAcc = 'live')
load('data/SampleTradingDatasetFX.RData')
AUDUSD$OA.MID = (AUDUSD$OA.BIDASK[, 1:4] + AUDUSD$OA.BIDASK[, 5:8]) / 2

# Feature Creation --------------------------------------------------------
finIndicators = finIndicatorHLCVol(AUDUSD$OA.MID[, c(2, 3, 1)], AUDUSD$OA.VOLUME)
target <- lag(diff(Cl(AUDUSD$OA.MID)), -1)
colnames(target) = 'target'
all = na.omit(merge(finIndicators, target))
all = na.omit(all)

# Walk-forward Time Slices ------------------------------------------------
test = all[(nrow(all)-100):nrow(all), ]
training = all[1:(nrow(all)-101), ]

preProcValues <- preProcess(as.data.frame(training[, -ncol(training)]), method = c("scale"))
trainBC <- predict(preProcValues, as.data.frame(training))
testBC <- predict(preProcValues, as.data.frame(test))
all.points = nrow(trainBC) - 200
train.points = round(all.points * 0.9)
test.points = all.points - train.points

# Model Tuning and Training -----------------------------------------------
trainBC$target = ifelse(trainBC$target >= 0, 'Y', 'N')
testBC$target = ifelse(testBC$target >= 0, 'Y', 'N')
library(doMC)
registerDoMC(cores = 6)
set.seed(825)

# GBM
fitControl <- trainControl(
    method = "timeslice",
    initialWindow = train.points,
    horizon = test.points,
    fixedWindow = TRUE,
    skip = 100,
    allowParallel = TRUE
    ,summaryFunction = twoClassSummary
    ,classProbs = TRUE
)

gbmFit <- train(target ~ ., data = trainBC,
                method = "gbm",
                trControl = fitControl,
                verbose = TRUE
                ,metric = "ROC"
)

# nb
fitControl <- trainControl(
    method = "timeslice",
    initialWindow = train.points,
    horizon = test.points,
    fixedWindow = TRUE,
    skip = 100,
    allowParallel = TRUE
    ,summaryFunction = twoClassSummary
    ,classProbs = TRUE
)

nbFit <- train(target ~ ., data = trainBC,
                method = "nb",
                trControl = fitControl,
                verbose = TRUE
                ,metric = "ROC"
)

# svmLinear
fitControl <- trainControl(
    method = "timeslice",
    initialWindow = train.points,
    horizon = test.points,
    fixedWindow = TRUE,
    skip = 20,
    allowParallel = TRUE
    ,summaryFunction = twoClassSummary
    ,classProbs = TRUE
)

svmLFit <- train(target ~ ., data = trainBC,
               method = "svmLinear",
               trControl = fitControl,
               verbose = TRUE
               ,metric = "ROC"
)

# svmRadial
fitControl <- trainControl(
    method = "timeslice",
    initialWindow = train.points,
    horizon = test.points,
    fixedWindow = TRUE,
    skip = 20,
    allowParallel = TRUE
    ,summaryFunction = twoClassSummary
    ,classProbs = TRUE
)

svmRFit <- train(target ~ ., data = trainBC,
                      method = "svmRadial",
                      trControl = fitControl,
                      verbose = TRUE
                      ,metric = "ROC"
)

# avNNet
fitControl <- trainControl(
    method = "timeslice",
    initialWindow = train.points,
    horizon = test.points,
    fixedWindow = TRUE,
    skip = 20,
    allowParallel = TRUE
    ,summaryFunction = twoClassSummary
    ,classProbs = TRUE
)

avNNetFit <- train(target ~ ., data = trainBC,
                method = "avNNet",
                trControl = fitControl,
                verbose = TRUE
                ,metric = "ROC"
)

# rf
# fitControl <- trainControl(
#     method = "timeslice",
#     initialWindow = train.points,
#     horizon = test.points,
#     fixedWindow = TRUE,
#     skip = 100,
#     allowParallel = TRUE
#     ,summaryFunction = twoClassSummary
#     ,classProbs = TRUE
# )
#
# rfFit <- train(target ~ ., data = trainBC,
#                       method = "rf",
#                       trControl = fitControl,
#                       verbose = TRUE
#                       ,metric = "ROC"
# )


gbmPred = predict(gbmFit, testBC, type = 'prob')
nbPred = predict(nbFit, testBC, type = 'prob')
svmLPred = predict(svmLFit, testBC, type = 'prob')
svmRPred = predict(svmRFit, testBC, type = 'prob')
avNNetPred = predict(avNNetFit, testBC, type = 'prob')
rfPred = predict(rfFit, testBC, type = 'prob')

# 0.5186359, 0.5277479, 0.5023675, 0.5155662, 0.5222925, 0.5186826
pred = (gbmPred$Y + nbPred$Y + svmLPred$Y + svmRPred$Y + avNNetPred$Y)/5
pred = ifelse(pred >= 0.5, 'Y', 'N')
mean(pred == testBC$target) #56.436% (ROC) | 58.41% (accuracy)


#xgboost
trainBC$target = ifelse(trainBC$target == 'Y', 1, 0)
testBC$target = ifelse(testBC$target == 'Y', 1, 0)

dval          <- xgb.DMatrix(data=as.matrix(testBC[,-ncol(testBC)]),label=testBC$target)
dtrain        <- xgb.DMatrix(data=as.matrix(trainBC[,-ncol(trainBC)]),label=trainBC$target)
watchlist     <- list(val=dval,train=dtrain)
xgbTFit <- xgb.train(data                = dtrain,
                     nrounds             = 1000,
                     early.stop.round    = 20,
                     watchlist           = watchlist,
                     eval_metric         = 'auc',
                     # maximize            = TRUE,
                     objective           = "binary:logistic",
                     booster             = "gbtree", # gblinear
                     eta                 = 0.05,
                     max_depth           = 3,
                     min_child_weight    = 10,
                     subsample           = 1,
                     colsample           = 1,
                     print.every.n       = 10,
                     gamma               = 0.05
)
xgbTPred <- predict(xgbTFit, dval, ntreelimit = xgbTFit$best_iteration)
mean(ifelse(xgbTPred >= 0.5, 1, 0) == testBC$target)

pred = (gbmPred$Y + nbPred$Y + svmLPred$Y + svmRPred$Y + avNNetPred$Y + xgbTPred)/6
pred = ifelse(pred >= 0.5, 1, 0)
mean(pred == testBC$target) #53.465%
