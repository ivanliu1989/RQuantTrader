library(doMC)
library(caret)
registerDoMC(cores = 6)
set.seed(825)

trainBC = as.data.frame(main)
trainBC$Returns = as.character(ifelse(main$Returns > 0, 'Y', 'N'))
tot.n = nrow(trainBC)
train.points = round(0.75 * tot.n)

testBC = trainBC[(train.points+1):nrow(trainBC),]
trainBC = trainBC[1:train.points,]

pre.fit = preProcess(trainBC[, -ncol(trainBC)], method = c('center', 'scale'))
trainBC[, -ncol(trainBC)] = predict(pre.fit, trainBC[, -ncol(trainBC)])
testBC[, -ncol(testBC)] = predict(pre.fit, testBC[, -ncol(testBC)])


# randomGLM 0.5285851 /
# svmLinearWeights 0.5571348
# RRF 0.5700577

# fitControl <- trainControl(
#     method = "adaptive_cv",
#     allowParallel = TRUE
#     ,summaryFunction = twoClassSummary
#     ,selectionFunction = "best"
#     ,adaptive = list(min = 5, alpha = 0.05, method = "BT", complete = TRUE)
#     # ,search = "random"
#     ,classProbs = TRUE
#     ,trim = FALSE #TRUE
# )
#
# gbmFit <- train(Returns ~ ., data = trainBC,
#                 method = "nodeHarvest",
#                 tuneLength = 4,
#                 trControl = fitControl,
#                 preProc = c("center", "scale"),
#                 verbose = TRUE
#                 ,metric = "ROC"
# )


# GBM
fitControl <- trainControl(
    method = "adaptive_cv",
    allowParallel = TRUE
    ,summaryFunction = twoClassSummary
    ,selectionFunction = "best"
    ,adaptive = list(min = 5, alpha = 0.05, method = "BT", complete = TRUE)
    # ,search = "random"
    ,classProbs = TRUE
    ,trim = FALSE #TRUE
)

gbmFit <- train(Returns ~ ., data = trainBC,
                method = "gbm",
                tuneLength = 20,
                trControl = fitControl,
                # preProc = c("center", "scale"),
                verbose = TRUE
                ,metric = "ROC"
)
gbmRes = setDT(gbmFit$results)
setorder(gbmRes, -ROC)

# RF
rfFit <- train(Returns ~ ., data = trainBC,
                method = "rf",
                tuneLength = 20,
                trControl = fitControl,
                verbose = TRUE
                ,metric = "ROC"
)
gbmRes = setDT(gbmFit$results)

# nb
# nbFit <- train(Returns ~ ., data = trainBC,
#                method = "nb",
#                tuneLength = 20,
#                trControl = fitControl,
#                verbose = TRUE
#                ,metric = "ROC"
# )

# svmLinear
# fitControl.svmL <- trainControl(
#     method = "cv",
#     allowParallel = TRUE
#     ,summaryFunction = twoClassSummary
#     ,classProbs = TRUE
# )
# svmLFit <- train(Returns ~ ., data = trainBC,
#                  method = "svmLinear",
#                  trControl = fitControl.svmL,
#                  verbose = TRUE
#                  ,metric = "ROC"
# )

# svmRadial
svmRFit <- train(Returns ~ ., data = trainBC,
                 method = "svmRadial",
                 tuneLength = 20,
                 trControl = fitControl,
                 verbose = TRUE
                 ,metric = "ROC"
)

# avNNet
avNNetFit <- train(Returns ~ ., data = trainBC,
                   method = "avNNet",
                   tuneLength = 6,
                   trControl = fitControl,
                   verbose = TRUE
                   ,metric = "ROC"
)


gbmPred = predict(gbmFit, testBC, type = 'prob')[,2]
w.caret.gbm = max(gbmFit$results$ROC)

svmRPred = predict(svmRFit, testBC, type = 'prob')[,2]
w.caret.svmR = max(svmRFit$results$ROC)

avNNetPred = predict(avNNetFit, testBC, type = 'prob')[,2]
w.caret.avNNet = max(avNNetFit$results$ROC)

rfPred = predict(rfFit, testBC, type = 'prob')[,2]
w.caret.rf = max(rfPred$results$ROC)

nbPred = predict(nbFit, testBC, type = 'prob')[,2]
w.caret.nb = max(nbFit$results$ROC)

svmLPred = predict(svmLFit, testBC, type = 'prob')[,2]
w.caret.svmL = max(svmLFit$results$ROC)



final = (gbmPred + nbPred + svmLPred + svmRPred + avNNetPred)/5
final = (gbmPred + svmRPred + avNNetPred + rfPred)/4

# Blending
table(ifelse(gbmPred>=0.5, 'Y', 'N'), testBC$Returns)
(177+114)/length(final)
plot(final, col = as.factor(testBC$Returns))


# XGboost
library(xgboost)
trainBC = as.data.frame(main)
trainBC$Returns = as.character(ifelse(main$Returns > 0, 1, 0))
tot.n = nrow(trainBC)
train.points = round(0.75 * tot.n)
testBC = trainBC[(train.points+1):nrow(trainBC),]
trainBC = trainBC[1:train.points,]

pre.fit = preProcess(trainBC[, -ncol(trainBC)], method = c('center', 'scale'))
trainBC[, -ncol(trainBC)] = predict(pre.fit, trainBC[, -ncol(trainBC)])
testBC[, -ncol(testBC)] = predict(pre.fit, testBC[, -ncol(testBC)])

dtrain <- xgb.DMatrix(data.matrix(trainBC[, -ncol(trainBC)]), label = trainBC$Returns)
dtest <- xgb.DMatrix(data.matrix(testBC[, -ncol(trainBC)]), label = testBC$Returns)
watchlist <- list(train = dtrain, eval = dtest)

param <- list(max_depth = 6,
              eta = 0.01,
              nthread = 6,
              objective = "binary:logistic",
              eval_metric = "rmse",
              booster = "gbtree",
              gamma = 0.001,
              min_child_weight = 10,
              subsample = 0.8,
              colsample_bytree = 0.2,
              maximize = FALSE
              )
bstRMSE <- xgb.train(param,
                 dtrain,
                 nrounds = 1000,
                 watchlist,
                 early_stopping_rounds = 100,
                 maximize = FALSE,
                 print_every_n = 10
)
param <- list(max_depth = 6,
              eta = 0.001,
              nthread = 6,
              objective = "binary:logistic",
              eval_metric = "auc",
              booster = "gbtree",
              gamma = 0.001,
              min_child_weight = 1,
              subsample = 0.9,
              colsample_bytree = 0.2,
              maximize = TRUE
)
bstAUC <- xgb.train(param,
                     dtrain,
                     nrounds = 1000,
                     watchlist,
                     early_stopping_rounds = 100,
                    maximize = TRUE,
                    print_every_n = 5
)
xgbAUCPred = predict(bstAUC, dtest)
xgbRMSEPred = predict(bstRMSE, dtest)

# Binary effect -----------------------------------------------------------
binaryClassifierEvaluation(gbmPred, testBC$Returns) # 0.5631 / 0.5496
binaryClassifierEvaluation(nbPred, testBC$Returns) # 0.5624 / 0.5565
binaryClassifierEvaluation(svmLPred, testBC$Returns) # 0.5279 / 0.5148
binaryClassifierEvaluation(svmRPred, testBC$Returns) # 0.5692 / 0.553
binaryClassifierEvaluation(avNNetPred, testBC$Returns) # 0.5203 / 0.4939
binaryClassifierEvaluation(xgbAUCPred, testBC$Returns) # 0.5889 / 0.5878
binaryClassifierEvaluation(xgbRMSEPred, testBC$Returns) # 0.5688 / 0.5426
final = (gbmPred + nbPred + svmRPred + xgbPred + xgbRMSEPred) / 5
binaryClassifierEvaluation(final, testBC$Returns) # 0.5695 / 0.5565








