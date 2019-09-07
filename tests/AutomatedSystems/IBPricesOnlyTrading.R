library(caret)
library(xgboost)
library(quantmod)
source('tests/AutomatedSystems/IBPaperTradingIndicators.R')

AUDUSD = prepareForexData(ib.duration = "6 Y", ib.barsize = "1 day", Cur1 = "AUD", Cur2 = "USD",
                          oanda.granularity = 'D', QuandlSymbol1 = 'AUS', QuandlSymbol2 = 'USA', ibAcc = 'paper')

AUDUSD$OA.MID = (AUDUSD$OA.BIDASK[, 1:4] + AUDUSD$OA.BIDASK[, 5:8]) / 2

# Feature Creation --------------------------------------------------------
# 1. HLC
# 2. Vol
# 3. Spreads
# 4. Consecu wins
finIndicators = finIndicatorHLC(AUDUSD$OA.MID[, c(2, 3, 1)])
finIndicators = finIndicatorHLCVol(AUDUSD$OA.MID[, c(2, 3, 1)], AUDUSD$OA.VOLUME)
target <- lag(diff(Cl(AUDUSD$OA.MID)), -1)
colnames(target) = 'target'
all = na.omit(merge(finIndicators, target))
# all$Ret2 = lag(all$target, 1)
all = na.omit(all)

# Walk-forward Time Slices ------------------------------------------------
all.points = nrow(all) - 200
train.points = round(all.points * 0.9)
test.points = all.points - train.points

test = all[(nrow(all)-200):nrow(all), ]
training = all[1:(nrow(all)-201), ]
# timeSlices = createTimeSlices(y = 1:nrow(train), initialWindow = train.points, horizon = test.points, fixedWindow = TRUE)
# timeSlices.idx = seq(from = 1, to = 201, by = 20)
# training = all

# Centering and Scaling
# preProcValues <- preProcess(as.data.frame(training[, -ncol(training)]), method = c("center", "scale")) # pca
preProcValues <- preProcess(as.data.frame(training[, -ncol(training)]), method = c("scale"))
trainBC <- predict(preProcValues, as.data.frame(training))

# Model Tuning and Training
trainBC$target = ifelse(trainBC$target >= 0, 'Y', 'N')
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

library(doMC)
registerDoMC(cores = 6)
set.seed(825)
mlFit1 <- train(target ~ ., data = trainBC,
                method = "svmRadial",
                trControl = fitControl,
                verbose = TRUE
                # ,metric = "ROC"
)
plot(mlFit1)
mlFit1

### Classifications
# xgbTree 0.5316521
# svmLinear 0.4905644 0.5518528
# svmRadial 0.5007291 0.5419883 0.5528821
# gbm 0.5199434 0.5330674
# elm 0.5232887 0.5252187
# avNNet 0.5102076 0.5587579 0.5622275 0.5855594 | 0.5626492(vol single)
# xgbLinear
# randomGLM
# glmnet_h2o
# nb (need to blend kernel) 0.5728255(hlc) | 0.5802453/0.6064218(hlc+ret) | 0.6115269(Vol)



fitControl <- trainControl(
    method = "timeslice",
    initialWindow = train.points,
    horizon = test.points,
    fixedWindow = TRUE,
    skip = 100,
    allowParallel = TRUE)

# library(doMC)
# registerDoMC(cores = 6)
set.seed(825)
mlFit1 <- train(as.numeric(target) ~ ., data = trainBC,
                method = "xgbLinear",
                trControl = fitControl,
                verbose = TRUE)
plot(mlFit1)
mlFit1
### Regression
# xgbLinear
# xgbTree
# avNNet
# gbm_h2o 0.01511858
# glmnet_h2o
# rvmLinear 0.03740246
# rvmRadial 0.01511858
# ridge 0.0492743
# gbm
# lasso 0.0492743
