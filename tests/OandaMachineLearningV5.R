# Vn := voladaily := StDev({d1, ..., dn})
# Rn := en/e0
# minimize the mean squared error, i.e., minimize the quantities X i X k (R ∗ (xik) − Rn(i, k))2 , and X i X k (V ∗ (xik) − Vn(i, k))2 respectively.
# predicted returns by a power of the predicted volatility
rm(list = ls());
gc()

Cur1 = 'AUD'
Cur2 = 'USD'
nxt.n = 3

loadQuantPackages()

RetModelPath = paste0('~/Common/audcad_intraday_m15/model/ReturnModel_', Cur1, Cur2, '_', round(as.numeric(Sys.time())), '.RData')
DataPath = paste0('~/Common/audcad_intraday_m15/model/Data_', Cur1, Cur2, '_', round(as.numeric(Sys.time())), '.RData')

# 0. Get Prices -----------------------------------------------------------
PRICE.OA = prepareForexOandaPrices(oanda.count = 550, Cur1 = Cur1, Cur2 = Cur2, oanda.granularity = 'M15')
PRICE.IB = prepareIBForexPrices(ib.duration = "1 Y", ib.barsize = "15 mins", Cur1 = Cur1, Cur2 = Cur2, ibAcc = "paper", midOnly = TRUE)
price.oa = PRICE.OA$OA.MID
price.ib = PRICE.IB$IB.MID
# save(price.ib, price.oa, file = DataPath)
# load('~/Common/audcad_intraday_m15/model/Data_AUDUSD_1487485108.RData')

# 1. Objective Variable ---------------------------------------------------
oa.ret = lag(ROC(Cl(price.oa), n = nxt.n, type = 'discrete'), -nxt.n); names(oa.ret) = 'target'
ib.ret = lag(ROC(Cl(price.ib), n = nxt.n, type = 'discrete'), -nxt.n); names(ib.ret) = 'target'

# 2. Feature Engineering --------------------------------------------------
feats.ib =  na.omit(merge(finIndicatorHLC(price.ib),generateCandleStickPatterns(price.ib))) # 200
# feats.ib =  na.omit(generateCandleStickPatterns(price.ib)) # 200
feats.oa = na.omit(merge(finIndicatorHLC(price.oa), generateCandleStickPatterns(price.oa))) # 200
# feats.oa =  na.omit(generateCandleStickPatterns(price.oa)) # 200

ret.dt.ib = merge(feats.ib, ib.ret); ret.dt.ib = na.omit(ret.dt.ib)
ret.dt.oa = merge(feats.oa, oa.ret); ret.dt.oa = na.omit(ret.dt.oa)


# 3. Splitting data sets --------------------------------------------------
trainBC = as.data.frame(ret.dt.ib[1:(nrow(ret.dt.ib)-nrow(ret.dt.oa)),])
# trainBC = as.data.frame(ret.dt.ib)
trainBC$target = ifelse(trainBC$target >=0, 1, 0)
testBC = as.data.frame(ret.dt.oa)
testBC$target = ifelse(testBC$target >=0, 1, 0)

# trainBC = testBC
predictors = names(trainBC)[!names(trainBC) %in% c('target')]
response = 'target'


library(xgboost)
set.seed(888)
dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
dtest <- xgb.DMatrix(data.matrix(testBC[, predictors]), label = testBC[, response])
watchlist <- list(train = dtrain, eval = dtest)

param <- list(max_depth = 6,
              eta = 0.01,
              nthread = 6,
              objective = "binary:logistic",
              eval_metric = "rmse",
              booster = "gbtree",
              gamma = 0.0001,
              min_child_weight = 5,
              subsample = 0.7,
              colsample_bytree = 0.05
)

xgbFit <- xgb.train(param,
                    dtrain,
                    nrounds = 5000,
                    watchlist,
                    early_stopping_rounds = 300,
                    # maximize = TRUE,
                    print_every_n = 10
)

xgb.pred = predict(xgbFit, dtest, ntreelimit = xgbFit$best_iteration)
binaryClassifierEvaluation(xgb.pred, testBC$target)
# 0.5426 / 0.5469
# 0.5335 / 0.5504
# 0.5422 / 0.5514
# 0.546 / 0.58744 AUC
# 0.5632 / 0.5783 rmse



importanceRaw <- xgb.importance(feature_names = predictors, model = xgbFit)
xgb.plot.importance(importance_matrix = importanceRaw)





searchGridSubCol <- expand.grid(subsample = c(0.6,0.8, 1),
                                colsample_bytree = c(0.6, 0.8, 1),
                                max_depth = c(4,8,12,16,18),
                                eta = c(0.001, 0.003, 0.01, 0.1))

aucErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){

    #Extract Parameters to test
    currentSubsampleRate <- parameterList[["subsample"]]
    currentColsampleRate <- parameterList[["colsample_bytree"]]
    currentMaxDepth <- parameterList[["max_depth"]]
    currentEta <- parameterList[["eta"]]

    xgbFit <- xgb.cv(data =  dtrain, nrounds = 2000, nfold = 10, showsd = TRUE, early_stopping_rounds = 50, nthread = 6,
                     metrics = "auc", verbose = TRUE, eval_metric = "auc", print_every_n = 20,
                     objective = "binary:logistic", max_depth = currentMaxDepth, eta = currentEta, #2/ntrees,
                     subsample = currentSubsampleRate, colsample_bytree = currentColsampleRate,
                     gamma = 0.0005,
                     min_child_weight = 10)

    # xvalidationScores <- as.data.frame(xgbFit)
    #Save rmse of the last iteration
    aucScr <- tail(xgbFit$evaluation_log$test_auc_mean, 1)

    return(c(aucScr, currentSubsampleRate, currentColsampleRate, currentMaxDepth, currentEta))

})





xgbFit = xgb.cv(params = param,
                data = dtrain,
                nround = 5000,
                nfold=10,
                metrics="auc",
                stratified = TRUE,
                print_every_n = 10,
                early_stopping_rounds = 10,
                showsd = TRUE)
