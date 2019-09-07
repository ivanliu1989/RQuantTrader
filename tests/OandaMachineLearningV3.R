# Vn := voladaily := StDev({d1, ..., dn})
# Rn := en/e0
# minimize the mean squared error, i.e., minimize the quantities X i X k (R ∗ (xik) − Rn(i, k))2 , and X i X k (V ∗ (xik) − Vn(i, k))2 respectively.
# predicted returns by a power of the predicted volatility
# rm(list = ls());
# gc()
loadQuantPackages()
Cur1 = 'AUD'
Cur2 = 'USD'
tuneLen = 6
nxt.n = 1

RetModelPath = paste0('~/Common/audcad_intraday_m15/model/ReturnModel_', Cur1, Cur2, '_', round(as.numeric(Sys.time())), '.RData')
VolModelPath = paste0('~/Common/audcad_intraday_m15/model/VolatilityModel_', Cur1, Cur2, '_', round(as.numeric(Sys.time())), '.RData')
DataPath = paste0('~/Common/audcad_intraday_m15/model/Data_', Cur1, Cur2, '_', round(as.numeric(Sys.time())), '.RData')

# 0. Get Prices -----------------------------------------------------------
PRICE.OA = prepareForexOandaPrices(oanda.count = 2500, Cur1 = Cur1, Cur2 = Cur2, oanda.granularity = 'M15')
PRICE.IB = prepareIBForexPrices(ib.duration = "1 Y", ib.barsize = "15 mins", Cur1 = Cur1, Cur2 = Cur2, ibAcc = "paper", midOnly = TRUE)
price.oa = PRICE.OA$OA.MID
price.ib = PRICE.IB$IB.MID
save(price.ib, price.oa, file = DataPath)
# load('~/Common/audcad_intraday_m15/model/Data_AUDUSD_1487485108.RData')

# 1. Objective Variable ---------------------------------------------------
oa.ret = lag(ROC(Cl(price.oa), n = nxt.n, type = 'discrete'), -nxt.n); names(oa.ret) = 'target'
ib.ret = lag(ROC(Cl(price.ib), n = nxt.n, type = 'discrete'), -nxt.n); names(ib.ret) = 'target'
oa.sd = lag(rollapply(Cl(price.oa), width = 24, FUN = sd), -nxt.n); names(oa.sd) = 'target'
ib.sd = lag(rollapply(Cl(price.ib), width = 24, FUN = sd), -nxt.n); names(ib.sd) = 'target'


# 2. Feature Engineering --------------------------------------------------
feats.ib = finIndicatorHLC(price.ib) # 200
feats.oa = finIndicatorHLC(price.oa) # 200

ret.dt.ib = merge(feats.ib, ib.ret); ret.dt.ib = na.omit(ret.dt.ib)
ret.dt.oa = merge(feats.oa, oa.ret); ret.dt.oa = na.omit(ret.dt.oa)
sd.dt.ib = merge(feats.ib, ib.sd); sd.dt.ib = na.omit(sd.dt.ib)
sd.dt.oa = merge(feats.oa, oa.sd); sd.dt.oa = na.omit(sd.dt.oa)


# 3. Splitting data sets --------------------------------------------------
predictors = names(ret.dt.ib)[!names(ret.dt.ib) %in% c('target')]
response = 'target'


# 4. Machine Learning Models ----------------------------------------------
# Volatility
trainBC = as.data.frame(sd.dt.ib)
volatilityModels = volatilityTraining(trainBC = trainBC, valPerc = 0.25, cores = 6, seeds = 888, tuneLen = tuneLen,
                                      predictors = predictors, response = response, pca = TRUE)
save(volatilityModels, file = VolModelPath)
# Returns
trainBC = as.data.frame(ret.dt.ib)
returnModels = returnTraining(trainBC = trainBC, valPerc = 0.25, cores = 6, seeds = 888, tuneLen = tuneLen,
                              predictors = predictors, response = response, xgbThreshold = 0.55, pca = TRUE)
save(returnModels, file = RetModelPath)


# 5. Evaluations ----------------------------------------------------------
# Classification
# Pre process
testBC.oa = as.data.frame(ret.dt.oa)
testBC.oa$target = ifelse(testBC.oa$target>=0, 1,0)
testBC.pca = predict(returnModels$preFunc, testBC.oa[, -ncol(testBC.oa)])
testBC.oa = cbind(testBC.pca, target = testBC.oa$target)

gbmPred = predict(returnModels$gbmFit, testBC.oa, type = 'prob')$Y
rfPred = predict(returnModels$rfFit, testBC.oa, type = 'prob')$Y
svmRPred = predict(returnModels$svmRFit, testBC.oa, type = 'prob')$Y
avNNetRPred = predict(returnModels$avNNetFit, testBC.oa, type = 'prob')$Y
# svmLPred = predict(returnModels$svmLFit, testBC.oa)$Y
xgbPred = predict(returnModels$xgbFit, data.matrix(testBC.oa[, -ncol(testBC.oa)]))

fnlPred = (gbmPred + rfPred + svmRPred + avNNetRPred + xgbPred)/5
binaryClassifierEvaluation(fnlPred, testBC.oa$target) # 0.517
binaryClassifierEvaluation(xgbPred, testBC.oa$target) # 0.8509


# Regression
# Pre process
testBC.oa = as.data.frame(sd.dt.oa)
testBC.pca = predict(returnModels$preFunc, testBC.oa[, -ncol(testBC.oa)])
testBC.oa = cbind(testBC.pca, target = testBC.oa$target)

gbmPred = predict(volatilityModels$gbmFit, testBC.oa)
rfPred = predict(volatilityModels$rfFit, testBC.oa)
svmRPred = predict(volatilityModels$svmRFit, testBC.oa)
svmLPred = predict(volatilityModels$svmLFit, testBC.oa)
xgbPred = predict(volatilityModels$xgbFit, data.matrix(testBC.oa[, -ncol(testBC.oa)]))

fnlPred = (gbmPred + rfPred + svmRPred + svmLPred + xgbPred)/5
regressionEvaluation(testBC.oa$target, fnlPred) # 0.961
regressionEvaluation(testBC.oa$target, svmRPred) # 0.979

