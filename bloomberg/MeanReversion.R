PRICE.OA = prepareForexOandaPrices(ACCOUNT_TYPE, ACCESS_TOKEN,
                                   oanda.count = 2500, Cur1 = 'AUD', Cur2 = 'CAD',
                                   oanda.granularity = 'D')

price.oa = PRICE.OA$OA.MID
nxt.n = 6
oa.ret = lag(ROC(Cl(price.oa), n = nxt.n, type = 'discrete'), -nxt.n); names(oa.ret) = 'target'

feats.oa = prepareMachinelearningFeatures(price.oa)
ret.dt.oa = merge(feats.oa, oa.ret); ret.dt.oa = na.omit(ret.dt.oa)

library(xgboost)
set.seed(888)

# 4. Classifications ------------------------------------------------------
trainBC = as.data.frame(ret.dt.oa[1:1500,])
trainBC$target = ifelse(trainBC$target >=0, 1, 0)
testBC = as.data.frame(ret.dt.oa[1501:1800,])
testBC$target = ifelse(testBC$target >=0, 1, 0)

predictors = names(trainBC)[!names(trainBC) %in% c('target')]
response = 'target'

dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
dtest <- xgb.DMatrix(data.matrix(testBC[, predictors]), label = testBC[, response])
watchlist <- list(train = dtrain, eval = dtest)

param <- list(max_depth = 6,
              eta = 0.01,
              nthread = 6,
              objective = "binary:logistic",
              # objective = "reg:linear",
              eval_metric = "rmse",
              booster = "gbtree",
              gamma = 0.0001,
              min_child_weight = 10,
              subsample = 1,
              colsample_bytree = 0.06
)

for(i in 1:15){
    xgbFitClass <- xgb.train(param,
                             dtrain,
                             nrounds = 5000,
                             watchlist,
                             early_stopping_rounds = 300,
                             print_every_n = 10
    )
    if(i == 1){
        best_score = xgbFitClass$best_score
        xgbFit = xgbFitClass
    }else{
        if(best_score > xgbFitClass$best_score){
            best_score = xgbFitClass$best_score
            xgbFit = xgbFitClass
        }
    }
}


xgb.pred = predict(xgbFitClass, dtest, ntreelimit = xgbFitClass$best_iteration)
regEval = regressionEvaluation(xgb.pred, testBC$target)
biEval = binaryClassifierEvaluation(xgb.pred, testBC$target)
importanceRaw <- xgb.importance(feature_names = predictors, model = xgbFitClass)



# Machine Learning Test
par(mfcol = c(2,3))
plot(testBC$target, type = 'l')
lines(xgb.pred, col = 'red')
hist((testBC$target - xgb.pred)/testBC$target, 500)
plot(testBC$target,xgb.pred)

library(data.table)
quantopian = fread("./bloomberg/demo_data_1year_20150101_20160527.csv - demo_data_1year_20150101_20160527.csv.csv")
regEval = regressionEvaluation(quantopian$model_h2 , quantopian$act_log_ret)
plot(tail(quantopian$act_log_ret, 500), type = 'l')
lines(tail(quantopian$model_h2, 500), col = 'red')
hist((tail(quantopian$act_log_ret, 500) - tail(quantopian$model_h2, 500))/tail(quantopian$act_log_ret, 500), 100)
plot(tail(quantopian$act_log_ret, 500),tail(quantopian$model_h2, 500))



pred.idx = ifelse(testBC$target >=0,1,0) == ifelse(xgb.pred >=0,1,0)
table(pred.idx)
sum(abs(testBC$target[pred.idx])); sum(abs(testBC$target[!pred.idx]))

pred.idx = ifelse(tail(quantopian$act_log_ret, 500) >=0,1,0) == ifelse(tail(quantopian$model_h2, 500) >=0,1,0)
table(pred.idx)
sum(abs(tail(quantopian$act_log_ret, 500)[pred.idx])); sum(abs(tail(quantopian$act_log_ret, 500)[!pred.idx]))
