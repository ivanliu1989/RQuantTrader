rm(list =ls()); gc()
PRICE.AUD = prepareIBForexPrices(ib.duration = '5 Y', ib.barsize = "1 day", Cur1 = 'AUD', Cur2 = 'USD', ibAcc = "paper", midOnly = TRUE)
PRICE.CAD = prepareIBForexPrices(ib.duration = '5 Y', ib.barsize = "1 day", Cur1 = 'USD', Cur2 = 'CAD', ibAcc = "paper", midOnly = TRUE)
load('./data/cleanedBloomBergIndex.RData')
ls()
price.aud = PRICE.AUD$IB.MID
price.cad = PRICE.CAD$IB.MID

allvalue = rbind(auvalue, cavalue, usvalue)

for(Cur in c('AUDUSD', "USDCAD")){

    for(n.ret in c(1, 3, 5, 10, 21)){
        aud.ret = lag(ROC(Cl(price.aud), n = n.ret, type = 'discrete'), -2); names(aud.ret) = 'target'
        cad.ret = lag(ROC(Cl(price.cad), n = n.ret, type = 'discrete'), -2); names(cad.ret) = 'target'

        if(Cur == 'AUDUSD'){
            retvalue = aud.ret
        }else{
            retvalue = cad.ret
        }

        # AUD Ticker
        for(i in unique(allvalue$Ticker)){
            print(i)
            dt = allvalue[Ticker == i, .(DATE, ACTUAL_RELEASE)]
            dt[ACTUAL_RELEASE == '#N/A N/A', ACTUAL_RELEASE := NA]
            dt$DATE = as.Date(dt$DATE, '%d/%m/%Y')
            dt[, ACTUAL_RELEASE_CHG := c(NA,diff(as.numeric(ACTUAL_RELEASE)))]

            if(nrow(na.omit(dt)) > 5){
                dt = na.omit(as.xts(as.matrix(dt$ACTUAL_RELEASE_CHG), as.POSIXct(dt$DATE)))

                model.dt = (merge(dt, retvalue))
                for(j in 1:nrow(model.dt)){
                    if(is.na(model.dt[j, 2]) & j > 1){
                        model.dt[j, 2] = model.dt[j-1, 2]
                    }
                }
                model.dt = na.omit(model.dt)

                fit = lm(target~.-1, data=model.dt)
                summary(fit)
                cor(model.dt$target, model.dt$dt)

                tmp.res = data.frame(Ticker = i, Currency = Cur, lags = n.ret,
                                     RSquare = summary(fit)[[8]],
                                     Estimate = as.numeric(fit$coefficients),
                                     Cor = cor(model.dt$target, model.dt$dt))

                if(i == unique(allvalue$Ticker)[1]){
                    audres = tmp.res
                }else{
                    audres = rbind(audres, tmp.res)
                }
            }
        }
        setDT(audres)
        all = rbind(all, audres)
    }
}

write.csv(all, file = './data/Bloomberg_Index_Impact_Results.csv', row.names = F)



# Build Machine Learning
nxt.n = 3
PRICE.OA = prepareForexOandaPrices(ACCOUNT_TYPE, ACCESS_TOKEN,
                                   oanda.count = 2500, Cur1 = 'AUD', Cur2 = 'CAD',
                                   oanda.granularity = 'D')
price.oa = PRICE.OA$OA.MID
oa.ret = lag(ROC(Cl(price.oa), n = nxt.n, type = 'discrete'), -nxt.n); names(oa.ret) = 'target'

head(price.oa)
head(oa.ret)
allvalue = rbind(auvalue, cavalue, usvalue)
head(allvalue)
table(allvalue$ACTUAL_RELEASE)

ecoCalOanda = unique(rbind(RQuantAPI::EconomicCalendarOanda(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, "AUD_CAD", PERIOD = "1 year"),
                    RQuantAPI::EconomicCalendarOanda(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, "AUD_USD", PERIOD = "1 year"),
                    RQuantAPI::EconomicCalendarOanda(ACCOUNT_TYPE, ACCESS_TOKEN, ACCOUNT_ID, "USD_CAD", PERIOD = "1 year")))
ecoCalOanda[, timestamp := as.Date(timestamp)]
ecoCalOanda[, eventid := gsub(" ","",gsub('[[:punct:]]','',paste0(tolower(title), tolower(region), tolower(currency))))]
unique(ecoCalOanda$eventid)
ecoCalOandaFnl = copy(ecoCalOanda[,.(timestamp, eventid, previous, market, actual, forecast, impact)])
ecoCalOandaFnl[, act_pre := (as.numeric(actual) - as.numeric(previous))/as.numeric(previous)]
ecoCalOandaFnl[, act_fore := (as.numeric(actual) - as.numeric(forecast))/as.numeric(forecast)]
ecoCalOandaFnl[, act_mkt := (as.numeric(actual) - as.numeric(market))/as.numeric(market)]
ecoCalOandaFnl[, mkt_pre := (as.numeric(market) - as.numeric(previous))/as.numeric(previous)]
ecoCalOandaFnl[, mkt_fore := (as.numeric(market) - as.numeric(forecast))/as.numeric(forecast)]
ecoCalOandaFnl[, fore_pre := (as.numeric(forecast) - as.numeric(previous))/as.numeric(previous)]

ecoCalOandaFeat = ecoCalOandaFnl[, -c(2), with = F]
trainRet = oa.ret[as.Date(index(oa.ret)) %in% ecoCalOandaFeat$timestamp]
trainRet = setDT(as.data.frame(trainRet))
trainRet$timestamp = as.Date(index(oa.ret[as.Date(index(oa.ret)) %in% ecoCalOandaFeat$timestamp]))

train = merge(ecoCalOandaFeat, trainRet, by = "timestamp")
train = train[!is.na(train$target)]
train = train[,-1,with = T]





# XGBOOST
trainBC = as.data.frame(train[1:300,])
testBC = as.data.frame(train[301:438,])

predictors = names(trainBC)[!names(trainBC) %in% c('target')]
response = 'target'

dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
dtest <- xgb.DMatrix(data.matrix(testBC[, predictors]), label = testBC[, response])
watchlist <- list(train = dtrain, eval = dtest)

param <- list(max_depth = 3,
              eta = 0.1,
              nthread = 6,
              # objective = "binary:logistic",
              objective = "reg:linear",
              eval_metric = "rmse",
              booster = "gbtree",
              gamma = 0.001,
              min_child_weight = 1,
              subsample = 1,
              colsample_bytree = 1
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
importanceRaw <- xgb.importance(feature_names = predictors, model = xgbFitClass)

pred.idx = ifelse(testBC$target >=0,1,0) == ifelse(xgb.pred >=0,1,0)
table(pred.idx)
sum(abs(testBC$target[pred.idx])); sum(abs(testBC$target[!pred.idx]))
