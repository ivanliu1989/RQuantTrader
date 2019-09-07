#' Volatility Machine Learning Models
#'
#' Volatility Machine Learning Models
#'
#' @param ACCOUNT_TYPE Account type (e.g. "real" or "paper")
#' @param ACCESS_TOKEN Account API Token
#' @param Cur1 currency 1
#' @param Cur2 currency 2
#' @param nxt.n number of lags to predict
#' @param neval number of evaluations
#' @param ntrain number of obs for training set
#' @param ib.size 1 hour
#' @param oa.size H1
#'
#'
#' @return A \code{list} of Models
#'
#' @examples
#' volatilityModel = volatilityTraining('AUD', 'USD', 3, 550, '1 Y')
#'
#' @export
volatilityTraining = function(ACCOUNT_TYPE = .oandaEnv$ACCOUNT_TYPE, ACCESS_TOKEN = .oandaEnv$ACCESS_TOKEN,
                              Cur1, Cur2, nxt.n, neval = 550, ntrain = '1 Y',
                              ib.size = '1 hour', oa.size = "H1"){
    loadQuantPackages()
    gc()

    # 0. Get Prices -----------------------------------------------------------
    PRICE.OA = prepareForexOandaPrices(ACCOUNT_TYPE, ACCESS_TOKEN, oanda.count = neval, Cur1 = Cur1, Cur2 = Cur2, oanda.granularity = oa.size)
    PRICE.IB = prepareIBForexPrices(ib.duration = ntrain, ib.barsize = ib.size, Cur1 = Cur1, Cur2 = Cur2, ibAcc = "paper", midOnly = TRUE)
    price.oa = PRICE.OA$OA.MID
    price.ib = PRICE.IB$IB.MID

    # 1. Objective Variable ---------------------------------------------------
    oa.sd = lag(100*rollapply(Cl(price.oa), width = 24, FUN = sd)/Cl(price.oa), -nxt.n); names(oa.sd) = 'target'
    ib.sd = lag(100*rollapply(Cl(price.ib), width = 24, FUN = sd)/Cl(price.ib), -nxt.n); names(ib.sd) = 'target'


    # 2. Feature Engineering --------------------------------------------------
    feats.ib =  na.omit(merge(finIndicatorHLC(price.ib),generateCandleStickPatterns(price.ib)))
    feats.oa = na.omit(merge(finIndicatorHLC(price.oa), generateCandleStickPatterns(price.oa)))

    sd.dt.ib = merge(feats.ib, ib.sd); sd.dt.ib = na.omit(sd.dt.ib)
    sd.dt.oa = merge(feats.oa, oa.sd); sd.dt.oa = na.omit(sd.dt.oa)


    # 3. Regression -----------------------------------------------------------
    trainBC = as.data.frame(sd.dt.ib[1:(nrow(sd.dt.ib)-nrow(sd.dt.oa)),])
    testBC = as.data.frame(sd.dt.oa)

    predictors = names(trainBC)[!names(trainBC) %in% c('target')]
    response = 'target'

    library(xgboost)
    set.seed(888)
    dtrain <- xgb.DMatrix(data.matrix(trainBC[, predictors]), label = trainBC[, response])
    dtest <- xgb.DMatrix(data.matrix(testBC[, predictors]), label = testBC[, response])
    watchlist <- list(train = dtrain, eval = dtest)

    param <- list(max_depth = 9,
                  eta = 0.01,
                  nthread = 6,
                  objective = "reg:linear",
                  eval_metric = "rmse",
                  booster = "gbtree",
                  gamma = 0.0001,
                  min_child_weight = 5
    )

    xgbFitReg <- xgb.train(param,
                           dtrain,
                           nrounds = 5000,
                           watchlist,
                           early_stopping_rounds = 300,
                           print_every_n = 10
    )

    xgb.pred = predict(xgbFitReg, dtest, ntreelimit = xgbFitReg$best_iteration)
    regEval = regressionEvaluation(xgb.pred, testBC$target)

    return(list(xgbFitReg = xgbFitReg,
                regEval = regEval,
                predictors = predictors))
}



#' Returns Machine Learning Models
#'
#' Returns Machine Learning Models
#'
#' @param ACCOUNT_TYPE Account type (e.g. "real" or "paper")
#' @param ACCESS_TOKEN Account API Token
#' @param Cur1 currency 1
#' @param Cur2 currency 2
#' @param nxt.n number of lags to predict
#' @param neval number of evaluations
#' @param ntrain number of obs for training set
#' @param nitr number of iteration to test
#' @param ib.size 1 hour
#' @param oa.size H1
#'
#' @return A \code{list} of Models
#'
#' @examples
#' returnTraining = returnTraining('AUD', 'USD', 3, 550, '1 Y')
#'
#' @export
returnTraining = function(ACCOUNT_TYPE = .oandaEnv$ACCOUNT_TYPE, ACCESS_TOKEN = .oandaEnv$ACCESS_TOKEN,
                          Cur1, Cur2, nxt.n, neval = 550, ntrain = '1 Y', nitr = 5,
                          ib.size = '1 hour', oa.size = "H1"){

    loadQuantPackages()
    gc()

    # 0. Get Prices -----------------------------------------------------------
    PRICE.OA = prepareForexOandaPrices(ACCOUNT_TYPE, ACCESS_TOKEN, oanda.count = neval, Cur1 = Cur1, Cur2 = Cur2, oanda.granularity = oa.size)
    PRICE.IB = prepareIBForexPrices(ib.duration = ntrain, ib.barsize = ib.size, Cur1 = Cur1, Cur2 = Cur2, ibAcc = "paper", midOnly = TRUE)
    price.oa = PRICE.OA$OA.MID
    price.ib = PRICE.IB$IB.MID

    # 1. Objective Variable ---------------------------------------------------
    oa.ret = lag(ROC(Cl(price.oa), n = nxt.n, type = 'discrete'), -nxt.n); names(oa.ret) = 'target'
    ib.ret = lag(ROC(Cl(price.ib), n = nxt.n, type = 'discrete'), -nxt.n); names(ib.ret) = 'target'

    # 2. Feature Engineering --------------------------------------------------
    feats.ib = prepareMachinelearningFeatures(price.ib)
    feats.oa = prepareMachinelearningFeatures(price.oa)

    ret.dt.ib = merge(feats.ib, ib.ret); ret.dt.ib = na.omit(ret.dt.ib)
    ret.dt.oa = merge(feats.oa, oa.ret); ret.dt.oa = na.omit(ret.dt.oa)

    # 3. Regression -----------------------------------------------------------
    library(xgboost)
    set.seed(888)

    # 4. Classifications ------------------------------------------------------
    trainBC = as.data.frame(ret.dt.ib[1:(nrow(ret.dt.ib)-nrow(ret.dt.oa)),])
    trainBC$target = ifelse(trainBC$target >=0, 1, 0)
    testBC = as.data.frame(ret.dt.oa)
    testBC$target = ifelse(testBC$target >=0, 1, 0)

    predictors = names(trainBC)[!names(trainBC) %in% c('target')]
    # predictors = importanceRaw$Feature[1:100]
    response = 'target'

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
                  min_child_weight = 10,
                  subsample = 1,
                  colsample_bytree = 0.06
    )

    for(i in 1:nitr){
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
    biEval = binaryClassifierEvaluation(xgb.pred, testBC$target)


    importanceRaw <- xgb.importance(feature_names = predictors, model = xgbFitClass)
    # xgb.plot.importance(importance_matrix = importanceRaw[1:200,])


    return(list(
        xgbFitClass = xgbFitClass,
        biEval = biEval,
        # xgb.pred = xgb.pred,
        # testBC = testBC,
        importanceRaw = importanceRaw,
        predictors = predictors))
}


#' Prepare the features for machine learning
#'
#' Prepare the features for machine learning
#'
#' @param price.x prices data
#'
#' @export
prepareMachinelearningFeatures = function(price.x){
    zscoreFeat = function(x, look.back = 63){
        # Rolling Moving Average
        MaRatio <- function(x, N){
            Mavg <- rollapply(x, N , mean)
            colnames(Mavg) <- 'Price.Ratio.MA'
            Mavg
        }
        # Rolling Standard Deviation
        Sd <- function(x, N){
            Stand.dev <- rollapply(x, N, sd)
            colnames(Stand.dev) <- "Price.Ratio.SD"
            Stand.dev
        }
        # Z Score
        ZScore <- function(Price.Ratio,Price.Ratio.MA,Price.Ratio.SD){
            a1 <- Price.Ratio
            b1 <- Price.Ratio.MA
            c1 <- Price.Ratio.SD
            z <- (a1-b1)/c1
            colnames(z)<- 'Z.Score'
            z
        }
        spreads.MA <- MaRatio(Cl(x), look.back)
        spreads.SD <- Sd(Cl(x), look.back)
        Z.Score <- ZScore(Cl(x),spreads.MA,spreads.SD)

        Z.Score2 <- zscores(Cl(x))
        Z.Score3 <- zscores.ma(Cl(x), look.back, round(look.back/6))

        zsFeat = merge(Z.Score, Z.Score2, Z.Score3); names(zsFeat) = c('ZScore1', 'ZScore2', 'ZScore3')
        return(zsFeat)
    }

    feats.x =  merge(finIndicatorHLC(price.x),generateCandleStickPatterns(price.x), zscoreFeat(price.x, 63))
    feats.x$Vol = Vo(price.x)/100000
    feats.x$PGap = Op(price.x) - lag(Cl(price.x))
    feats.x$vix.spreads = price.x$vix.spreads
    feats.x$vix.change = price.x$vix.change
    feats.x = na.omit(feats.x)

    feats.x.lag1 = lag(feats.x); names(feats.x.lag1) = paste0(names(feats.x.lag1), '.lag1')

    feats.all = merge(feats.x, feats.x.lag1)

    return(feats.all)
}




#' Prepare the non-volume features for machine learning
#'
#' Prepare the non-volume features for machine learning
#'
#' @param price.x prices data
#'
#' @export
prepareMachinelearningFeaturesIG = function(price.x){
    zscoreFeat = function(x, look.back = 63){
        # Rolling Moving Average
        MaRatio <- function(x, N){
            Mavg <- rollapply(x, N , mean)
            colnames(Mavg) <- 'Price.Ratio.MA'
            Mavg
        }
        # Rolling Standard Deviation
        Sd <- function(x, N){
            Stand.dev <- rollapply(x, N, sd)
            colnames(Stand.dev) <- "Price.Ratio.SD"
            Stand.dev
        }
        # Z Score
        ZScore <- function(Price.Ratio,Price.Ratio.MA,Price.Ratio.SD){
            a1 <- Price.Ratio
            b1 <- Price.Ratio.MA
            c1 <- Price.Ratio.SD
            z <- (a1-b1)/c1
            colnames(z)<- 'Z.Score'
            z
        }
        spreads.MA <- MaRatio(Cl(x), look.back)
        spreads.SD <- Sd(Cl(x), look.back)
        Z.Score <- ZScore(Cl(x),spreads.MA,spreads.SD)

        Z.Score2 <- zscores(Cl(x))
        Z.Score3 <- zscores.ma(Cl(x), look.back, round(look.back/6))

        zsFeat = merge(Z.Score, Z.Score2, Z.Score3); names(zsFeat) = c('ZScore1', 'ZScore2', 'ZScore3')
        return(zsFeat)
    }

    feats.x =  merge(finIndicatorHLC(price.x),generateCandleStickPatterns(price.x), zscoreFeat(price.x, 63))
    feats.x$Vol = Vo(price.x)/100000
    feats.x$vix.spreads = price.x$vix.spreads
    feats.x$vix.change = price.x$vix.change
    feats.x = na.omit(feats.x)

    feats.x.lag1 = lag(feats.x); names(feats.x.lag1) = paste0(names(feats.x.lag1), '.lag1')

    feats.all = merge(feats.x, feats.x.lag1)

    return(feats.all)
}
