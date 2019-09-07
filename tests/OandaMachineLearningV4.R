# Vn := voladaily := StDev({d1, ..., dn})
# Rn := en/e0
# minimize the mean squared error, i.e., minimize the quantities X i X k (R ∗ (xik) − Rn(i, k))2 , and X i X k (V ∗ (xik) − Vn(i, k))2 respectively.
# predicted returns by a power of the predicted volatility
rm(list = ls());
gc()

trainDeepLearningReturns = function(Cur1 = 'AUD', Cur2 = 'USD', nxt.n = 3){
    loadQuantPackages()

    RetModelPath = paste0('~/Common/audcad_intraday_m15/model/ReturnModel_', Cur1, Cur2, '_', round(as.numeric(Sys.time())), '.RData')
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

    # 2. Feature Engineering --------------------------------------------------
    feats.ib =  na.omit(generateCandleStickPatterns(price.ib)) # 200
    feats.oa = na.omit(generateCandleStickPatterns(price.oa)) # 200

    ret.dt.ib = merge(feats.ib, ib.ret); ret.dt.ib = na.omit(ret.dt.ib)
    ret.dt.oa = merge(feats.oa, oa.ret); ret.dt.oa = na.omit(ret.dt.oa)


    # 3. Splitting data sets --------------------------------------------------
    trainBC = as.data.frame(ret.dt.ib)
    trainBC$target = as.factor(ifelse(trainBC$target >=0, 1, 0))
    testBC = as.data.frame(ret.dt.oa)
    testBC$target = ifelse(testBC$target >=0, 1, 0)

    predictors = names(trainBC)[!names(trainBC) %in% c('target')]
    response = 'target'

    reg = F
    library(h2o)
    localH2O <- h2o.init(max_mem_size = '40g', nthreads = -1)
    h2o.removeAll()
    nfolds = 5
    evalMetrics = ifelse(reg, 'MSE', 'AUC')
    maxmize = ifelse(evalMetrics == 'AUC', TRUE, FALSE)
    glm.family = ifelse(reg, 'gaussian', 'binomial')
    # Deep learning -----------------------------------------------------------
    h2o.dl.learner = h2o.deeplearning(training_frame = as.h2o(trainBC),
                                      x=predictors,
                                      y=response,
                                      nfolds = 5,
                                      fold_assignment = 'Stratified',
                                      activation = "RectifierWithDropout", # "Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout"
                                      hidden = c(100,100,100),
                                      epochs = 1000,
                                      seed = 888,
                                      rho = 0.99,
                                      rate = 0.01,
                                      rate_annealing = 1e-06,
                                      rate_decay = 1,
                                      input_dropout_ratio = 0.2,
                                      hidden_dropout_ratios = c(0.15, 0.15, 0.15),
                                      l1 = 0.000001,
                                      l2 = 0.000003,
                                      loss = "Automatic",  # "CrossEntropy", "Quadratic", "Huber", "Absolute", "Quantile".
                                      distribution =  "AUTO", #"bernoulli", "multinomial", "gaussian", "poisson", "gamma", "tweedie", "laplace", "quantile", "huber"
                                      stopping_rounds = 20,
                                      stopping_metric = evalMetrics, # "deviance", "logloss", "MSE", "RMSE", "MAE", "RMSLE", "AUC", "lift_top_group", "misclassification", "mean_per_class_error"
                                      fast_mode = FALSE, # minor approximation in back-propagation
                                      model_id = 'h2o.dl.learner.audusd'
    )
    h2o.dl.pred.1 = as.data.frame(h2o.predict(h2o.dl.learner.audusd, as.h2o(testBC)))[,3]
    valres = binaryClassifierEvaluation(h2o.dl.pred.1, testBC$target) # 0.696 / 0.6916

    h2o::h2o.saveModel(h2o.dl.learner, path = modelPath, force = TRUE)

    return(list(
        model = h2o.dl.learner,
        valres = valres,
        path = RetModelPath
    ))
}




trainDeepLearningVolatility = function(Cur1 = 'AUD', Cur2 = 'USD', nxt.n = 3){
    loadQuantPackages()

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
    oa.sd = lag(rollapply(Cl(price.oa), width = 24, FUN = sd), -nxt.n); names(oa.sd) = 'target'
    ib.sd = lag(rollapply(Cl(price.ib), width = 24, FUN = sd), -nxt.n); names(ib.sd) = 'target'

    # 2. Feature Engineering --------------------------------------------------
    feats.ib =  na.omit(generateCandleStickPatterns(price.ib)) # 200
    feats.oa = na.omit(generateCandleStickPatterns(price.oa)) # 200

    sd.dt.ib = merge(feats.ib, ib.sd); sd.dt.ib = na.omit(sd.dt.ib)
    sd.dt.oa = merge(feats.oa, oa.sd); sd.dt.oa = na.omit(sd.dt.oa)


    # 3. Splitting data sets --------------------------------------------------
    trainBC = as.data.frame(sd.dt.ib)
    testBC = as.data.frame(sd.dt.oa)

    predictors = names(trainBC)[!names(trainBC) %in% c('target')]
    response = 'target'

    reg = T
    library(h2o)
    localH2O <- h2o.init(max_mem_size = '40g', nthreads = -1)
    h2o.removeAll()
    nfolds = 5
    evalMetrics = ifelse(reg, 'MSE', 'AUC')
    maxmize = ifelse(evalMetrics == 'AUC', TRUE, FALSE)
    glm.family = ifelse(reg, 'gaussian', 'binomial')
    # Deep learning -----------------------------------------------------------
    h2o.dl.learner = h2o.deeplearning(training_frame = as.h2o(trainBC),
                                      x=predictors,
                                      y=response,
                                      nfolds = 5,
                                      activation = "RectifierWithDropout", # "Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout"
                                      hidden = c(100,100,100),
                                      epochs = 1000,
                                      seed = 888,
                                      rho = 0.99,
                                      rate = 0.01,
                                      rate_annealing = 1e-06,
                                      rate_decay = 1,
                                      input_dropout_ratio = 0.2,
                                      hidden_dropout_ratios = c(0.15, 0.15, 0.15),
                                      l1 = 0.000001,
                                      l2 = 0.000003,
                                      loss = "Automatic",  # "CrossEntropy", "Quadratic", "Huber", "Absolute", "Quantile".
                                      distribution =  "AUTO", #"bernoulli", "multinomial", "gaussian", "poisson", "gamma", "tweedie", "laplace", "quantile", "huber"
                                      stopping_rounds = 20,
                                      stopping_metric = evalMetrics, # "deviance", "logloss", "MSE", "RMSE", "MAE", "RMSLE", "AUC", "lift_top_group", "misclassification", "mean_per_class_error"
                                      fast_mode = FALSE, # minor approximation in back-propagation
                                      model_id = 'h2o.dl.learner.audusd'
    )
    h2o.dl.pred.1 = as.data.frame(h2o.predict(h2o.dl.learner, as.h2o(testBC)))[,1]
    valres = binaryClassifierEvaluation(h2o.dl.pred.1, testBC$target) # 0.696 / 0.6916

    h2o::h2o.saveModel(h2o.dl.learner, path = modelPath, force = TRUE)

    return(list(
        model = h2o.dl.learner,
        valres = valres,
        path = VolModelPath
    ))
}
