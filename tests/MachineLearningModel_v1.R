rm(list = ls()); gc()

# Model -------------------------------------------------------------------
audusdRetModel = returnTraining(ACCOUNT_TYPE, ACCESS_TOKEN, 'AUD', 'USD', 1, 1000, '2 Y', nitr = 10,
                                ib.size = '15 mins', oa.size = "M15")
usdcadRetModel = returnTraining(ACCOUNT_TYPE, ACCESS_TOKEN, 'USD', 'CAD', 2, 1000, '2 Y', nitr = 10,
                                ib.size = '15 mins', oa.size = "M15")

audusdVolModel = volatilityTraining(ACCOUNT_TYPE, ACCESS_TOKEN, 'AUD', 'USD', 4, 1000, '1 Y',
                                    ib.size = '15 mins', oa.size = "M15")
usdcadVolModel = volatilityTraining(ACCOUNT_TYPE, ACCESS_TOKEN, 'USD', 'CAD', 4, 1000, '1 Y',
                                    ib.size = '15 mins', oa.size = "M15")


# Result ------------------------------------------------------------------
audusdRetModel$biEval$summaryStats
usdcadRetModel$biEval$summaryStats

audusdVolModel$regEval$goodness_of_fit
usdcadVolModel$regEval$goodness_of_fit


# Validation --------------------------------------------------------------
nxt.n = 2;
Cur1 = 'USD'
Cur2 = 'CAD'
neval = 480
PRICE.OA = prepareForexOandaPrices(ACCOUNT_TYPE, ACCESS_TOKEN, oanda.count = neval, Cur1 = Cur1, Cur2 = Cur2,
                                   oanda.granularity = 'M15')
price.oa = PRICE.OA$OA.MID
tgt = lag(ROC(Cl(price.oa), n = nxt.n, type = 'discrete'), -nxt.n); names(tgt) = 'target'
price.feat =  prepareMachinelearningFeatures(price.oa)
price.feat = na.omit(merge(price.feat, tgt))

oa.pred.ret = predict(usdcadRetModel$xgbFitClass, data.matrix(price.feat[,usdcadRetModel$predictors]))
oa.pred.sd = predict(usdcadVolModel$xgbFitReg, data.matrix(price.feat[,usdcadVolModel$predictors]))

binaryClassifierEvaluation(oa.pred.ret, as.numeric(ifelse(price.feat$target>=0, 1,0)))

# conv.ret = sign((oa.pred.ret - 0.5)) * abs(oa.pred.ret - 0.5) ^ oa.pred.sd # 0.8


# Load model --------------------------------------------------------------
save(audusdRetModel, usdcadRetModel, audusdVolModel, usdcadVolModel,
     file = '~/Common/models/audcad_15m_models/audcad_15m_models.RData')
save(audusdRetModel, usdcadRetModel, audusdVolModel, usdcadVolModel,
     file = '~/Common/models/audcad_H1_models/audcad_H1_models.RData')
# load('~/Common/models/audcad_15m_models/audcad_15m_models.RData')

# volatility.threshold = 0.05
# volatility.multiplier = volatility.threshold / oa.pred.sd # low -> more | high -> less
# classifier.threshold = 0.5
# classifier.multiplier = sign(oa.pred.ret - classifier.threshold) * (abs(oa.pred.ret - classifier.threshold) + classifier.threshold)/classifier.threshold
