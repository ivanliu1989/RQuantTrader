#' Get a Template Run of AutoPairTrading Package
#'
#' Get a Template Run of AutoPairTrading Package
#'
#' @examples
#' getUserTemplate()
#'
#' @export
getUserTemplate <- function(){
  sink("getUserTemplate.R")
  cat(
"
# 0. Env setup ------------------------------------------------------------
rm(list = ls()); gc()
library(testthat)
library(AutoPairTrading)
library(TTR)
library(data.table)

# 1. Get Y, X and Price.Ratio ---------------------------------------------
tscale <- \"2014-01-01/2016-10-01\"
y = AUDUSD[tscale]
x = CADUSD[tscale]
price.ratio <- getPriceRatio(y, x, FALSE)
names(price.ratio) = \"price.ratio\"


# 2. Correlation tests ----------------------------------------------------
cor.test <- CorrelationTest(y, x)


# 3. Stationary tests -----------------------------------------------------
adf.y <- AugmentedDickeyFullerTest(y, type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.y$signif[[1]]))
adf.x <- AugmentedDickeyFullerTest(x, type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.x$signif[[1]]))
adf.y.ret <- AugmentedDickeyFullerTest(ROC(y)[-1], type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.y.ret$signif[[1]]))
adf.x.ret <- AugmentedDickeyFullerTest(ROC(x)[-1], type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.x.ret$signif[[1]]))


# 4. Cointegration tests --------------------------------------------------
adf.ratio <- AugmentedDickeyFullerTest(price.ratio, type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.ratio$signif[[1]]))
jc.test <- JohansenCointegrationTest(merge(y,x), type = \"trace\", ecdet = \"none\", K = 2); cat(paste0(\"P-value: \", jc.test$signif[[1]]))


# 5. Half-life tests ------------------------------------------------------
half.life <- HalfLifeMeanReversion(price.ratio)$half.life.round; cat(paste0(\"Half-Life: \", half.life))


# 6. Hurse Exponent tests -------------------------------------------------
hurst.test <- HurstExponentTest(price.ratio, half.life); cat(paste0(\"Hurse Exponent: \", mean(hurst.test$hurstKY, na.rm = T)))


# 7. Hedge Ratio Calculation ----------------------------------------------
hedgeRatio <- HedgeRatioOLS(y[(length(y)-half.life):length(y)], x[(length(x)-half.life):length(x)])
hedgeRatio


# 8. Preparing the Universe data ------------------------------------------
all.bonds <- na.omit(merge(AUSYC, CANYC, USAYC))
all.bonds$IR.AUSUSA.2 = (all.bonds$AUS2Y+100)/(all.bonds$USA2Y+100)
all.bonds$IR.AUSUSA.3 = (all.bonds$AUS3Y+100)/(all.bonds$USA3Y+100)
all.bonds$IR.AUSUSA.5 = (all.bonds$AUS5Y+100)/(all.bonds$USA5Y+100)
all.bonds$IR.AUSUSA.10 = (all.bonds$AUS10Y+100)/(all.bonds$USA10Y+100)
all.bonds$IR.CANUSA.2 = (all.bonds$CAN2Y+100)/(all.bonds$USA2Y+100)
all.bonds$IR.CANUSA.3 = (all.bonds$CAN3Y+100)/(all.bonds$USA3Y+100)
all.bonds$IR.CANUSA.5 = (all.bonds$CAN5Y+100)/(all.bonds$USA5Y+100)
all.bonds$IR.CANUSA.10 = (all.bonds$CAN10Y+100)/(all.bonds$USA10Y+100)
all.bonds$IR.AUSCAN.2 = (all.bonds$AUS2Y+100)/(all.bonds$CAN2Y+100)
all.bonds$IR.AUSCAN.3 = (all.bonds$AUS3Y+100)/(all.bonds$CAN3Y+100)
all.bonds$IR.AUSCAN.5 = (all.bonds$AUS5Y+100)/(all.bonds$CAN5Y+100)
all.bonds$IR.AUSCAN.10 = (all.bonds$AUS10Y+100)/(all.bonds$CAN10Y+100)

all.bonds$IR.AUS.10.2 = (all.bonds$AUS10Y+100)/(all.bonds$AUS2Y+100)
all.bonds$IR.AUS.10.3 = (all.bonds$AUS10Y+100)/(all.bonds$AUS3Y+100)
all.bonds$IR.AUS.10.5 = (all.bonds$AUS10Y+100)/(all.bonds$AUS5Y+100)
all.bonds$IR.CAN.10.2 = (all.bonds$CAN10Y+100)/(all.bonds$CAN2Y+100)
all.bonds$IR.CAN.10.3 = (all.bonds$CAN10Y+100)/(all.bonds$CAN3Y+100)
all.bonds$IR.CAN.10.5 = (all.bonds$CAN10Y+100)/(all.bonds$CAN5Y+100)

all.bonds = all.bonds[, -c(1:12)]

datasets = na.omit(merge(price.ratio, y, x, all.bonds))[tscale]


# 9. Zscore ---------------------------------------------------------------
zc <- zscores(datasets$price.ratio)
zc.ma <- zscores.ma(datasets$price.ratio, 60, 10)


# 10. Bollinger Bands ------------------------------------------------------
BBbands <- BollingerBands(datasets$price.ratio, half.life, 2)


# 11. Momentum Indicators -------------------------------------------------
momRSI = data.frame()
for(c in 1:dim(datasets)[2]){
a = momentum.RSI(datasets[,c])
momRSI = cbind(momRSI, a)
}
colnames(momRSI) <- paste0(colnames(datasets), \".RSI\")
momRSI$XY.VAR.RSI <- abs(momRSI[,2] - momRSI[,3])

momMACD = data.frame()
for(c in 1:dim(datasets)[2]){
a = momentum.MACD(datasets[,c])$longshort
momMACD = cbind(momMACD, a)
}
colnames(momMACD) <- paste0(colnames(datasets), \".MACD\")
momMACD$XY.VAR.MACD <- abs(momMACD[,2] - momMACD[,3])

momCrossover = data.frame()
for(c in 1:dim(datasets)[2]){
a = momentum.Crossover(datasets[,c])
a = a$hamming.Dist * a$spearman * a$thickness
momCrossover = cbind(momCrossover, a)
}
colnames(momCrossover) <- paste0(colnames(datasets), \".xOver\")
momCrossover$XY.VAR.xOver <- abs(momCrossover[,2] - momCrossover[,3])


# 13. Model Data Generation -----------------------------------------------
all <- na.omit(merge(datasets, momRSI, momMACD, momCrossover))
for(i in 1:ncol(all)){
all[,i] = lag(all[,i], 1)
}
all$obj1 = lag(all$price.ratio, 1) - all$price.ratio
all$obj25 = lag(all$price.ratio, round(half.life/4)) - all$price.ratio
all$obj50 = lag(all$price.ratio, round(half.life/2)) - all$price.ratio
all$obj75 = lag(all$price.ratio, round(half.life*0.75)) - all$price.ratio
all$obj100 = lag(all$price.ratio, half.life) - all$price.ratio
all$obj125 = lag(all$price.ratio, round(half.life*1.25)) - all$price.ratio
all <- na.omit(all)
for(i in 1:ncol(all)){
all[,i] <- zscores(all[,i])
}

idx <- 1: (nrow(all)*0.8)
features <- all[, -c((ncol(all)-5):ncol(all))]
labels <- all[, c((ncol(all)-5):ncol(all))]


# 14. Machine learning Pipline --------------------------------------------
library(xgboost)
par(mfcol = c(3,2))
models <- list()
for(i in 1:ncol(labels)){
dtrain <- xgb.DMatrix(features[idx,], label = labels[idx, i])
dtest <- xgb.DMatrix(features[-idx,], label = labels[-idx, i])
watchlist = list(eval = dtest, train = dtrain)
param <- list(max.depth = 5,
eta = 0.1,
objective=\"reg:linear\",
eval_metric=\"rmse\",
num_parallel_tree = 5)
bst <- xgb.train(param, dtrain, nround = 100, watchlist, early.stop.round = 5)
# pred <- predict(bst, dtest)
# plot(pred, labels[-idx, i])
# xgb.plot.importance(xgb.importance(names(features), model = bst)[1:20])
models[[i]] = bst
}


# 15. Strategy creation ---------------------------------------------------
# Machine learning indicator
pred <- na.omit(merge(datasets, momRSI, momMACD, momCrossover))
for(i in 1:ncol(pred)){
pred[,i] = lag(pred[,i], 1)
}
pred <- na.omit(pred)
for(i in 1:ncol(pred)){
pred[,i] <- zscores(pred[,i])
}
dtest <- xgb.DMatrix(pred)
pred$obj1 <- predict(models[[1]], dtest)
pred$obj2 <- predict(models[[2]], dtest)
pred$obj3 <- predict(models[[3]], dtest)
pred$obj4 <- predict(models[[4]], dtest)
pred$obj5 <- predict(models[[5]], dtest)
pred$obj6 <- predict(models[[6]], dtest)
slope <- apply(pred[,88:93], 1, getSlope)
slope <- as.xts(as.data.frame(slope), as.Date(names(slope)))

# Mean reversion Indicator
strategies <- na.omit(merge(y, x, lag(zc.ma), lag(slope)))

# Final Strategies
strategies$strategies <- ifelse(strategies[,3] > 1 & strategies$slope > 0, strategies[,3]-strategies$slope,
ifelse(strategies[,3] < 1 & strategies$slope < 0, strategies[,3]-strategies$slope,
strategies[,3]))
strategies <- strategies[, c(1,2,5)]
strategies <- strategies[, c(1,2,3)]


# 16. Back Testing --------------------------------------------------------
context <- InitializeContext(strategies[,1], strategies[,2], capital = 1e5, window = 20, lookback = 250, brokerage = 0.001, stoploss = 0.1)
dt.summary <- BackTesting(strategies[,1], strategies[,2], context, strategies[,3], rep(-1, nrow(strategies)),
rep(1, nrow(strategies)), rep(0, nrow(strategies)))


# 17. Performance Analytics -----------------------------------------------
basic.report <- performanceReport(dt.summary)
performanceEvaluationEmail(basic.report, c(\"ivan.liuyanfeng@gmail.com\"), message = \"Machine learning & Mean Reversion\")


# 18. Searching Good Integrated Pairs -------------------------------------
data(\"sp500\")
datasets <- sp500[,c(50:80)]
searchCointegratedPairs(datasets, path = \"GoodIntegratedPairs.pdf\",
to = c(\"ivan.liuyanfeng@gmail.com\", \"ivan.liuyanfeng@gmail.com\"),
testPeriod = 63, trainPeriod = 252)
")

  sink()
  file.edit("getUserTemplate.R")
}


#' Get a Template Run of AutoPairTrading Package
#'
#' Get a Template Run of AutoPairTrading Package
#'
#' @examples
#' getUserTemplate2()
#'
#' @export
getUserTemplate2 <- function(){
  sink("getUserTemplate.R")
  cat(
"

# 0. Env setup ------------------------------------------------------------
rm(list = ls()); gc()
library(testthat)
library(AutoPairTrading)
library(TTR)
library(data.table)

# 1. Get Y, X and Price.Ratio ---------------------------------------------
tscale <- \"2014-01-01/2016-10-01\"
y = AUDUSD[tscale]
x = CADUSD[tscale]
price.ratio <- getPriceRatio(y, x, FALSE)
names(price.ratio) = \"price.ratio\"


# 1.2 Extract data from interactive brokers -------------------------------
CADUSD <- reqHistoryFX(duration = \"10 Y\", barsize = \"1 day\", Cur1 = \"USD\", Cur2 = \"CAD\")$CleanData
CADUSD <- 1/CADUSD
AUDUSD <- reqHistoryFX(duration = \"10 Y\", barsize = \"1 day\", Cur1 = \"AUD\", Cur2 = \"USD\")$CleanData
NZDUSD <- reqHistoryFX(duration = \"10 Y\", barsize = \"1 day\", Cur1 = \"NZD\", Cur2 = \"USD\")$CleanData

tscale <- \"2014-01-01/2016-07-22\"
y.series = AUDUSD[tscale]
x.series = CADUSD[tscale]
y = y.series$Close.price
x = x.series$Close.price
price.ratio <- getPriceRatio(y, x, FALSE)
names(price.ratio) = \"price.ratio\"


# 2. Correlation tests ----------------------------------------------------
cor.test <- CorrelationTest(y, x)


# 3. Stationary tests -----------------------------------------------------
adf.y <- AugmentedDickeyFullerTest(y, type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.y$signif[[1]]))
adf.x <- AugmentedDickeyFullerTest(x, type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.x$signif[[1]]))
adf.y.ret <- AugmentedDickeyFullerTest(ROC(y)[-1], type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.y.ret$signif[[1]]))
adf.x.ret <- AugmentedDickeyFullerTest(ROC(x)[-1], type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.x.ret$signif[[1]]))


# 4. Cointegration tests --------------------------------------------------
adf.ratio <- AugmentedDickeyFullerTest(price.ratio, type = \"drift\", lags = 1); cat(paste0(\"P-value: \", adf.ratio$signif[[1]]))
jc.test <- JohansenCointegrationTest(merge(y,x), type = \"trace\", ecdet = \"none\", K = 2); cat(paste0(\"P-value: \", jc.test$signif[[1]]))


# 5. Half-life tests ------------------------------------------------------
half.life <- HalfLifeMeanReversion(price.ratio)$half.life.round; cat(paste0(\"Half-Life: \", half.life))


# 6. Hurse Exponent tests -------------------------------------------------
hurst.test <- HurstExponentTest(price.ratio, half.life); cat(paste0(\"Hurse Exponent: \", mean(hurst.test$hurstKY, na.rm = T)))


# 7. Hedge Ratio Calculation ----------------------------------------------
hedgeRatio <- HedgeRatioOLS(y[(length(y)-half.life):length(y)], x[(length(x)-half.life):length(x)])
hedgeRatio


# 8. Preparing the Universe data ------------------------------------------
head(SampleUniverse)
index(y.series) <- as.Date(index(y.series))
index(x.series) <- as.Date(index(x.series))
index(price.ratio) <- as.Date(index(price.ratio))

Universe <- merge(price.ratio, y.series, x.series, SampleUniverse[,-c(1:3)])
names(Universe) <- c(\"price.ratio\", \"y.close\", \"y.bid\", \"y.ask\", \"x.close\", \"x.bid\", \"x.ask\", names(SampleUniverse[,-c(1:3)]))
Universe <- na.omit(Universe)


# 9. Back Testing ---------------------------------------------------------
context <- InitializeContext(Universe$y.close, Universe$x.close, capital = 1e6, window = 20,
                             lookback = 252, brokerage = 0.001, stoploss = 0.1, half.life = half.life)
dt.summary <- BackTestingRealTime(context, Universe, nEval = 350)
dt.summary <- BackTestingRealTimeBenchmark(context, Universe, nEval = 350)


# 10. Performance Analytics -----------------------------------------------
basic.report <- performanceReport(dt.summary)
performanceEvaluationEmail(basic.report, c(\"ivan.liuyanfeng@gmail.com\"), message = \"Machine Learning and Mean Reversion - Real Time\")


# 11. Searching Good Integrated Pairs -------------------------------------
data(\"sp500\")
datasets <- sp500[,c(50:80)]
searchCointegratedPairs(datasets, path = \"GoodIntegratedPairs.pdf\",
to = c(\"ivan.liuyanfeng@gmail.com\", \"ivan.liuyanfeng@gmail.com\"),
testPeriod = 63, trainPeriod = 252)
")

  sink()
  file.edit("getUserTemplate.R")
}

