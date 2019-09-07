rm(list=ls());gc()
library(caret)
library(xgboost)
library(quantmod)
library(tseries)
library(TTR)
library(data.table)
library(xts)
library(lattice)
library(timeSeries)
library(PerformanceAnalytics)
library(zoo)
library(IBrokers)
source('tests/AutomatedSystems/IBPaperTradingIndicators.R')
source('tests/h2o/h2oDeeplearningMain.R')
source('tests/caret/caretMachineLearningEnsemble.R')
Sys.setenv(TZ='US/Eastern')

# Sunday approximately 5 p.m. to Friday 5 p.m
AUDUSD = prepareForexOandaPrices(oanda.count = 100, Cur1 = "AUD", Cur2 = "USD", oanda.granularity = 'H1')
USDCAD = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "CAD", oanda.granularity = 'H1')
USDHUF = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "HUF", oanda.granularity = 'H1')
USDCNH = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "CNH", oanda.granularity = 'H1')
USDDKK = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "DKK", oanda.granularity = 'H1')
USDMXN = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "MXN", oanda.granularity = 'H1')
USDPLN = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "PLN", oanda.granularity = 'H1')
USDSEK = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "SEK", oanda.granularity = 'H1')
USDTHB = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "THB", oanda.granularity = 'H1')
USDZAR = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "ZAR", oanda.granularity = 'H1')
USDCZK = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "CZK", oanda.granularity = 'H1')
USDHKD = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "HKD", oanda.granularity = 'H1')
USDINR = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "INR", oanda.granularity = 'H1')
USDNOK = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "NOK", oanda.granularity = 'H1')
USDSAR = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "SAR", oanda.granularity = 'H1')
USDSGD = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "SGD", oanda.granularity = 'H1')
USDTRY = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "TRY", oanda.granularity = 'H1')
USDCHF = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "CHF", oanda.granularity = 'H1')
USDJPY = prepareForexOandaPrices(oanda.count = 100, Cur1 = "USD", Cur2 = "JPY", oanda.granularity = 'H1')
EURUSD = prepareForexOandaPrices(oanda.count = 100, Cur1 = "EUR", Cur2 = "USD", oanda.granularity = 'H1')
GBPUSD = prepareForexOandaPrices(oanda.count = 100, Cur1 = "GBP", Cur2 = "USD", oanda.granularity = 'H1')
NZDUSD = prepareForexOandaPrices(oanda.count = 100, Cur1 = "NZD", Cur2 = "USD", oanda.granularity = 'H1')


# 1. Price model ----------------------------------------------------------
aud = Cl(AUDUSD$OA.MID); #index(aud) = as.Date(index(aud))
cad = Cl(1/USDCAD$OA.MID); #index(cad) = as.Date(index(cad))
huf = Cl(1/USDHUF$OA.MID); #index(huf) = as.Date(index(huf))
cnh = Cl(1/USDCNH$OA.MID); #index(aud) = as.Date(index(aud))
dkk = Cl(1/USDDKK$OA.MID); #index(cad) = as.Date(index(cad))
mxn = Cl(1/USDMXN$OA.MID); #index(rub) = as.Date(index(rub))
pln = Cl(1/USDPLN$OA.MID); #index(huf) = as.Date(index(huf))
sek = Cl(1/USDSEK$OA.MID); #index(huf) = as.Date(index(huf))
thb = Cl(1/USDTHB$OA.MID); #index(huf) = as.Date(index(huf))
zar = Cl(1/USDZAR$OA.MID); #index(huf) = as.Date(index(huf))
czk = Cl(1/USDCZK$OA.MID); #index(huf) = as.Date(index(huf))
hkd = Cl(1/USDHKD$OA.MID); #index(huf) = as.Date(index(huf))
inr = Cl(1/USDINR$OA.MID); #index(huf) = as.Date(index(huf))
nok = Cl(1/USDNOK$OA.MID); #index(huf) = as.Date(index(huf))
sar = Cl(1/USDSAR$OA.MID); #index(huf) = as.Date(index(huf))
sgd = Cl(1/USDSGD$OA.MID); #index(huf) = as.Date(index(huf))
try = Cl(1/USDTRY$OA.MID); #index(huf) = as.Date(index(huf))
chf = Cl(1/USDCHF$OA.MID); #index(huf) = as.Date(index(huf))
jpy = Cl(1/USDJPY$OA.MID); #index(huf) = as.Date(index(huf))
eur = Cl(EURUSD$OA.MID); #index(huf) = as.Date(index(huf))
gbp = Cl(GBPUSD$OA.MID); #index(huf) = as.Date(index(huf))
nzd = Cl(NZDUSD$OA.MID); #index(huf) = as.Date(index(huf))


aud.ret = log10(Cl(AUDUSD$OA.MID)/Op(AUDUSD$OA.MID))
cad.ret = log10(Cl(1/USDCAD$OA.MID)/Op(1/USDCAD$OA.MID))
huf.ret = log10(Cl(1/USDHUF$OA.MID)/Op(1/USDHUF$OA.MID))
cnh.ret = log10(Cl(1/USDCNH$OA.MID)/Op(1/USDCNH$OA.MID))
dkk.ret = log10(Cl(1/USDDKK$OA.MID)/Op(1/USDDKK$OA.MID))
mxn.ret = log10(Cl(1/USDMXN$OA.MID)/Op(1/USDMXN$OA.MID))
pln.ret = log10(Cl(1/USDPLN$OA.MID)/Op(1/USDPLN$OA.MID))
sek.ret = log10(Cl(1/USDSEK$OA.MID)/Op(1/USDSEK$OA.MID))
thb.ret = log10(Cl(1/USDTHB$OA.MID)/Op(1/USDTHB$OA.MID))
zar.ret = log10(Cl(1/USDZAR$OA.MID)/Op(1/USDZAR$OA.MID))
czk.ret = log10(Cl(1/USDCZK$OA.MID)/Op(1/USDCZK$OA.MID))
hkd.ret = log10(Cl(1/USDHKD$OA.MID)/Op(1/USDHKD$OA.MID))
inr.ret = log10(Cl(1/USDINR$OA.MID)/Op(1/USDINR$OA.MID))
nok.ret = log10(Cl(1/USDNOK$OA.MID)/Op(1/USDNOK$OA.MID))
sar.ret = log10(Cl(1/USDSAR$OA.MID)/Op(1/USDSAR$OA.MID))
sgd.ret = log10(Cl(1/USDSGD$OA.MID)/Op(1/USDSGD$OA.MID))
try.ret = log10(Cl(1/USDTRY$OA.MID)/Op(1/USDTRY$OA.MID))
chf.ret = log10(Cl(1/USDCHF$OA.MID)/Op(1/USDCHF$OA.MID))
jpy.ret = log10(Cl(1/USDJPY$OA.MID)/Op(1/USDJPY$OA.MID))
eur.ret = log10(Cl(EURUSD$OA.MID)/Op(EURUSD$OA.MID))
gbp.ret = log10(Cl(GBPUSD$OA.MID)/Op(GBPUSD$OA.MID))
nzd.ret = log10(Cl(NZDUSD$OA.MID)/Op(NZDUSD$OA.MID))




# Iteration 1 -------------------------------------------------------------
cols = c('AUDUSD', 'USDCAD', 'USDHUF', 'USDCNH', 'USDDKK', 'USDMXN', 'USDPLN', 'USDSEK', 'USDTHB', 'USDZAR', 'USDCZK', 'USDHKD'
         ,'USDINR', 'USDNOK', 'USDSAR', 'USDSGD', 'USDTRY', 'USDCHF', 'USDJPY', 'EURUSD', 'GBPUSD', 'NZDUSD')
portf = na.omit(merge(aud.ret, cad.ret, huf.ret, cnh.ret, dkk.ret, mxn.ret, pln.ret, sek.ret, thb.ret, zar.ret
                      , czk.ret, hkd.ret, inr.ret, nok.ret, sar.ret, sgd.ret, try.ret, chf.ret, jpy.ret
                      , eur.ret, gbp.ret, nzd.ret)); names(portf) = cols
prices = na.omit(merge(aud, cad, huf, cnh, dkk, mxn, pln, sek, thb, zar, czk, hkd, inr, nok, sar, sgd, try, chf
                       , jpy, eur, gbp, nzd)); names(prices) = cols

ols.fit = lm(AUDUSD~. , portf)
summary(ols.fit)
ols.fit = lm(AUDUSD~.-1 , prices)
summary(ols.fit)
ols.fit = lm(AUDUSD~. , prices)
summary(ols.fit)


# Iteration 2 -------------------------------------------------------------
# USDHUF
# USDCNH
# USDDKK
# USDSEK
# USDTHB
# USDZAR
# USDCZK
# USDHKD
cols = c('AUDUSD', 'USDCAD', 'USDMXN', 'USDNOK')
portf = na.omit(merge(aud.ret, cad.ret, mxn.ret, nok.ret)); names(portf) = cols
prices = na.omit(merge(aud, cad, huf, mxn, nok)); names(prices) = cols

ols.fit = lm(AUDUSD~. , portf)
summary(ols.fit)
ols.fit = lm(AUDUSD~.-1 , prices)
summary(ols.fit)
ols.fit = lm(AUDUSD~. , prices)
summary(ols.fit)


# Iteration 3 -------------------------------------------------------------
# USDSAR
# USDCZK
# USDCNH
# USDHKD
# USDTRY
cols = c('AUDUSD', 'USDCAD', 'USDHUF', 'USDDKK', 'USDJPY', 'EURUSD', 'NZDUSD')
portf = na.omit(merge(aud.ret, cad.ret, huf.ret, dkk.ret, jpy.ret, eur.ret, nzd.ret)); names(portf) = cols
prices = na.omit(merge(aud, cad, huf, dkk, jpy, eur, nzd)); names(prices) = cols

ols.fit = lm(AUDUSD~. , portf)
summary(ols.fit)
ols.fit = lm(AUDUSD~.-1 , prices)
summary(ols.fit)
ols.fit = lm(AUDUSD~. , prices)
summary(ols.fit)

jc.res=JohansenCointegrationTest(prices, type = "eigen", ecdet = 'const', K = 2)
