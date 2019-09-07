rm(list=ls());gc()
library(caret)
library(xgboost)
library(quantmod)
library(vars)
source('tests/AutomatedSystems/IBPaperTradingIndicators.R')
Sys.setenv(TZ='US/Eastern')

AUDUSD = prepareForexData(ib.duration = "8 Y", ib.barsize = "1 day", Cur1 = "AUD", Cur2 = "USD",
                          oanda.granularity = 'D', QuandlSymbol1 = 'AUS', QuandlSymbol2 = 'USA', ibAcc = 'paper')


index(AUDUSD$IB.MID) = as.Date(index(AUDUSD$IB.MID))
index(AUDUSD$IB.SPREADS) = as.Date(index(AUDUSD$IB.SPREADS))
index(AUDUSD$IB.VOLATILITY) = as.Date(index(AUDUSD$IB.VOLATILITY))
index(AUDUSD$OA.CBOT) = as.Date(index(AUDUSD$OA.CBOT))
index(AUDUSD$Quandl.EquityIndex) = as.Date(index(AUDUSD$Quandl.EquityIndex))
index(AUDUSD$Quandl.YC.Bond) = as.Date(index(AUDUSD$Quandl.YC.Bond))
index(AUDUSD$Quandl.YC.Bond2) = as.Date(index(AUDUSD$Quandl.YC.Bond2))

dat = na.omit(merge(Cl(AUDUSD$IB.MID), Cl(AUDUSD$IB.SPREADS),
            AUDUSD$IB.VOLATILITY,
            # AUDUSD$OA.CBOT
            AUDUSD$Quandl.EquityIndex,
            AUDUSD$Quandl.YC.Bond,
            AUDUSD$Quandl.YC.Bond2))

dat.pca = as.xts(prcomp(dat)$x)
index(dat.pca) = as.Date(index(dat.pca))
index(dat) = as.Date(index(dat))
chart_Series(na.omit(merge(dat.pca[,1], dat[,1]))[,1])
chart_Series(na.omit(merge(dat.pca[,1], dat[,1]))[,2])

VARselect(as.data.frame(dat), lag.max = 8 , type = 'trend')
mdl <- VAR(as.data.frame(dat), lag.max=3, type="trend")
summary(mdl, equation = "IB.M.Close")
plot(mdl, names = "IB.M.Close")


ser11 <- serial.test(mdl, lags.pt = 16, type = "PT.asymptotic")
ser11$serial
arch1 <- arch.test(mdl, lags.multi = 5)
arch1$arch.mul

vecm = ca.jo(dat[,1:5], type = "trace", ecdet = "trend", K = 3, spec = "transitory")
summary(vecm)
vecm.r1 <- cajorls(vecm, r = 1)


# SR <- matrix(NA, nrow = 4, ncol = 4)
# SR[4, 2] <- 0
# LR <- matrix(NA, nrow = 4, ncol = 4)
# LR[1, 2:4] <- 0
# LR[2:4, 4] <- 0
# svec <- SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot = TRUE, runs = 100)
svec.irf <- irf(mdl, response = "IB.M.Close", n.ahead = 48, boot = TRUE)
plot(svec.irf)

vars::causality(mdl,cause="IB.M.Close")

