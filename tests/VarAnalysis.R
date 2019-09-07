library(vars)
dat = as.data.frame(AUDUSD$Quandl.EquityIndex)

VARselect(dat[,1:5], lag.max = 8 , type = 'both')

p1ct <- VAR(dat[,1:5], p = 1, type = "both")
p1ct
summary(p1ct, equation = "ASX")
plot(p1ct, names = "ASX")

ser11 <- serial.test(p1ct, lags.pt = 16, type = "PT.asymptotic")
ser11$serial

arch1 <- arch.test(p1ct, lags.multi = 5)
arch1$arch.mul

vecm = ca.jo(dat[,1:5], type = "trace", ecdet = "trend", K = 3, spec = "transitory")
vecm.r1 <- cajorls(vecm, r = 1)

SR <- matrix(NA, nrow = 4, ncol = 4)
SR[4, 2] <- 0
LR <- matrix(NA, nrow = 4, ncol = 4)
LR[1, 2:4] <- 0
LR[2:4, 4] <- 0
svec <- SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot = TRUE, runs = 100)
summary(svec)

svec <- SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot = TRUE, runs = 100)


svec.irf <- irf(p1ct, response = "U", n.ahead = 48, boot = TRUE)
plot(svec.irf)


vars::causality(p1ct)
