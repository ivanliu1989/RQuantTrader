library('depmixS4')
# library('quantmod')
set.seed(1)

loadQuantPackages()
Cur1 = 'AUD'
Cur2 = 'USD'
PRICE.OA = prepareForexOandaPrices(oanda.count = 500, Cur1 = Cur1, Cur2 = Cur2, oanda.granularity = 'M15')
price.oa = PRICE.OA$OA.MID
oa.ret = ROC(Cl(price.oa), n = 1, type = 'discrete')[-1]; names(oa.ret) = 'target'


# Fit a Hidden Markov Model with two states
# to the S&P500 returns stream
hmm <- depmix(target ~ 1, family = gaussian(), nstates = 2, data=as.data.frame(oa.ret))
hmmfit <- fit(hmm, verbose = F)
post_probs <- posterior(hmmfit)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(oa.ret, type='l', main='Regime Detection', xlab='', ylab='Returns')
chart_Series(Cl(price.oa))
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='bottomleft', c('Regime #1','Regime #2'), fill=1:2, bty='n')


mean(oa.ret[post_probs[,1]==1])
mean(oa.ret[post_probs[,1]==2])
# mean(oa.ret[post_probs[,1]==3])
sd(oa.ret[post_probs[,1]==1])
sd(oa.ret[post_probs[,1]==2])
sd(oa.ret)
# sd(oa.ret[post_probs[,1]==3])

table(post_probs[,1])

plot(Cl(price.oa)[post_probs[,1]==1])
plot(Cl(price.oa)[post_probs[,1]==2])


spreads.MA <- MaRatio(Cl(price.oa), 20)
spreads.SD <- Sd(Cl(price.oa), 20)
Z.Score <- ZScore(Cl(price.oa),spreads.MA,spreads.SD)
chart_Series(Z.Score)
print(chart_Series(Z.Score[post_probs[,1]==2]))

plot(post_probs[,1], type = 'l')





















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
