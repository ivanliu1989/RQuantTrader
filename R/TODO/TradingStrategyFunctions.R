#' Generate Indicators of Entry/Exit points
#'
#' Use rule based and machine learning predictions to generate indicators for determining next trading activities.
#'
#' @param datasets the latest datasets for modeling (first columns should always be the target series. e.g. price ratio)
#' @param half.life half-life of mean reversion series
#' @param itr default 10, number of iterations for simulations, the larger the better performance and longer running time
#'
#' @return An indicator of entry/exit
#'
#' @export
strategyBuilder <- function(datasets, half.life = 10, itr = 10){

  # 1. Zscore ---------------------------------------------------------------
  zc <- zscores(datasets$price.ratio)
  # zc.ma <- zscores.ma(datasets$price.ratio, 60, 10)

  indicator <- tail(zc,1)
  if(abs(indicator) > 1){
    # 2. Bollinger Bands ------------------------------------------------------
    # BBbands <- BollingerBands(datasets$price.ratio, half.life, 2)


    # 3. Momentum Indicators -------------------------------------------------
    momRSI = data.frame()
    for(c in 1:dim(datasets)[2]){
      a = momentum.RSI(datasets[,c])
      momRSI = cbind(momRSI, a)
    }
    colnames(momRSI) <- paste0(colnames(datasets), ".RSI")
    momRSI$XY.VAR.RSI <- abs(momRSI[,2] - momRSI[,3])

    momMACD = data.frame()
    for(c in 1:dim(datasets)[2]){
      a = momentum.MACD(datasets[,c])$longshort
      momMACD = cbind(momMACD, a)
    }
    colnames(momMACD) <- paste0(colnames(datasets), ".MACD")
    momMACD$XY.VAR.MACD <- abs(momMACD[,2] - momMACD[,3])

    momCrossover = data.frame()
    for(c in 1:dim(datasets)[2]){
      a = momentum.Crossover(datasets[,c])
      a = a$hamming.Dist * a$spearman * a$thickness
      momCrossover = cbind(momCrossover, a)
    }
    colnames(momCrossover) <- paste0(colnames(datasets), ".xOver")
    momCrossover$XY.VAR.xOver <- abs(momCrossover[,2] - momCrossover[,3])


    # 4. Model Data Generation -----------------------------------------------
    all <- na.omit(merge(datasets, momRSI, momMACD, momCrossover))
    feats <- names(all)
    forwardPred <- round(quantile(1:half.life, c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.75)))
    objFeatures <- matrix(NA, nrow = nrow(all), ncol = length(forwardPred))
    for(i in 1:length(forwardPred)){
      objFeatures[,i] = lag(all$price.ratio, forwardPred[i]) - all$price.ratio
    }
    all <- na.omit(cbind(as.data.frame(all), objFeatures))
    for(i in 1:ncol(all)){
      all[,i] <- zscores(all[,i])
    }

    pred <- na.omit(merge(datasets, momRSI, momMACD, momCrossover))
    pred <- na.omit(pred)
    for(i in 1:ncol(pred)){
      pred[,i] <- zscores(pred[,i])
    }
    pred = pred[nrow(pred),]

    idx <- 1: (nrow(all)*0.8)
    features <- all[, feats]
    labels <- all[, !names(all) %in% feats]


    # 5. Machine learning Pipline --------------------------------------------
    # par(mfcol = c(3,2))
    predictions <- xgbRegSimulation(idx, features, labels, pred, itr = itr)
    fit <- lm(y~x, as.data.frame(predictions)[predictions$x != 9,])
    plot(predictions)
    abline(fit)
    slope <- coef(fit)[2]
    slope <- as.xts(as.data.frame(slope), as.Date(index(pred)))

    indicator = indicator - 20 * slope
  }

  # 6. Strategies builder ---------------------------------------------------
  return(indicator)
}



