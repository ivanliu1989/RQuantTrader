#' Xgboost Simulations for Regression
#'
#' Use xgboost algorithm to predict future movements of the price ratios
#'
#' @param idx index for train/test split
#' @param features feature data used for training
#' @param labels objective variables
#' @param pred today's features for predicting
#' @param itr default 10, number of iterations for simulations, the larger the better performance and longer running time
#'
#' @return A vector of estimated future slopes/movements
#'
#' @export
#' @import xgboost
xgbRegSimulation <- function(idx, features, labels, pred, itr = 10){
  library(xgboost)
  # par(mfcol = c(3,2))
  dpred <- xgb.DMatrix(pred, missing = NaN)

  # predictions <- matrix(NA, nrow = itr, ncol = ncol(labels))
  predictions <- matrix(NA, nrow = itr*ncol(labels), ncol = 2)
  for(i in 1:ncol(labels)){
    # cat(paste0("\nLabel: ", i))
    dtrain <- xgb.DMatrix(data.matrix(features[idx,]), label = labels[idx, i], missing = NaN)
    dtest <- xgb.DMatrix(data.matrix(features[-idx,]), label = labels[-idx, i], missing = NaN)
    watchlist = list(eval = dtest, train = dtrain)
    param <- list(max.depth = 5,
                  eta = 0.1,
                  objective="reg:linear",
                  eval_metric="rmse",
                  subsample = 0.99)
    for(j in 1:itr){
      # cat(paste0("\nLabel: ", i, " Iteration: ", j))
      bst <- xgb.train(param, dtrain, nround = 100, watchlist, early.stop.round = 5, print.every.n = 100)
      predictions[itr*(i-1)+j,] = c(i, predict(bst, dpred))
      # predictions <- c(predictions, predict(bst, dpred))
    }
  }
  colnames(predictions) <- c("x", "y")
  # plot(pred, labels[-idx, i])
  # xgb.plot.importance(xgb.importance(names(features), model = bst)[1:20])
  return(as.data.frame(predictions))
}
