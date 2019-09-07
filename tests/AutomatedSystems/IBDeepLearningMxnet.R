# https://github.com/dmlc/mxnet/tree/master/R-package
# http://mxnet.io/tutorials/r/fiveMinutesNeuralNetwork.html
# install.packages('mlbench')
# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("mxnet")
# library(mlbench)
# library(mxnet)

library(caret)
library(xgboost)
library(quantmod)
library(h2o)
source('tests/AutomatedSystems/IBPaperTradingIndicators.R')
localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '40g', nthreads = -1)
h2o.removeAll() ## clean slate - just in case the cluster was already running

trainBC$target = as.factor(ifelse(trainBC$target >= 0, 1, 0))
testBC$target = as.factor(ifelse(testBC$target >= 0, 1, 0))
response <- "target"
predictors <- setdiff(names(trainBC), response)


# Modeling ----------------------------------------------------------------
hyper_params <- list(
    activation=c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
    hidden=list(c(20,20),c(50,50),c(30,30,30),c(25,25,25,25)),
    input_dropout_ratio=c(0,0.05),
    l1=seq(0,1e-4,1e-6),
    l2=seq(0,1e-4,1e-6)
)
hyper_params

## Stop once the top 5 models are within 1% of each other (i.e., the windowed average varies less than 1%)
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 1000, seed=999,
                       stopping_rounds=5, stopping_tolerance=1e-2)
dl_random_grid <- h2o.grid(
    algorithm="deeplearning",
    grid_id = "dl_grid_random",
    training_frame=as.h2o(trainBC),
    validation_frame=as.h2o(testBC),
    x=predictors,
    y='target',
    epochs=1,
    stopping_metric="AUC",
    stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
    stopping_rounds=5,
    # score_validation_samples=10000, ## downsample validation set for faster scoring
    score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
    max_w2=10,                      ## can help improve stability for Rectifier
    hyper_params = hyper_params,
    search_criteria = search_criteria
)
grid <- h2o.getGrid("dl_grid_random",sort_by="AUC",decreasing=TRUE)
grid

grid@summary_table[1,]
best_model <- h2o.getModel(grid@model_ids[[1]]) ## model with lowest logloss
summary(best_model) ## Now the model metrics contain AUC for binary classification
plot(h2o.performance(best_model)) ## display ROC curve
head(as.data.frame(h2o.varimp(best_model)))

pred <- h2o.predict(best_model, as.h2o(testBC))
mean(as.data.frame(pred$predict) == as.numeric(testBC$target)-1) # 0.6930693
