library(quantmod)
library(h2o)
library(devtools)
# install_github("h2oai/h2o-3/h2o-r/ensemble/h2oEnsemble-package")
library(h2oEnsemble)  # This will load the `h2o` R package as well
h2o.init(nthreads = -1, max_mem_size = '40g')  # Start an H2O cluster with nthreads = num cores on your machine
h2o.removeAll() # (Optional) Remove all objects in H2O cluster


# Load data ---------------------------------------------------------------
trainBC$target = as.factor(ifelse(trainBC$target >= 0, 1, 0))
testBC$target = as.factor(ifelse(testBC$target >= 0, 1, 0))
response <- "target"
predictors <- setdiff(names(trainBC), response)
family <- "binomial"


# Setup basic learner -----------------------------------------------------
learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper",
             "h2o.gbm.wrapper", "h2o.deeplearning.wrapper")
metalearner <- "h2o.glm.wrapper"


# Train ensemble models ---------------------------------------------------
fit <- h2o.ensemble(x = predictors, y = response,
                    training_frame = as.h2o(trainBC),
                    family = family,
                    learner = learner,
                    metalearner = metalearner,
                    cvControl = list(V = 5))


# Evaluation --------------------------------------------------------------
perf <- h2o.ensemble_performance(fit, newdata = as.h2o(testBC))
perf

pred <- predict(fit, newdata = as.h2o(testBC))
predictions <- as.data.frame(pred$pred)[,3]  #third column is P(Y==1)
labels <- as.data.frame(testBC$target)[,1]



# Stacking ----------------------------------------------------------------
nfolds <- 5

glm1 <- h2o.glm(x = predictors, y = response, family = family,
                training_frame = as.h2o(trainBC),
                nfolds = nfolds,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)

gbm1 <- h2o.gbm(x = predictors, y = response, distribution = "bernoulli",
                training_frame = as.h2o(trainBC),
                seed = 1,
                nfolds = nfolds,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE)

rf1 <- h2o.randomForest(x = predictors, y = response, # distribution not used for RF
                        training_frame = as.h2o(trainBC),
                        seed = 1,
                        nfolds = nfolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE)

dl1 <- h2o.deeplearning(x = predictors, y = response, distribution = "bernoulli",
                        training_frame = as.h2o(trainBC),
                        nfolds = nfolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE)

models <- list(glm1, gbm1, rf1, dl1)
metalearner <- "h2o.glm.wrapper"

stack <- h2o.stack(models = models,
                   response_frame = as.h2o(trainBC)[,response],
                   metalearner = metalearner,
                   seed = 1,
                   keep_levelone_data = TRUE)


# Compute test set performance:
perf <- h2o.ensemble_performance(stack, newdata = as.h2o(testBC))
perf



# Shutdown ----------------------------------------------------------------
h2o.shutdown()

