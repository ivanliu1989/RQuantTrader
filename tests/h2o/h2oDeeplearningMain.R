h2oDeeplearingEnsemble = function(trainBC, testBC = NULL, predictors, response, reg = FALSE){
    library(h2o)
    localH2O <- h2o.init(max_mem_size = '40g', nthreads = -1)
    h2o.removeAll()
    nfolds = 5
    evalMetrics = ifelse(reg, 'MSE', 'AUC')
    maxmize = ifelse(evalMetrics == 'AUC', TRUE, FALSE)
    glm.family = ifelse(reg, 'gaussian', 'binomial')
    # Deep learning -----------------------------------------------------------
    # 0.6983739837398374
    hyper_params <- list(
        activation=c("Rectifier","Maxout","RectifierWithDropout", "MaxoutWithDropout"),# "Tanh","TanhWithDropout",
        hidden=list(c(20,20),c(50,50),c(30,30,30),c(25,25,25,25)),
        input_dropout_ratio=c(0,0.05),
        l1=seq(0,1e-6,1e-8),
        l2=seq(0,1e-6,1e-8),
        epochs=c(1,5) #,10,30
    )
    search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 100, seed=999,
                           stopping_rounds=5, stopping_tolerance=1e-2)
    dl_random_grid <- h2o.grid(
        algorithm="deeplearning",
        grid_id = "dl_grid_random",
        training_frame=as.h2o(trainBC),
        # validation_frame=as.h2o(testBC),
        x=predictors,
        y=response,
        stopping_metric=evalMetrics,
        stopping_tolerance=1e-2,
        stopping_rounds=5,
        score_duty_cycle=0.025,
        max_w2=10,
        nfolds = nfolds,
        fold_assignment = "Modulo",
        keep_cross_validation_predictions = TRUE,
        hyper_params = hyper_params,
        search_criteria = search_criteria
    )
    grid <- h2o.getGrid("dl_grid_random",sort_by=evalMetrics,decreasing=maxmize)
    grid@summary_table[1,]
    h2o.dl.learner.1 <- h2o.getModel(grid@model_ids[[1]])
    h2o.dl.learner.2 <- h2o.getModel(grid@model_ids[[2]])
    h2o.dl.learner.3 <- h2o.getModel(grid@model_ids[[3]])
    h2o.dl.learner.4 <- h2o.getModel(grid@model_ids[[4]])
    h2o.dl.learner.5 <- h2o.getModel(grid@model_ids[[5]])
    # Performance
    # h2o.confusionMatrix(h2o.dl.learner.1, valid = TRUE)

    # GBM ---------------------------------------------------------------------
    # h2o.gbm.learner <- h2o.gbm(
    #     training_frame=as.h2o(trainBC),
    #     # validation_frame=as.h2o(testBC),
    #     x=predictors,
    #     y=response,
    #     ntrees = 1000,
    #     learn_rate = 0.01,
    #     learn_rate_annealing = 0.99,
    #     min_rows = 10,
    #     max_depth = 3,
    #     stopping_rounds = 2,
    #     stopping_tolerance = 0.01,
    #     fold_assignment = "Modulo",
    #     nfolds = nfolds,
    #     seed = 999)
    # Performance
    # h2o.confusionMatrix(h2o.gbm.learner, valid = TRUE)


    # GLM ---------------------------------------------------------------------
    # h2o.glm.learner = h2o.glm(training_frame=as.h2o(trainBC),
    #                           # validation_frame=as.h2o(testBC),
    #                           x=predictors,
    #                           y=response,
    #                           family=glm.family,
    #                           max_iterations=1e5,
    #                           # standardize = TRUE,
    #                           # intercept = FALSE,
    #                           alpha = 0, # 1 lasso | 0 ridge
    #                           lambda_search=TRUE,
    #                           # fold_assignment = "Modulo",
    #                           nfolds = nfolds,
    #                           early_stopping=TRUE
    # )
    # Performance
    # h2o.confusionMatrix(h2o.glm.learner, valid = TRUE)


    # RF ----------------------------------------------------------------------
    # h2o.rf.learner <- h2o.randomForest(
    #     training_frame=as.h2o(trainBC),
    #     # validation_frame=as.h2o(testBC),
    #     x=predictors,
    #     y=response,
    #     # mtries = 12,
    #     # col_sample_rate_change_per_level = 0.8,
    #     # sample_rate = 0.632,
    #     # col_sample_rate_per_tree = 0.8,
    #     ntrees = 200,
    #     max_depth = 6,
    #     # min_rows = 10,
    #     binomial_double_trees = TRUE,
    #     balance_classes = TRUE,
    #     stopping_metric = evalMetrics,
    #     stopping_rounds = 2,
    #     stopping_tolerance = 1e-2,
    #     score_each_iteration = T,
    #     fold_assignment = "Modulo",
    #     nfolds = nfolds,
    #     seed=999)
    # Performance
    # h2o.confusionMatrix(h2o.rf.learner, valid = TRUE)

    # NB ----------------------------------------------------------------------
    # h2o.nb.learner <- h2o.naiveBayes(
    #     training_frame=as.h2o(trainBC),
    #     # validation_frame=as.h2o(testBC),
    #     x=predictors,
    #     y=response,
    #     ignore_const_cols=TRUE,
    #     compute_metrics=TRUE,
    #     fold_assignment = "Modulo",
    #     nfolds = nfolds,
    #     seed=999
    # )
    # Performance
    # h2o.confusionMatrix(h2o.nb.learner, valid = TRUE)

    # Stacking ----------------------------------------------------------------
    h2o.dl.pred.1 = tail(as.data.frame(h2o.predict(h2o.dl.learner.1, as.h2o(trainBC)))[,3],1)
    h2o.dl.pred.2 = tail(as.data.frame(h2o.predict(h2o.dl.learner.2, as.h2o(trainBC)))[,3],1)
    h2o.dl.pred.3 = tail(as.data.frame(h2o.predict(h2o.dl.learner.3, as.h2o(trainBC)))[,3],1)
    h2o.dl.pred.4 = tail(as.data.frame(h2o.predict(h2o.dl.learner.4, as.h2o(trainBC)))[,3],1)
    h2o.dl.pred.5 = tail(as.data.frame(h2o.predict(h2o.dl.learner.5, as.h2o(trainBC)))[,3],1)
    # h2o.glm.pred = tail(as.data.frame(h2o.predict(h2o.glm.learner, as.h2o(testBC)))[,3],1)
    # h2o.gbm.pred = tail(as.data.frame(h2o.predict(h2o.gbm.learner, as.h2o(testBC)))[,3],1)
    # h2o.rf.pred = tail(as.data.frame(h2o.predict(h2o.rf.learner, as.h2o(testBC)))[,3],1)
    # h2o.nb.pred = tail(as.data.frame(h2o.predict(h2o.nb.learner, as.h2o(testBC)))[,3],1)

    w.dl.1 = h2o.dl.learner.1@model$training_metrics@metrics$AUC
    w.dl.2 = h2o.dl.learner.2@model$training_metrics@metrics$AUC
    w.dl.3 = h2o.dl.learner.3@model$training_metrics@metrics$AUC
    w.dl.4 = h2o.dl.learner.4@model$training_metrics@metrics$AUC
    w.dl.5 = h2o.dl.learner.5@model$training_metrics@metrics$AUC
    # w.glm = h2o.glm.learner@model$validation_metrics@metrics$AUC
    # w.gbm = h2o.gbm.learner@model$validation_metrics@metrics$AUC
    # w.rf = h2o.rf.learner@model$validation_metrics@metrics$AUC
    # w.bn = h2o.nb.learner@model$validation_metrics@metrics$AUC

    h2o.shutdown(prompt = FALSE)

    return(list(h2o.dl.pred.1 = h2o.dl.pred.1,
                h2o.dl.pred.2 = h2o.dl.pred.2,
                h2o.dl.pred.3 = h2o.dl.pred.3,
                h2o.dl.pred.4 = h2o.dl.pred.4,
                h2o.dl.pred.5 = h2o.dl.pred.5,
                h2o.dl.learner.1 = h2o.dl.learner.1,
                h2o.dl.learner.2 = h2o.dl.learner.2,
                h2o.dl.learner.3 = h2o.dl.learner.3,
                h2o.dl.learner.4 = h2o.dl.learner.4,
                h2o.dl.learner.5 = h2o.dl.learner.5,
                # h2o.glm.pred = h2o.glm.pred,
                # h2o.gbm.pred = h2o.gbm.pred,
                # h2o.rf.pred = h2o.rf.pred,
                # h2o.nb.pred = h2o.nb.pred,
                w.dl.1 = w.dl.1,
                w.dl.2 = w.dl.2,
                w.dl.3 = w.dl.3,
                w.dl.4 = w.dl.4,
                w.dl.5 = w.dl.5
                # w.glm = w.glm,
                # w.gbm = w.gbm,
                # w.rf = w.rf,
                # w.bn = w.bn
                ))
}
