caretMachineLearningEnsemble = function(trainBC, train.points, test.points){
    library(doMC)
    registerDoMC(cores = 6)
    set.seed(825)

    trainBC$target = ifelse(trainBC$target == 1, 'Y', 'N')

    # GBM
    fitControl <- trainControl(
        method = "timeslice",
        initialWindow = train.points,
        horizon = test.points,
        fixedWindow = TRUE,
        skip = 20,
        allowParallel = TRUE
        ,summaryFunction = twoClassSummary
        ,classProbs = TRUE
    )

    gbmFit <- train(target ~ ., data = trainBC,
                    method = "gbm",
                    trControl = fitControl,
                    verbose = TRUE
                    ,metric = "ROC"
    )

    # nb
    fitControl <- trainControl(
        method = "timeslice",
        initialWindow = train.points,
        horizon = test.points,
        fixedWindow = TRUE,
        skip = 20,
        allowParallel = TRUE
        ,summaryFunction = twoClassSummary
        ,classProbs = TRUE
    )

    nbFit <- train(target ~ ., data = trainBC,
                   method = "nb",
                   trControl = fitControl,
                   verbose = TRUE
                   ,metric = "ROC"
    )

    # svmLinear
    fitControl <- trainControl(
        method = "timeslice",
        initialWindow = train.points,
        horizon = test.points,
        fixedWindow = TRUE,
        skip = 20,
        allowParallel = TRUE
        ,summaryFunction = twoClassSummary
        ,classProbs = TRUE
    )

    svmLFit <- train(target ~ ., data = trainBC,
                     method = "svmLinear",
                     trControl = fitControl,
                     verbose = TRUE
                     ,metric = "ROC"
    )

    # svmRadial
    fitControl <- trainControl(
        method = "timeslice",
        initialWindow = train.points,
        horizon = test.points,
        fixedWindow = TRUE,
        skip = 20,
        allowParallel = TRUE
        ,summaryFunction = twoClassSummary
        ,classProbs = TRUE
    )

    svmRFit <- train(target ~ ., data = trainBC,
                     method = "svmRadial",
                     trControl = fitControl,
                     verbose = TRUE
                     ,metric = "ROC"
    )

    # avNNet
    fitControl <- trainControl(
        method = "timeslice",
        initialWindow = train.points,
        horizon = test.points,
        fixedWindow = TRUE,
        skip = 20,
        allowParallel = TRUE
        ,summaryFunction = twoClassSummary
        ,classProbs = TRUE
    )

    avNNetFit <- train(target ~ ., data = trainBC,
                       method = "avNNet",
                       trControl = fitControl,
                       verbose = TRUE
                       ,metric = "ROC"
    )

    gbmPred = tail(predict(gbmFit, trainBC, type = 'prob')[,2],1)
    w.caret.gbm = max(gbmFit$results$ROC)

    nbPred = tail(predict(nbFit, trainBC, type = 'prob')[,2],1)
    w.caret.nb = max(nbFit$results$ROC)

    svmLPred = tail(predict(svmLFit, trainBC, type = 'prob')[,2],1)
    w.caret.svmL = max(svmLFit$results$ROC)

    svmRPred = tail(predict(svmRFit, trainBC, type = 'prob')[,2],1)
    w.caret.svmR = max(svmRFit$results$ROC)

    avNNetPred = tail(predict(avNNetFit, trainBC, type = 'prob')[,2],1)
    w.caret.avNNet = max(avNNetFit$results$ROC)

    return(list(
        gbmPred = gbmPred,
        nbPred = nbPred,
        svmLPred = svmLPred,
        svmRPred = svmRPred,
        avNNetPred = avNNetPred,
        gbmFit = gbmFit,
        nbFit = nbFit,
        svmLFit = svmLFit,
        svmRFit = svmRFit,
        avNNetFit = avNNetFit,
        w.caret.gbm = w.caret.gbm,
        w.caret.nb = w.caret.nb,
        w.caret.svmL = w.caret.svmL,
        w.caret.svmR = w.caret.svmR,
        w.caret.avNNet = w.caret.avNNet
    ))
}
