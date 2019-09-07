#' Anomaly Detection of Tweets on certain Topic
#'
#' When do people tweet? Anomaly Detection of Tweets on certain Topic.
#'
#' @param tweets a list of corpus and data.frame of tweets retreived from \code{tweet_corpus_chunck()} or \code{tweet_corpus{}}
#' @param lastDay last date of the analysis
#' @param title title to be added to the results plots
#' @param alpha opacity 0.5 default
#' @param fontsize font size
#'
#' @details
#' Please use \code{RQuantAPI} for twitter data retrieval
#'
#' @examples
#' setupTwitterConn()
#' tweets <- tweet_corpus_chunck(search = "aud+usd", n = 50000, chunck_size = 7, total_size = 90, until = Sys.Date())
#' saveRDS(tweets, '~/analytics/Workspace/Tweets.RDS')
#' tweets = readRDS('~/analytics/Workspace/Tweets.RDS')
#' anomalies <- twitterAnomalyDetect(tweets, lastDay = Sys.Date(), title = "AUD/USD")
#' anomalies$tweetsDistribution
#' anomalies$anomalies
#'
#' @import data.table
#' @export
twitterAnomalyDetect <- function(tweets, lastDay = Sys.Date(), title = "AUD/USD", alpha = 0.5, fontsize = 10){
    library(ggplot2)
    library(data.table)

    d = tweets$d

    timeDist = ggplot(d, aes(created)) +
        geom_density(aes(fill = isRetweet), alpha = alpha) +
        scale_fill_discrete(guide = 'none') +
        xlab('All tweets') + ggtitle(paste0(title, " tweets distribution over all periods")) +
        theme_rquant(simple_theme = TRUE, rquant_colours = TRUE, rquant_font = TRUE, logo = "rquant", font_size = fontsize, inv = 2)

    # Zoom in on last day
    dayOf = d[as.Date(d$created) >= lastDay-1,]
    timeDistDayOf = ggplot(dayOf, aes(created)) +
        geom_density(aes(fill = isRetweet), adjust = .25, alpha = alpha) +
        theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
        xlab('Day-of tweets') + ggtitle(paste0(title, " tweets distribution within last day")) +
        theme_rquant(simple_theme = TRUE, rquant_colours = TRUE, rquant_font = TRUE, logo = "rquant", font_size = fontsize, inv = 2)

    tweetsDistribution <- cowplot::plot_grid(timeDist, timeDistDayOf, rows = 2)
    tweetsDistribution

    x = setDT(d)
    x = x[, .(created)]
    x = x[, created := as.POSIXct(strptime(created, "%Y-%m-%d %H"))]
    x = unique(x[, count := .N, by = created])
    x = as.data.frame(x)
    x$count <- as.numeric(x$count)
    anomalies <- AnomalyDetectionTs(x, max_anoms=0.1, direction='both', plot=TRUE,
                                    title = paste0(title, " Tweets Anomalies Detection"))
    anomalies$plot <- anomalies$plot +
        theme_rquant(simple_theme = F, rquant_colours = TRUE, rquant_font = TRUE, logo = "rquant", font_size = fontsize, opacity = 0.05, inv = 3)


    return(list(timeDist = timeDist,
                timeDistDayOf = timeDistDayOf,
                tweetsDistribution = tweetsDistribution,
                anomalies = anomalies))
}



#' Emotional valence of Tweets on certain Topic
#'
#' When are people's attitudes? Emotional valence of Tweets on certain Topic.
#'
#' @param tweets a list of corpus and data.frame of tweets retreived from \code{tweet_corpus_chunck()} or \code{tweet_corpus{}}
#' @param title title to be added to the results plots
#' @param dir directory to save the plots
#' @param fontsize font size
#'
#' @details
#' Please use \code{RQuantAPI} for twitter data retrieval
#'
#' @examples
#' setupTwitterConn()
#' tweets <- tweet_corpus_chunck(search = "aud+usd", n = 50000, chunck_size = 7, total_size = 90, until = Sys.Date())
#' saveRDS(tweets, '~/analytics/Workspace/Tweets.RDS')
#' tweets = readRDS('~/analytics/Workspace/Tweets.RDS')
#' emotion = twitterEmotionalValence(tweets, "AUD/USD")
#' emotion$orig$text_raw[which.max(emotion$orig$emotionalValence)]
#' emotion$orig$text_raw[which.min(emotion$orig$emotionalValence)]
#' emotion$emotionDailyPlot
#' emotion$emotionPlotDailyBox
#' emotion$emotionPlot
#' emotion$emotionPlotViolin
#' emotion$wc_filename
#'
#' @export
twitterEmotionalValence <- function(tweets, title = "AUD/USD", dir = "~/analytics/common/", fontsize = 10){
    library(stringr)
    library(qdap)
    library(tm)
    library(data.table)
    d = data.table(tweets$d)

    # Text mining -------------------------------------------------------------
    if(!dir.exists(dir)){
        dir.create(dir)
    }
    # Split into retweets and original tweets
    sp = split(d, d$isRetweet)
    orig = sp[['FALSE']]
    # Extract the retweets and pull the original author's screenname
    rt = plyr::mutate(sp[['TRUE']], sender = substr(text, 5, regexpr(':', text) - 1))

    orig$text_raw = orig$text
    orig$text = text_clean(orig$text_raw, rmDuplicates = FALSE, partial = FALSE)$dataframe
    orig = orig[nchar(text) > 0,]

    cat("Capturing emotional languages.....")
    pol =
        lapply(orig$text, function(txt) {
            # strip sentence enders so each tweet is analyzed as a sentence,
            # and +'s which muck up regex
            gsub('(\\.|!|\\?)\\s+|(\\++)', ' ', txt) %>%
                # strip URLs
                gsub(' http[^[:blank:]]+', '', .) %>%
                # calculate polarity
                polarity()
        })
    orig$emotionalValence = sapply(pol, function(x) x$all$polarity)
    orig <- orig[!is.nan(emotionalValence)]

    # Some ggplots ------------------------------------------------------------
    # As reality check, what are the most and least positive tweets
    orig$text_raw[which.max(orig$emotionalValence)]
    orig$text_raw[which.min(orig$emotionalValence)]
    # How does emotionalValence change over the day?
    emotionDailyPlot <- orig[as.Date(created) == max(as.Date(created)),] %>%
        ggplot(aes(created, emotionalValence)) +
        geom_point() +
        geom_smooth(span = .5) + ggtitle(paste0(title, " - Daily Emotional Valence of Tweets \n", max(as.Date(orig$created)))) +
        theme_rquant(simple_theme = T, rquant_colours = TRUE, rquant_font = TRUE, logo = "rquant", font_size = fontsize)

    orig[as.Date(created) == max(as.Date(created)) & emotionalValence != 0,
         color := mean(emotionalValence), by = hour(as.ITime(created))]
    emotionPlotDailyBox <- ggplot(orig[as.Date(created) == max(as.Date(orig$created)) & emotionalValence != 0,],
                                  aes(factor(hour(as.ITime(created))), emotionalValence, fill = color)) +
        scale_fill_gradient(name = "Mean Emotion") +
        geom_boxplot() + labs(x = "Hour") + geom_jitter(size = 0.8, shape = 8) +
        ggtitle(paste0(title, " - Daily Emotional Valence of Tweets \n", max(as.Date(orig$created)))) +
        theme_rquant(simple_theme = T, rquant_colours = FALSE, rquant_font = TRUE, logo = "rquant", font_size = fontsize)

    emotionPlot <- orig %>%
        ggplot(aes(created, emotionalValence)) +
        geom_point() +
        geom_smooth(span = .5) + ggtitle(paste0(title, " - Weekly Emotional Valence of Tweets \n", min(as.Date(orig$created)), " to ", max(as.Date(orig$created)))) +
        theme_rquant(simple_theme = T, rquant_colours = TRUE, rquant_font = TRUE, logo = "rquant", font_size = fontsize)

    orig[emotionalValence != 0, color := mean(emotionalValence), by = as.Date(created)]
    emotionPlotViolin <- ggplot(orig[emotionalValence != 0,], aes(factor(as.Date(created)), emotionalValence, fill = color)) +
        scale_fill_gradient(name = "Mean Emotion") +
        geom_violin() + labs(x = "Date") + geom_jitter(size = 0.8, shape = 8) +
        ggtitle(paste0(title, " - Weekly Emotional Valence of Tweets \n", min(as.Date(orig$created)), " to ", max(as.Date(orig$created)))) +
        theme_rquant(simple_theme = T, rquant_colours = FALSE, rquant_font = TRUE, logo = "rquant", font_size = fontsize)
    orig[,color := NULL]

    # Emotional content -------------------------------------------------------
    polWordTables =
        sapply(pol, function(p) {
            words = c(positiveWords = paste(p[[1]]$pos.words[[1]], collapse = ' '),
                      negativeWords = paste(p[[1]]$neg.words[[1]], collapse = ' '))
            gsub('-', '', words)  # Get rid of nothing found's "-"
        }) %>%
        apply(1, paste, collapse = ' ') %>%
        stripWhitespace() %>%
        strsplit(' ') %>%
        sapply(table)

    # tryCatch({
    #     par(mfrow = c(1, 2))
    #     invisible(
    #         lapply(1:2, function(i) {
    #             dotchart(sort(polWordTables[[i]])[1:50], cex = .8)
    #             mtext(names(polWordTables)[i])
    #         }))
    # })

    # Emotionally associated non-emotional words ------------------------------
    polSplit = split(orig, sign(orig$emotionalValence))
    polText = sapply(polSplit, function(df) {
        paste(tolower(df$text), collapse = ' ') %>%
            gsub(' (http|@)[^[:blank:]]+', '', .) %>%
            gsub('[[:punct:]]', '', .)
    }) %>%
        structure(names = c('negative', 'neutral', 'positive'))

    # remove emotive words
    polText['negative'] = removeWords(polText['negative'], names(polWordTables$negativeWords))
    polText['positive'] = removeWords(polText['positive'], names(polWordTables$positiveWords))

    # Make a corpus by valence and a wordcloud from it
    corp = Corpus(VectorSource(polText))# make_corpus(polText)
    col3 = quantColours(inv = 3)[c(2,4,6)] # Define some pretty colors, mostly for later
    wcdt = as.matrix(TermDocumentMatrix(corp))
    colnames(wcdt) = c('negative', 'neutral', 'positive')

    # par(mfrow = c(1, 1))
    wc_filename = paste(dir, "comparison_cloud.png",sep='')
    # png(filename=wc_filename, width=15, height=10,units = "cm",res = 300)
    # wordcloud::comparison.cloud(wcdt,
    #                             max.words = 100, min.freq = 2, random.order=FALSE,
    #                             rot.per = 0, colors = col3, vfont = c("sans serif", "plain"))
    # dev.off()

    return(list(orig = orig,
                emotionDailyPlot = emotionDailyPlot,
                emotionPlotDailyBox = emotionPlotDailyBox,
                emotionPlot = emotionPlot,
                emotionPlotViolin = emotionPlotViolin,
                wc_filename = wc_filename,
                wcdt = wcdt))
}





#' Cluster Graph of Tweets on certain Topic
#'
#' Which words are closer to each other? Cluster Graph of Tweets on certain Topic.
#'
#' @param tweets a list of corpus and data.frame of tweets retreived from \code{tweet_corpus_chunck()} or \code{tweet_corpus{}}
#' @param title title to be added to the results plots
#' @param minDocFreq minimum frequenciesof each term in percent in all documents
#' @param groups number of clusters
#' @param dir directory to save the plots
#'
#' @details
#' Please use \code{RQuantAPI} for twitter data retrieval
#'
#' @examples
#' setupTwitterConn()
#' tweets <- tweet_corpus_chunck(search = "aud+usd", n = 50000, chunck_size = 7, total_size = 90, until = Sys.Date())
#' saveRDS(tweets, '~/analytics/Workspace/Tweets.RDS')
#' tweets = readRDS('~/analytics/Workspace/Tweets.RDS')
#' twitterClusterGraph(tweets, minDocFreq = 0.95, groups = 6, title ="AUD/USD")
#'
#' @export
twitterClusterGraph <- function(tweets, minDocFreq = 0.9, groups = 6, title ="AUD/USD", dir = "~/analytics/common/"){
    library(tm)
    if(!dir.exists(dir)){
        dir.create(dir)
    }

    # Cluster Graph -----------------------------------------------------------
    v = tweets$v
    # term-document matrix
    v_corpus <- text_clean(v, rmDuplicates = TRUE, partial = FALSE)$corpus
    tdm = TermDocumentMatrix(v_corpus)
    # convert tdm to matrix
    m = as.matrix(tdm)
    # word counts
    wc = rowSums(m)

    # get those words above the 3rd quantile
    lim = quantile(wc, probs=minDocFreq)
    good = m[wc > lim,]
    # remove columns (docs) with zeroes
    good = good[,colSums(good)!=0]


    # adjacency matrix
    M = good %*% t(good)

    # set zeroes in diagonal
    diag(M) = 0

    # graph
    library(igraph)
    g = graph.adjacency(M, weighted=TRUE, mode="undirected",
                        add.rownames=TRUE)
    # layout
    glay = layout.fruchterman.reingold(g)

    # let's superimpose a cluster structure with k-means clustering
    kmg = kmeans(M, centers=groups)
    gk = kmg$cluster

    # create nice colors for each cluster
    gbrew = quantColours()[-c(2:4)]
    gpal = rgb2hsv(col2rgb(gbrew))
    gcols = rep("", length(gk))
    for (k in 1:groups) {
        gcols[gk == k] = hsv(gpal[1,k], gpal[2,k], gpal[3,k], alpha=0.5)
    }

    # prepare ingredients for plot
    V(g)$size = 10
    V(g)$label = V(g)$name
    V(g)$degree = degree(g)
    #V(g)$label.cex = 1.5 * log10(V(g)$degree)
    V(g)$label.color = hsv(0, 0, 0.2, 0.55)
    V(g)$frame.color = NA
    V(g)$color = gcols
    E(g)$color = hsv(0, 0, 0.7, 0.3)

    # plot
    par(mfrow = c(1, 1))
    title = paste0(title, " - Cluster Graph of tweets \n", min(as.Date(tweets$d$created)), " to ", max(as.Date(tweets$d$created)))
    wc_filename = paste(dir, "custer_graph.png",sep='')
    png(filename=wc_filename)
    plot(g, layout=glay)
    title(title,
          col.main=gbrew[1], cex.main=1, family="serif")
    dev.off()

    return(list(wc_filename = wc_filename,
                plot = g,
                title = title))
}
