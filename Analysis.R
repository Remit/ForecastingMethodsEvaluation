ts.characteristics <- compute.characteristics(lst)
scores.tst <- scores.and.models$scores
scores.tst.enhanced <- cbind(scores.tst, ts.characteristics)
scores.tst.grpd <- group_by(scores.tst, min.score.index)
summarise(scores.tst.grpd, n = n())

scores.tst$ets.duration = as.numeric(scores.tst$ets.duration, units="secs")
scores.tst$sarima.duration = as.numeric(scores.tst$sarima.duration, units="secs")
scores.tst$sarimaoa.duration = as.numeric(scores.tst$sarimaoa.duration, units="secs")
scores.tst$sarima.garch.duration = as.numeric(scores.tst$sarima.garch.duration, units="secs")
scores.tst$sarimaoa.garch.duration = as.numeric(scores.tst$sarimaoa.garch.duration, units="secs")
scores.tst$sarfima.duration = as.numeric(scores.tst$sarfima.duration, units="secs")
scores.tst$sarfima.garch.duration = as.numeric(scores.tst$sarfima.garch.duration, units="secs")
scores.tst$lm.duration = as.numeric(scores.tst$lm.duration, units="secs")
scores.tst.new.estimate <- scores.tst[,seq(2,7)] * scores.tst[, seq(2,7) + 8]
scores.tst.new.estimate$min.score.index <- apply(scores.tst.new.estimate, 1, which.min)
scores.tst.grpd.new.estimate <- group_by(scores.tst.new.estimate, min.score.index)
summarise(scores.tst.grpd.new.estimate, n = n())

scores.tst.melted <- melt(scores.tst[,1:16])
scores.tst.melted$variable <- as.character(scores.tst.melted$variable)
scores.tst.melted$is.score <- grepl("score", scores.tst.melted$variable)
scores.tst.melted[scores.tst.melted$is.score == TRUE, "method"] <- substr(scores.tst.melted[scores.tst.melted$is.score == TRUE, "variable"],
                                                                          1,
                                                                          nchar(scores.tst.melted[scores.tst.melted$is.score == TRUE, "variable"]) - 6)
scores.tst.melted[scores.tst.melted$is.score == FALSE, "method"] <- substr(scores.tst.melted[scores.tst.melted$is.score == FALSE, "variable"], 
                                                                           1,
                                                                           nchar(scores.tst.melted[scores.tst.melted$is.score == FALSE, "variable"]) - 9)
scores.tst.melted$method <- as.factor(scores.tst.melted$method)
scores.tst.for.plot <- data.frame(method = scores.tst.melted[scores.tst.melted$is.score == TRUE, "method"],
                                  score = scores.tst.melted[scores.tst.melted$is.score == TRUE, "value"],
                                  time = scores.tst.melted[scores.tst.melted$is.score == FALSE, "value"])
scores.tst.for.plot <- scores.tst.for.plot[!is.nan(scores.tst.for.plot$score),]

# General plot
ggplot(scores.tst.for.plot, aes(x=score,
                                y=time,
                                color=method)) + geom_point()

# Medians help to organize the acquired data points into 4 classes. Leftmost lower quadrant represents the class with the most efficient forecasting algs.
median.score <- median(scores.tst.for.plot$score)
median.time <- median(scores.tst.for.plot$time)

# Quadrant classes
# First class methods (best score & best time)
scores.tst.for.plot.first.class <- scores.tst.for.plot[scores.tst.for.plot$score <= median.score,]
scores.tst.for.plot.first.class <- scores.tst.for.plot.first.class[scores.tst.for.plot.first.class$time <= median.time,]
scores.tst.for.plot.first.class.grpd <- group_by(scores.tst.for.plot.first.class, method)
methods.first.class <- summarise(scores.tst.for.plot.first.class.grpd, n = n()) 
methods.first.class$frequency.in.class <- methods.first.class$n / nrow(scores.tst)

# Second class methods (best score & bad time)
scores.tst.for.plot.second.class <- scores.tst.for.plot[scores.tst.for.plot$score <= median.score,]
scores.tst.for.plot.second.class <- scores.tst.for.plot.second.class[scores.tst.for.plot.second.class$time > median.time,]
scores.tst.for.plot.second.class.grpd <- group_by(scores.tst.for.plot.second.class, method)
methods.second.class <- summarise(scores.tst.for.plot.second.class.grpd, n = n()) 
methods.second.class$frequency.in.class <- methods.second.class$n / nrow(scores.tst)

# Third class methods (bad score & best time)
scores.tst.for.plot.third.class <- scores.tst.for.plot[scores.tst.for.plot$score > median.score,]
scores.tst.for.plot.third.class <- scores.tst.for.plot.third.class[scores.tst.for.plot.third.class$time <= median.time,]
scores.tst.for.plot.third.class.grpd <- group_by(scores.tst.for.plot.third.class, method)
methods.third.class <- summarise(scores.tst.for.plot.third.class.grpd, n = n()) 
methods.third.class$frequency.in.class <- methods.third.class$n / nrow(scores.tst)

# Fourth class methods (bad score & bad time)
scores.tst.for.plot.fourth.class <- scores.tst.for.plot[scores.tst.for.plot$score > median.score,]
scores.tst.for.plot.fourth.class <- scores.tst.for.plot.fourth.class[scores.tst.for.plot.fourth.class$time > median.time,]
scores.tst.for.plot.fourth.class.grpd <- group_by(scores.tst.for.plot.fourth.class, method)
methods.fourth.class <- summarise(scores.tst.for.plot.fourth.class.grpd, n = n()) 
methods.fourth.class$frequency.in.class <- methods.fourth.class$n / nrow(scores.tst)

# Result: Best model in respect both to time and score - SARIMA. although not the best for the majority of cases as is shown below.

# High-score vs. low-score
scores.tst.for.plot.best.score <- scores.tst.for.plot[scores.tst.for.plot$score <= median.score,]
scores.tst.for.plot.best.score.grpd <- group_by(scores.tst.for.plot.best.score, method)
best.scores <- summarise(scores.tst.for.plot.best.score.grpd, n = n())
best.scores$frequency.in.class <- best.scores$n / nrow(scores.tst)

# Result: Best model in respect to score - outliers-adjusted SARIMA with GARCH

# parameters to zoom in the graph as the points are dense near the (0,0) coordinate
SCORE.ZOOM <- median.score * 2 
TIME.ZOOM <- median.time * 2
scores.tst.for.plot <- scores.tst.for.plot[scores.tst.for.plot$score <= SCORE.ZOOM,]
scores.tst.for.plot <- scores.tst.for.plot[scores.tst.for.plot$time <= TIME.ZOOM,]

#Zoomed plot with classes identified
ggplot(scores.tst.for.plot, aes(x=score,
                                y=time,
                                color=method)) + geom_point() + geom_vline(xintercept = median.score) + geom_hline(yintercept = median.time)

#GARCH modification vs non-GARCH
# SARIMAOA
scores.tst.for.plot.sarimaoa <- scores.tst.for.plot[grep("sarimaoa", scores.tst.for.plot$method),]
scores.tst.for.plot.sarimaoa$is.garch <- grepl("garch", scores.tst.for.plot.sarimaoa$method)
ggplot(scores.tst.for.plot.sarimaoa, aes(x=score,
                                         y=time,
                                         color=is.garch)) + geom_point() + ggtitle("Outliers-adjusted SARIMA: GARCH vs. non-GARCH")
#SARFIMA
scores.tst.for.plot.sarfima <- scores.tst.for.plot[grep("sarfima", scores.tst.for.plot$method),]
scores.tst.for.plot.sarfima$is.garch <- grepl("garch", scores.tst.for.plot.sarfima$method)
ggplot(scores.tst.for.plot.sarfima, aes(x=score,
                                        y=time,
                                        color=is.garch)) + geom_point() + ggtitle("Outliers-adjusted SARFIMA: GARCH vs. non-GARCH")

#SARIMA
scores.tst.for.plot.sarima <- scores.tst.for.plot[scores.tst.for.plot$method %in% c("sarima", "sarima.garch"),]
scores.tst.for.plot.sarima$is.garch <- grepl("garch", scores.tst.for.plot.sarima$method)
ggplot(scores.tst.for.plot.sarima, aes(x=score,
                                       y=time,
                                       color=is.garch)) + geom_point() + ggtitle("Simple SARIMA: GARCH vs. non-GARCH")

# Conclusion - the overall positive impact of the presence of GARCH component in the model is not so evident, although it is definetely increases the duration of model fit procedure.

#Outliers-aligned modification vs simple
scores.tst.for.plot.oa <- scores.tst.for.plot
scores.tst.for.plot.oa$is.outliers.aligned <- grepl("sarimaoa", scores.tst.for.plot.oa$method)
ggplot(scores.tst.for.plot.oa, aes(x=score,
                                   y=time,
                                   color=is.outliers.aligned)) + geom_point() + ggtitle("SARIMA: simple vs. outliers-aligned")

#Conclusion - accounting for outliers does not grant any advantage in terms of the score although on average requires slightly more time for fitting the model
#nevertheless, it is necessary to adjust for outliers as in other data series we may find some outliers that would bias our model significantly

# Pairwise comparison of models in terms of score
pairs(scores.tst[,1:10])
# Pairwise comparison of models in terms of fitting duration
pairs(scores.tst[,11:20])


#TESTING the calculation of the number of instances and their prices
requests.bottlenecks <- data.frame(inst.type = c("t2.micro", "t2.small", "t2.medium", "t2.large"),
                                   requests.num.per.min = c(1980, 1980, 3780, 3840),
                                   cost.per.hour = c(0.0116, 0.023, 0.0464, 0.0928))

sarima.garch.ub.95.adjusted <- sarima.garch.ub.95 * 10000000
t2.micro.num <- ceiling(sarima.garch.ub.95.adjusted / (requests.bottlenecks[1,"requests.num.per.min"] * 60))
t2.small.num <- ceiling(sarima.garch.ub.95.adjusted / (requests.bottlenecks[2,"requests.num.per.min"] * 60))
t2.medium.num <- ceiling(sarima.garch.ub.95.adjusted / (requests.bottlenecks[3,"requests.num.per.min"] * 60))
t2.large.num <- ceiling(sarima.garch.ub.95.adjusted / (requests.bottlenecks[4,"requests.num.per.min"] * 60))

t2.micro.price <- sum(t2.micro.num * requests.bottlenecks[1,"cost.per.hour"])
t2.small.price <- sum(t2.micro.num * requests.bottlenecks[2,"cost.per.hour"])
t2.medium.price <- sum(t2.micro.num * requests.bottlenecks[3,"cost.per.hour"])
t2.large.price <- sum(t2.micro.num * requests.bottlenecks[4,"cost.per.hour"])


# IDEAS:
# 1. Add ARIMA models without regressors to look at the influence of these