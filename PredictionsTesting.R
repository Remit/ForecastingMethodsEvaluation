library(imputeTS)
library(ggplot2)
library(tseries)
library(fGarch)
library(forecast)

Sys.setlocale("LC_TIME", "English") # This is very important in case the computer locale for time is different from English - it is needed for weekends marking. TODO: workaround?
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/ARIMAForecast.R"))
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/ANNForecast.R"))
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/ETSForecast.R"))
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/ForecastMetric.R"))
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/DataPreprocessing.R"))
source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/LinearRegressionForecast.R"))

# Forecast model creation, forecasting and scoring for the single time series.
testing.of.single.timeseries <- function(time.series) {
  example.ts <- list()
  example.ts$series <- time.series
  example.ts$end <- as.POSIXct("2017-11-06 10:41:23", "%Y-%m-%d %H:%M:%S")
  start.time <- example.ts$end - length(time.series) * 60 * 60 + 3600
  example.ts$start <- start.time
  example.ts$discretion <- 3600
  
  # Splitting time series for training and test data sets
  test.set.length.days <- 7
  train.ts <- list()
  train.ts$series <- window(example.ts$series, as.numeric(example.ts$start), as.numeric(example.ts$end - test.set.length.days * 24 * 3600))
  train.ts$start <- example.ts$start
  train.ts$end <- example.ts$end - test.set.length.days * 24 * 3600
  train.ts$discretion <- example.ts$discretion
  
  test.ts <- list()
  test.ts$series <- window(example.ts$series, as.numeric(example.ts$end - test.set.length.days * 24 * 3600 + 1), as.numeric(example.ts$end))
  test.ts$start <- example.ts$end - test.set.length.days * 24 * 3600 + 1
  test.ts$end <- example.ts$end
  test.ts$discretion <- example.ts$discretion
  
  # Deriving different forecasting models and forecasts with prediction intervals
  #ann.forecast.res = ann.forecast(train.ts, length(test.ts$series))
  
  
  ets.start.time <- Sys.time()
  ets.forecast.res = ets.forecast(train.ts$series, length(test.ts$series))
  ets.end.time <- Sys.time()
  
  sarima.start.time <- Sys.time()
  sarima.forecast.res = arima.forecast(example.ts, length(test.ts$series), "SARIMA")
  sarima.end.time <- Sys.time()
  
  sarimaoa.start.time <- Sys.time()
  sarimaoa.forecast.res = arima.forecast(example.ts, length(test.ts$series), "SOAARIMA")
  sarimaoa.end.time <- Sys.time()
  
  sarima.garch.start.time <- Sys.time()
  sarima.garch.forecast.res = arima.forecast(example.ts, length(test.ts$series), "SARIMA+GARCH")
  sarima.garch.end.time <- Sys.time()
  
  sarimaoa.garch.start.time <- Sys.time()
  sarimaoa.garch.forecast.res = arima.forecast(example.ts, length(test.ts$series), "SOAARIMA+GARCH")
  sarimaoa.garch.end.time <- Sys.time()
  
  sarfima.start.time <- Sys.time()
  sarfima.forecast.res = arima.forecast(example.ts, length(test.ts$series), "SARFIMA")
  sarfima.end.time <- Sys.time()
  
  sarfima.garch.start.time <- Sys.time()
  sarfima.garch.forecast.res = arima.forecast(example.ts, length(test.ts$series), "SARFIMA+GARCH")
  sarfima.garch.end.time <- Sys.time()
  
  lm.start.time <- Sys.time()
  lm.forecast.res = linear.regression.forecast(example.ts, length(test.ts$series))
  lm.end.time <- Sys.time()

  # Estimating running time of prediction algorithms
  ets.duration <- difftime(ets.end.time, ets.start.time, units = "secs")
  sarima.duration <- difftime(sarima.end.time, sarima.start.time, units = "secs")
  sarimaoa.duration <- difftime(sarimaoa.end.time, sarimaoa.start.time, units = "secs")
  sarima.garch.duration <- difftime(sarima.garch.end.time, sarima.garch.start.time, units = "secs")
  sarimaoa.garch.duration <- difftime(sarimaoa.garch.end.time, sarimaoa.garch.start.time, units = "secs")
  sarfima.duration <- difftime(sarfima.end.time, sarfima.start.time, units = "secs")
  sarfima.garch.duration <- difftime(sarfima.garch.end.time, sarfima.garch.start.time, units = "secs")
  lm.duration <- difftime(lm.end.time, lm.start.time, units = "secs")
  
  # Scoring of forecasting models
  actual.vals <- test.ts$series
  alpha <- 0.05 # Alpha could be changed to loosen or strenghten the interval scoring
  
  #ann.lb.95 <- ann.forecast.res$lower[,2]
  #ann.ub.95 <- ann.forecast.res$upper[,2]
  ets.lb.95 <- ets.forecast.res$lower[,2]
  ets.ub.95 <- ets.forecast.res$upper[,2]
  sarima.lb.95 <- sarima.forecast.res$lower[,2]
  sarima.ub.95 <- sarima.forecast.res$upper[,2]
  sarimaoa.lb.95 <- sarimaoa.forecast.res$lower[,2]
  sarimaoa.ub.95 <- sarimaoa.forecast.res$upper[,2]
  sarima.garch.lb.95 <- sarima.garch.forecast.res$lower[,2]
  sarima.garch.ub.95 <- sarima.garch.forecast.res$upper[,2]
  sarimaoa.garch.lb.95 <- sarimaoa.garch.forecast.res$lower[,2]
  sarimaoa.garch.ub.95 <- sarimaoa.garch.forecast.res$upper[,2]
  sarfima.lb.95 <- sarfima.forecast.res$lower
  sarfima.ub.95 <- sarfima.forecast.res$upper
  sarfima.garch.lb.95 <- sarfima.garch.forecast.res$lower
  sarfima.garch.ub.95 <- sarfima.garch.forecast.res$upper
  lm.lb.95 <- lm.forecast.res$lower
  lm.ub.95 <- lm.forecast.res$upper
  
  #ann.score <- interval.score(ann.lb.95, ann.ub.95, actual.vals, alpha)
  ets.score <- interval.score(ets.lb.95, ets.ub.95, actual.vals, alpha)
  sarima.score <- interval.score(sarima.lb.95, sarima.ub.95, actual.vals, alpha)
  sarimaoa.score <- interval.score(sarimaoa.lb.95, sarimaoa.ub.95, actual.vals, alpha)
  sarima.garch.score <- interval.score(sarima.garch.lb.95, sarima.garch.ub.95, actual.vals, alpha)
  sarimaoa.garch.score <- interval.score(sarimaoa.garch.lb.95, sarimaoa.garch.ub.95, actual.vals, alpha)
  sarfima.score <- interval.score(sarfima.lb.95, sarfima.ub.95, actual.vals, alpha)
  sarfima.garch.score <- interval.score(sarfima.garch.lb.95, sarfima.garch.ub.95, actual.vals, alpha)
  lm.score <- interval.score(lm.lb.95, lm.ub.95, actual.vals, alpha)
  
  scores <- data.frame(ets.score = ets.score,
                    sarima.score = sarima.score,
                    sarimaoa.score = sarimaoa.score,
                    sarima.garch.score = sarima.garch.score,
                    sarimaoa.garch.score = sarimaoa.garch.score,
                    sarfima.score = sarfima.score,
                    sarfima.garch.score = sarfima.garch.score,
                    lm.score = lm.score,
                    ets.duration = ets.duration,
                    sarima.duration = sarima.duration,
                    sarimaoa.duration = sarimaoa.duration,
                    sarima.garch.duration = sarima.garch.duration,
                    sarimaoa.garch.duration = sarimaoa.garch.duration,
                    sarfima.duration = sarfima.duration,
                    sarfima.garch.duration = sarfima.garch.duration,
                    lm.duration = lm.duration)
                    #ann.score = ann.score)
  models.boundaries <- data.frame(ets.lb.95 = ets.lb.95,
                                  ets.ub.95 = ets.ub.95,
                                  sarima.lb.95 = sarima.lb.95,
                                  sarima.ub.95 = sarima.ub.95,
                                  sarimaoa.lb.95 = sarimaoa.lb.95,
                                  sarimaoa.ub.95 = sarimaoa.ub.95,
                                  sarima.garch.lb.95 = sarima.garch.lb.95,
                                  sarima.garch.ub.95 = sarima.garch.ub.95,
                                  sarimaoa.garch.lb.95 = sarimaoa.garch.lb.95,
                                  sarimaoa.garch.ub.95 = sarimaoa.garch.ub.95,
                                  sarfima.lb.95 = sarfima.lb.95,
                                  sarfima.ub.95 = sarfima.ub.95,
                                  sarfima.garch.lb.95 = sarfima.garch.lb.95,
                                  sarfima.garch.ub.95 = sarfima.garch.ub.95,
                                  lm.lb.95 = lm.lb.95,
                                  lm.ub.95 = lm.ub.95)
  res <- list()
  res$scores <- scores
  res$models.boundaries <- models.boundaries
  
  return(res)
}

# Function to extract scores from the list (as list)
extract.scores <- function(list.elem) {
  return(list.elem$scores)
}

# Function to extract models boundaries from the list (as list)
extract.models <- function(list.elem) {
  return(list.elem$models.boundaries)
}

# Construction of the scoring table encompassing scores for all forecasting methods tested with
# identified best methods for each data set.
overall.testing <- function(list.of.data) {
  scores.and.models <- lapply(list.of.data, testing.of.single.timeseries)
  scores <- lapply(scores.and.models, extract.scores)
  score.table <- as.data.frame(do.call(rbind, scores))
  score.table$min.score.index <- apply(score.table, 1, which.min)
  models.boundaries <- lapply(scores.and.models, extract.models)
  res <- list()
  res$models.boundaries <- models.boundaries
  res$scores <- score.table
  return(res)
}

# Main script for testing
file.path <- "/home/remit/dissCloud/Instana/data/metrics.csv"
#file.path <- "D:/Business/Core/testdata/metrics.csv"
data.raw <- read.csv2(file = file.path, header = F, sep = ",", stringsAsFactors = F)
lst <- ts.preprocessing.matrix.Instana(data.raw)
#outliers test
#tst.sample <- list(lst[[11]])
#tst.sample[[1]][202] <- 0.1
#scores.and.models <- overall.testing(tst.sample)
scores.and.models <- overall.testing(lst)#1:15
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
pairs(scores.tst[,1:8])
# Pairwise comparison of models in terms of fitting duration
pairs(scores.tst[,9:16])


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
# 2. Add SSA non-parametric method https://ru.wikipedia.org/wiki/SSA_(%D0%BC%D0%B5%D1%82%D0%BE%D0%B4) (https://cran.r-project.org/web/packages/Rssa/Rssa.pdf) - R package Rssa
# 3. Add SVMs models (?) if not too much time https://www.elen.ucl.ac.be/Proceedings/esann/esannpdf/es2010-28.pdf 