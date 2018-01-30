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
library(influxdbr)
influx.con <- influx_connection(scheme = "http", host = "34.217.33.66", port = 32589, user = "root", pass = "root")
dbs <- show_databases(influx.con)[[1]]
db.name <- dbs[which(dbs != "_internal")]

# Function to get all possible values for a specific metric for a specific node for a specific namespace for a specific pod name
get.values.by.pod.name <- function(pod.name = "",
                                   influx.con = NULL,
                                   db.name = "",
                                   metric = "",
                                   node = "",
                                   name.space = "") {
  time.series.df <- tryCatch(
    { 
      time.series.raw <- influx_select(influx.con,
                                       db.name,
                                       field_keys = "value",
                                       rp = "default",
                                       measurement = metric,
                                       where = paste0("type = 'pod' AND pod_name = '",
                                                      pod.name,
                                                      "' AND namespace_name = '",
                                                      name.space,
                                                      "' AND nodename = '",
                                                      node,
                                                      "'"),
                                       group_by = "*")
      time.series <- time.series.raw[[1]][[1]]
      data.frame(pod.name = rep(pod.name, length(time.series)),
                 time = as.numeric(time(time.series)),
                 value = time.series$value)
    },
    error=function(cond) {
      er.ret <- data.frame(pod.name = character(),
                           time = numeric(),
                           value = numeric())
      return(er.ret)
    })

  return(time.series.df)
}

# Function to get all possible values for a specific metric for a specific node for a specific namespace
get.values.by.namespace <- function(name.space = "",
                                    influx.con = NULL,
                                    db.name = "",
                                    pod.names = NULL,
                                    metric = "",
                                    node = "") {
  if(is.null(pod.names)) {
    pod.names.raw <- influx_query(influx.con,
                                  db = db.name,
                                  query = paste0("SHOW TAG VALUES FROM uptime WITH KEY = pod_name WHERE nodename = '", node, "' AND namespace_name = '", name.space, "'"),
                                  return_xts = FALSE)
    pod.names <- pod.names.raw[[1]]$value
  }
  
  vals.by.name.space.lst <- lapply(pod.names, get.values.by.pod.name, influx.con, db.name, metric, node, name.space)
  vals.by.name.space <- do.call("rbind", vals.by.name.space.lst)
  vals.by.name.space$name.space <- rep(name.space, nrow(vals.by.name.space))
  return(vals.by.name.space)
}

# Function to get all possible values for a specific metric for a specific node
get.values.by.node <- function(node = "",
                               influx.con = NULL,
                               db.name = "",
                               name.spaces = NULL,
                               pod.names = NULL,
                               metric = "") {
  if(is.null(name.spaces)) {
    ns.raw <- influx_query(influx.con,
                           db = db.name,
                           query = paste0("SHOW TAG VALUES FROM uptime WITH KEY = namespace_name WHERE nodename = '",node,"'"),
                           return_xts = FALSE)
    name.spaces <- ns.raw[[1]]$value
  }
  
  vals.by.node.lst <- lapply(name.spaces, get.values.by.namespace, influx.con, db.name, pod.names, metric, node)
  vals.by.node <- do.call("rbind", vals.by.node.lst)
  vals.by.node$node <- rep(node, nrow(vals.by.node))
  return(vals.by.node)
}

# Function to get all possible values for a specific metric
get.values.by.metric <- function(metric = "",
                                 influx.con = NULL,
                                 db.name = "",
                                 nodes = NULL,
                                 name.spaces = NULL,
                                 pod.names = NULL) {
  if(is.null(nodes)) {
    nodes.raw <- influx_query(influx.con,
                                   db = db.name,
                                   query = 'SHOW TAG VALUES FROM uptime WITH KEY = nodename',
                                   return_xts = FALSE)
    nodes <- nodes.raw[[1]]$value
  }
  
  vals.by.metric.lst <- lapply(nodes, get.values.by.node, influx.con, db.name, name.spaces, pod.names, metric)
  vals.by.metric <- do.call("rbind", vals.by.metric.lst)
  vals.by.metric$metric <- rep(metric, nrow(vals.by.metric))
  return(vals.by.metric)
}

# Function to get all possible value from InfluxDB, includes some filters (by default - extracts everything)
get.values <- function(influx.con = NULL,
                       db.name = "",
                       metrics = NULL,
                       nodes = NULL,
                       name.spaces = NULL,
                       pod.names = NULL) {
  
  if(is.null(metrics)) {
    metrics.raw <- influx_query(influx.con,
                                db = db.name,
                                query = 'SHOW MEASUREMENTS',
                                return_xts = FALSE)
    
    metrics <- metrics.raw[[1]]$name
  }
  
  vals.lst <- lapply(metrics, get.values.by.metric, influx.con, db.name, nodes, name.spaces, pod.names)
  vals <- do.call("rbind", vals.lst)
  return(vals)
}

# Get all the metrics data from influxdb: 1, 6, 
performance.data <- get.values(influx.con, db.name)
#adjustment for the influxdb machine TODO: delete when it is not necessary anymore
performance.data$time <- performance.data$time - 3600

# Getting requests data from the mongo
library(mongolite)
library(dplyr)
library(xts)
mongo.url <- "mongodb://MongoServer3:cloud%40SecureMongod75@34.217.33.66"
collection.name <- "kubeserver"
db.names <- c("t2small", "t2large", "t22xlarge")

extract.data.for.db <- function(db.name, mongo.url, collection.name) {
  conMongo.requests <- mongo(collection.name, db.name, mongo.url)
  
  db.data <- list()
  if(conMongo.requests$count() > 0) {
    Requests.data <- conMongo.requests$find()
    
    errors.ts.adapted <- Requests.data$aggregate$errors$ETIMEDOUT
    errors.ts.adapted.vec <- as.vector(coredata(errors.ts.adapted))
    errors.ts.adapted.vec[is.na(errors.ts.adapted.vec)] <- 0
    errors.ts <- xts(errors.ts.adapted.vec, strptime(Requests.data$aggregate$timestamp, "%Y-%m-%dT%H:%M:%S"))
    
    latency.ts.adapted <- Requests.data$aggregate$latency$p99
    latency.ts.adapted.vec <- as.vector(coredata(latency.ts.adapted))
    latency.ts.adapted.vec[is.na(latency.ts.adapted.vec)] <- 0
    latency.ts <- xts(latency.ts.adapted.vec, strptime(Requests.data$aggregate$timestamp, "%Y-%m-%dT%H:%M:%S"))
    
    requests.ts <- xts(Requests.data$aggregate$rps, strptime(Requests.data$aggregate$timestamp, "%Y-%m-%dT%H:%M:%S"))
    
    db.data$type <- db.name
    db.data$errors.ts <- errors.ts[,1]
    db.data$latency.ts <- latency.ts[,1]
    db.data$requests.ts <- requests.ts$count
    
  }
  
  return(db.data)
}

data.for.vm.types <- lapply(db.names, extract.data.for.db, mongo.url, collection.name)

# TODO: add wrapper functions to make graphs for diff. metrics and analyse time series (deriving the model)
# TODO: add flexible analysis for diff. metrics and pods

preprocess.data.for.metric.and.pod.and.vm.type <- function(performance.data,
                                                           data.for.vm.types,
                                                           metric,
                                                           pod = NULL,
                                                           vm.type) {
  performance.data.for.metric.and.vm <- NULL
  if(is.null(pod)) {
    performance.data.for.metric.and.vm <- performance.data[(performance.data$metric == metric), ]
    performance.data.for.metric.and.vm.grpd <- group_by(performance.data.for.metric.and.vm, pod.name)
    performance.data.for.metric.and.vm <- summarise(performance.data.for.metric.and.vm.grpd,
                                                    value = sum(value),
                                                    metric = unique(metric)
                                                    )
  } else {
    performance.data.for.metric.and.vm <- performance.data[(performance.data$metric == metric) & (performance.data$pod.name == pod), ]
  }
  
  performance.data.time <- as.POSIXct(performance.data.for.metric.and.vm$time, origin="1970-01-01")
  performance.data.ts <- xts(performance.data.for.metric.and.vm$value, performance.data.time)
  
  data.index <- which(grepl(vm.type, data.for.vm.types))
  data.for.vm.type <- data.for.vm.types[[data.index]]
  
  # Adjusting timelines for different timeseries in order to conduct the analysis
  performance.data.ts.adapted <- as.numeric(performance.data.time)
  requests.ts.adapted <- as.numeric(time(data.for.vm.type$requests.ts))
  
  baseline.ts <- NULL
  adaptable.ts <- NULL
  adaptable.vals <- list()
  requests.ts <- data.for.vm.type$requests.ts
  errors.ts <- data.for.vm.type$errors.ts
  latency.ts <- data.for.vm.type$latency.ts
  
  if(length(requests.ts.adapted) < length(performance.data.ts.adapted)) {
    baseline.ts <- requests.ts.adapted
    adaptable.ts <- performance.data.ts.adapted
    adaptable.vals$vals <- performance.data.for.metric.and.vm$value
  } else {
    baseline.ts <- performance.data.ts.adapted
    adaptable.ts <- requests.ts.adapted
    adaptable.vals$rps <- requests.ts
    adaptable.vals$errors <- errors.ts
    adaptable.vals$latency <- latency.ts
  }
  
  baseline.ts.diff <- diff(baseline.ts)
  baseline.ts.diff.half.intvl <- baseline.ts.diff / 2
  intervals <- data.frame(lb = baseline.ts,
                          ub = baseline.ts)
  intervals[1,"lb"] <- 0
  intervals[2:nrow(intervals),"lb"] <- intervals[2:nrow(intervals),"lb"] - ceiling(baseline.ts.diff.half.intvl)
  intervals[1:(nrow(intervals) - 1),"ub"] <- intervals[1:(nrow(intervals) - 1),"ub"] + floor(baseline.ts.diff.half.intvl)
  intervals[nrow(intervals),"ub"] <- Inf
  
  check.interval <- function(value, intervals) {
    which((value >= intervals$lb) & (value < intervals$ub))
  }
  
  marked.ts.values <- unlist(sapply(adaptable.ts, check.interval, intervals))
  
  adapt.ts.vals <- function(adaptable.vals, adaptable.ts, marked.ts.values) {
    adabtable.df <- data.frame(adaptable.time.series = adaptable.ts,
                               adaptable.values = adaptable.vals,
                               interval = marked.ts.values)
    adabtable.df.grp <- group_by(adabtable.df, adabtable.df$interval)
    names(adabtable.df.grp) <- c("adaptable.time.series", "adaptable.values", "interval", "group")
    adabtable.df.grp.summary <- summarise(adabtable.df.grp, new.val = mean(adaptable.values))
    names(adabtable.df.grp.summary) <- c("interval", "val")
    '%!in%' <- function(x,y)!('%in%'(x,y))
    adj.indices <- which(seq(1,length(baseline.ts)) %!in% adabtable.df.grp.summary$interval)
    
    fill.in.gaps <- function(index.val, adabtable.df.grp.summary) {
      ret <- NULL
      if(index.val < min(adabtable.df.grp.summary$interval)) {
        ret <- adabtable.df.grp.summary[adabtable.df.grp.summary$interval == min(adabtable.df.grp.summary$interval),"val"][[1]]
      } else if(index.val > max(adabtable.df.grp.summary$interval)) {
        ret <- adabtable.df.grp.summary[adabtable.df.grp.summary$interval == max(adabtable.df.grp.summary$interval),"val"][[1]]
      } else {
        differences <- data.frame(intvl = adabtable.df.grp.summary$interval,
                                  diff = adabtable.df.grp.summary$interval - index.val)
        differences.pos <- differences[differences$diff > 0,]
        differences.neg <- differences[differences$diff < 0,]
        start.ind <- differences.neg[which.max(differences.neg$diff), "intvl"]
        end.ind <- differences.pos[which.min(differences.neg$diff), "intvl"]
        start.val <- adabtable.df.grp.summary[adabtable.df.grp.summary$interval == start.ind,"val"][[1]]
        end.val <- adabtable.df.grp.summary[adabtable.df.grp.summary$interval == end.ind,"val"][[1]]
        ret <- mean(c(start.val, end.val))
      }
      
      return(ret)
    }
    
    vals.for.gaps <- unlist(sapply(adj.indices, fill.in.gaps, adabtable.df.grp.summary))
    extended.grps <- data.frame(interval = adj.indices,
                                val = vals.for.gaps)
    adabtable.df.grp.summary.adjusted <- rbind(adabtable.df.grp.summary, extended.grps)
    adabtable.df.grp.summary.adjusted <- adabtable.df.grp.summary.adjusted[with(adabtable.df.grp.summary.adjusted, order(interval)), ]
    
    adapted.ts <- xts(adabtable.df.grp.summary.adjusted$val,
                      as.POSIXct(baseline.ts,  origin="1970-01-01"))
  }
  
  adapted.ts <- lapply(adaptable.vals, adapt.ts.vals, adaptable.ts, marked.ts.values)
  
  # Reassigning adapted timelines to sources
  if(length(requests.ts.adapted) < length(performance.data.ts.adapted)) {
    performance.data.ts <- adapted.ts$vals
  } else {
    latency.ts <- adapted.ts$latency
    requests.ts <- adapted.ts$rps
    errors.ts <- adapted.ts$errors
  }
  
  result <- list()
  result$latency.ts <- latency.ts
  result$requests.ts <- requests.ts
  result$errors.ts <- errors.ts
  result$performance.data.ts <- performance.data.ts
  result$performance.data.metric <- metric
  
  return(result)
}


list.available.pods.for.vm.type <- function(vm.type, performance.data) {
  vm.types <- c("t2small", "t2large", "t22xlarge")
  nodes <- c("ip-172-31-41-54", "ip-172-31-44-145", "ip-172-31-34-70")
  node <- nodes[which(vm.types == vm.type)]
  
  pods <- unique(performance.data[performance.data$node == node, "pod.name"])
  return(pods)
}

list.available.pods.for.vm.type("t2small", performance.data)
list.available.pods.for.vm.type("t2large", performance.data)
list.available.pods.for.vm.type("t22xlarge", performance.data)

vm.types <- c("t2small", "t2large", "t22xlarge")
nodes <- c("ip-172-31-41-54", "ip-172-31-44-145", "ip-172-31-34-70")

t2.small.data <- preprocess.data.for.metric.and.pod.and.vm.type(performance.data[performance.data$node == nodes[which(vm.types == "t2small")],],
                                                                data.for.vm.types,
                                                                "cpu/usage_rate",
                                                                "server-7f95cf4d9d-zf89g",
                                                                "t2small")
t2.large.data <- preprocess.data.for.metric.and.pod.and.vm.type(performance.data[performance.data$node == nodes[which(vm.types == "t2large")],],
                                                                data.for.vm.types,
                                                                "cpu/usage_rate",
                                                                "server-7f95cf4d9d-8zlgz",
                                                                "t2large")
t2.2xlarge.data <- preprocess.data.for.metric.and.pod.and.vm.type(performance.data[performance.data$node == nodes[which(vm.types == "t22xlarge")],],
                                                                data.for.vm.types,
                                                                "cpu/usage_rate",
                                                                "server-7f95cf4d9d-7rk6j",
                                                                "t22xlarge")


par(mfrow = c(4,3))
plot(t2.small.data$performance.data.ts)
plot(t2.large.data$performance.data.ts)
plot(t2.2xlarge.data$performance.data.ts)

plot(t2.small.data$errors.ts)
plot(t2.large.data$errors.ts)
plot(t2.2xlarge.data$errors.ts)

plot(t2.small.data$latency.ts)
plot(t2.large.data$latency.ts)
plot(t2.2xlarge.data$latency.ts)

plot(t2.small.data$requests.ts)
plot(t2.large.data$requests.ts)
plot(t2.2xlarge.data$requests.ts)

start.t <- as.numeric(strptime("2018-01-29T12:01:00","%Y-%m-%dT%H:%M:%S"))
end.t <- as.numeric(strptime("2018-01-29T12:43:00","%Y-%m-%dT%H:%M:%S"))
requests.ts.regular <- window(testing.data$requests.ts, start = start.t, end = end.t)
performance.data.ts.regular <- window(testing.data$performance.data.ts, start = start.t, end = end.t)

library(astsa)
predictor <- as.vector(coredata(requests.ts.regular))
result <- as.vector(coredata(performance.data.ts.regular))
#lag2.plot(diff(predictor), result, 20)

predictor.arima <- forecast::Arima(predictor,
                order = c(0, 1, 0),
                method = "CSS")
predictor.residuals <- predictor.arima$residuals
result.residuals <- residuals(forecast::Arima(result, model=predictor.arima))
par(mfrow = c(1,1))
ccf(predictor.residuals, result.residuals,na.action=na.omit)

#Deriving the service performance model
# Service model includes:
# @ for each tested VM type:
#   - overall bottleneck (numer of requests when the microservices achieves the peak performance)
#   - performance chracteristics as dependent on the number of requests

# To determine the bottleneck
t2.small.QoS.errors.total <- 1000
t2.small.total.errors <- cumsum(coredata(t2.small.data$errors.ts))
t2.small.bottleneck.index <- min(which(t2.small.total.errors > t2.small.QoS.errors.total)) - 1
t2.small.bottleneck <- t2.small.data$requests.ts[bottleneck.index][[1]]

#SHOW MEASUREMENTS