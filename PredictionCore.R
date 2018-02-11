suppressMessages(library(imputeTS))
suppressMessages(library(ggplot2))
suppressMessages(library(tseries))
suppressMessages(library(forecast))

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
  
  ssa.start.time <- Sys.time()
  ssa.forecast.res = ssa.forecast(train.ts, length(test.ts$series))
  ssa.end.time <- Sys.time()
  
  svr.start.time <- Sys.time()
  svr.forecast.res = svr.forecast(train.ts, length(test.ts$series))
  svr.end.time <- Sys.time()

  # Estimating running time of prediction algorithms
  ets.duration <- difftime(ets.end.time, ets.start.time, units = "secs")
  sarima.duration <- difftime(sarima.end.time, sarima.start.time, units = "secs")
  sarimaoa.duration <- difftime(sarimaoa.end.time, sarimaoa.start.time, units = "secs")
  sarima.garch.duration <- difftime(sarima.garch.end.time, sarima.garch.start.time, units = "secs")
  sarimaoa.garch.duration <- difftime(sarimaoa.garch.end.time, sarimaoa.garch.start.time, units = "secs")
  sarfima.duration <- difftime(sarfima.end.time, sarfima.start.time, units = "secs")
  sarfima.garch.duration <- difftime(sarfima.garch.end.time, sarfima.garch.start.time, units = "secs")
  lm.duration <- difftime(lm.end.time, lm.start.time, units = "secs")
  ssa.duration <- difftime(ssa.end.time, ssa.start.time, units = "secs")
  svr.duration <- difftime(svr.end.time, svr.start.time, units = "secs")
  
  # Scoring of forecasting models
  actual.vals <- test.ts$series
  alpha <- 0.05 # Alpha could be changed to loosen or strenghten the interval scoring
  
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
  ssa.lb.95 <- ssa.forecast.res$lower[,2]
  ssa.ub.95 <- ssa.forecast.res$upper[,2]
  svr.lb.95 <- svr.forecast.res$lower[,2]
  svr.ub.95 <- svr.forecast.res$upper[,2]
  
  ets.score <- interval.score(ets.lb.95, ets.ub.95, actual.vals, alpha)
  sarima.score <- interval.score(sarima.lb.95, sarima.ub.95, actual.vals, alpha)
  sarimaoa.score <- interval.score(sarimaoa.lb.95, sarimaoa.ub.95, actual.vals, alpha)
  sarima.garch.score <- interval.score(sarima.garch.lb.95, sarima.garch.ub.95, actual.vals, alpha)
  sarimaoa.garch.score <- interval.score(sarimaoa.garch.lb.95, sarimaoa.garch.ub.95, actual.vals, alpha)
  sarfima.score <- interval.score(sarfima.lb.95, sarfima.ub.95, actual.vals, alpha)
  sarfima.garch.score <- interval.score(sarfima.garch.lb.95, sarfima.garch.ub.95, actual.vals, alpha)
  lm.score <- interval.score(lm.lb.95, lm.ub.95, actual.vals, alpha)
  ssa.score <- interval.score(ssa.lb.95, ssa.ub.95, actual.vals, alpha)
  svr.score <- interval.score(svr.lb.95, svr.ub.95, actual.vals, alpha)
  
  scores <- data.frame(ets.score = ets.score,
                       sarima.score = sarima.score,
                       sarimaoa.score = sarimaoa.score,
                       sarima.garch.score = sarima.garch.score,
                       sarimaoa.garch.score = sarimaoa.garch.score,
                       sarfima.score = sarfima.score,
                       sarfima.garch.score = sarfima.garch.score,
                       lm.score = lm.score,
                       ssa.score = ssa.score,
                       svr.score = svr.score,
                       ets.duration = ets.duration,
                       sarima.duration = sarima.duration,
                       sarimaoa.duration = sarimaoa.duration,
                       sarima.garch.duration = sarima.garch.duration,
                       sarimaoa.garch.duration = sarimaoa.garch.duration,
                       sarfima.duration = sarfima.duration,
                       sarfima.garch.duration = sarfima.garch.duration,
                       lm.duration = lm.duration,
                       ssa.duration = ssa.duration,
                       svr.duration = svr.duration)
  
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
                                  lm.ub.95 = lm.ub.95,
                                  ssa.lb.95 = ssa.lb.95,
                                  ssa.ub.95 = ssa.ub.95,
                                  svr.lb.95 = svr.lb.95,
                                  svr.ub.95 = svr.ub.95)
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
overall.testing <- function(list.of.data, cluster) {
  scores.and.models <- parLapply(cluster, list.of.data, testing.of.single.timeseries)
  #scores.and.models <- lapply(list.of.data, testing.of.single.timeseries)
  scores <- lapply(scores.and.models, extract.scores)
  score.table <- as.data.frame(do.call(rbind, scores))
  #score.table$min.score.index <- apply(score.table, 1, which.min)
  models.boundaries <- lapply(scores.and.models, extract.models)
  res <- list()
  res$models.boundaries <- models.boundaries
  res$scores <- score.table
  return(res)
}