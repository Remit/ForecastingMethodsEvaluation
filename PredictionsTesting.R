library(imputeTS)
library(ggplot2)
library(tseries)
library(fGarch)
library(forecast)

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
  
  sarima.garch.start.time <- Sys.time()
  sarima.garch.forecast.res = arima.forecast(example.ts, length(test.ts$series), "SARIMA+GARCH")
  sarima.garch.end.time <- Sys.time()
  
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
  ets.duration <- ets.end.time - ets.start.time
  sarima.duration <- sarima.end.time - sarima.start.time
  sarima.garch.duration <- sarima.garch.end.time - sarima.garch.start.time
  sarfima.duration <- sarfima.end.time - sarfima.start.time 
  sarfima.garch.duration <- sarfima.garch.end.time - sarfima.garch.start.time
  lm.duration <- lm.end.time - lm.start.time
  
  # Scoring of forecasting models
  actual.vals <- test.ts$series
  alpha <- 0.05 # Alpha could be changed to loosen or strenghten the interval scoring
  
  #ann.lb.95 <- ann.forecast.res$lower[,2]
  #ann.ub.95 <- ann.forecast.res$upper[,2]
  ets.lb.95 <- ets.forecast.res$lower[,2]
  ets.ub.95 <- ets.forecast.res$upper[,2]
  sarima.lb.95 <- sarima.forecast.res$lower[,2]
  sarima.ub.95 <- sarima.forecast.res$upper[,2]
  sarima.garch.lb.95 <- sarima.garch.forecast.res$lower[,2]
  sarima.garch.ub.95 <- sarima.garch.forecast.res$upper[,2]
  sarfima.lb.95 <- sarfima.forecast.res$lower
  sarfima.ub.95 <- sarfima.forecast.res$upper
  sarfima.garch.lb.95 <- sarfima.garch.forecast.res$lower
  sarfima.garch.ub.95 <- sarfima.garch.forecast.res$upper
  lm.lb.95 <- lm.forecast.res$lower
  lm.ub.95 <- lm.forecast.res$upper
  
  #ann.score <- interval.score(ann.lb.95, ann.ub.95, actual.vals, alpha)
  ets.score <- interval.score(ets.lb.95, ets.ub.95, actual.vals, alpha)
  sarima.score <- interval.score(sarima.lb.95, sarima.ub.95, actual.vals, alpha)
  sarima.garch.score <- interval.score(sarima.garch.lb.95, sarima.garch.ub.95, actual.vals, alpha)
  sarfima.score <- interval.score(sarfima.lb.95, sarfima.ub.95, actual.vals, alpha)
  sarfima.garch.score <- interval.score(sarfima.garch.lb.95, sarfima.garch.ub.95, actual.vals, alpha)
  lm.score <- interval.score(lm.lb.95, lm.ub.95, actual.vals, alpha)
  
  res <- data.frame(ets.score = ets.score,
                    sarima.score = sarima.score,
                    sarima.garch.score = sarima.garch.score,
                    sarfima.score = sarfima.score,
                    sarfima.garch.score = sarfima.garch.score,
                    lm.score = lm.score,
                    ets.duration = ets.duration,
                    sarima.duration = sarima.duration,
                    sarima.garch.duration = sarima.garch.duration,
                    sarfima.duration = sarfima.duration,
                    sarfima.garch.duration = sarfima.garch.duration,
                    lm.duration = lm.duration)
                    #ann.score = ann.score)
  return(res)
}

# Construction of the scoring table encompassing scores for all forecasting methods tested with
# identified best methods for each data set.
overall.testing <- function(list.of.data) {
  score.table <- lapply(list.of.data, testing.of.single.timeseries)
  score.table <- as.data.frame(do.call(rbind, score.table))
  score.table$min.score.index <- apply(score.table, 1, which.min)
  return(score.table)
}

# Main script for testing
file.path <- "/home/remit/dissCloud/Instana/data/metrics.csv"
data.raw <- read.csv2(file = file.path, header = F, sep = ",", stringsAsFactors = F)
lst <- ts.preprocessing.matrix.Instana(data.raw)
score.table <- overall.testing(lst[8])