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
  ann.forecast.res = ann.forecast(train.ts, length(test.ts$series))
  ets.forecast.res = ets.forecast(train.ts$series, length(test.ts$series))
  arima.forecast.res = arima.forecast(train.ts, length(test.ts$series))

  # Scoring of forecasting models
  actual.vals <- test.ts$series
  alpha <- 0.05 # Alpha could be changed to loosen or strenghten the interval scoring
  
  ann.lb.95 <- ann.forecast.res$lower[,2]
  ann.ub.95 <- ann.forecast.res$upper[,2]
  ets.lb.95 <- ets.forecast.res$lower[,2]
  ets.ub.95 <- ets.forecast.res$upper[,2]
  arima.lb.95 <- arima.forecast.res$lower[,2]
  arima.ub.95 <- arima.forecast.res$upper[,2]
  
  ann.score <- interval.score(ann.lb.95, ann.ub.95, actual.vals, alpha)
  ets.score <- interval.score(ets.lb.95, ets.ub.95, actual.vals, alpha)
  arima.score <- interval.score(arima.lb.95, arima.ub.95, actual.vals, alpha)
  
  res <- data.frame(ets.score = ets.score, arima.score = arima.score, ann.score = ann.score)
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
score.table <- overall.testing(lst[1])