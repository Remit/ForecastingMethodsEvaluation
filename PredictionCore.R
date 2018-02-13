# Forecast model creation, forecasting and scoring for the single time series.
testing.of.single.timeseries <- function(time.series, start.time, prediction.steps.num, influx.con, db.name) {
  example.ts <- list()
  example.ts$series <- time.series
  example.ts$start <- start.time
  example.ts$end <- start.time + length(time.series) * 3600
  example.ts$discretion <- 3600
  
  # Using user-specified value for the number of predicted steps
  train.ts <- list()
  test.ts <- list()
  pred.test.interval <- 0
  if(prediction.steps.num >= 0) {
    pred.test.interval <- (prediction.steps.num + 1) * 3600
  } else {
    test.set.length.days <- 7
    pred.test.interval <- test.set.length.days * 24 * 3600
  }
  
  # Splitting time series for training and test data sets
  train.ts$series <- window(example.ts$series, as.numeric(example.ts$start), as.numeric(example.ts$end - pred.test.interval))
  train.ts$start <- example.ts$start
  train.ts$end <- example.ts$end - pred.test.interval
  train.ts$discretion <- example.ts$discretion
  
  test.ts$series <- window(example.ts$series, as.numeric(example.ts$end - pred.test.interval + 3600), as.numeric(example.ts$end))
  test.ts$start <- example.ts$end - pred.test.interval + 3600
  test.ts$end <- example.ts$end
  test.ts$discretion <- example.ts$discretion
  
  actual.vals <- test.ts$series
  alpha <- 0.05 # Alpha could be changed to loosen or strenghten the interval scoring
  train.length <- length(train.ts$series)
  actual.vals.ext <- c(as.vector(train.ts$series), as.vector(test.ts$series))
  timeline.ext <- c(as.vector(time(train.ts$series)), as.vector(time(test.ts$series)))

  # Deriving different forecasting models and forecasts with prediction intervals
  # TODO:
  # 2. adding point forecasts
  
  ets.start.time <- Sys.time()
  ets.forecast.res = ets.forecast(train.ts$series, length(test.ts$series))
  ets.end.time <- Sys.time()
  ets.duration <- difftime(ets.end.time, ets.start.time, units = "secs")
  ets.lb.95 <- ets.forecast.res$lower[,2]
  ets.ub.95 <- ets.forecast.res$upper[,2]
  ets.point <- ets.forecast.res$mean
  ets.score <- interval.score(ets.lb.95, ets.ub.95, actual.vals, alpha)
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), ets.lb.95)
    ub.95.ext <- c(rep(0, train.length), ets.ub.95)
    point.ext <- c(rep(0, train.length), ets.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin="1970-01-01"))
    influx_write(xts.obj, influx.con, db.name, "ETS")
  }
  
  sarima.start.time <- Sys.time()
  sarima.forecast.res = arima.forecast(example.ts, length(test.ts$series), "SARIMA")
  sarima.end.time <- Sys.time()
  sarima.duration <- difftime(sarima.end.time, sarima.start.time, units = "secs")
  sarima.lb.95 <- sarima.forecast.res$lower[,2]
  sarima.ub.95 <- sarima.forecast.res$upper[,2]
  sarima.point <- sarima.forecast.res$mean
  sarima.score <- interval.score(sarima.lb.95, sarima.ub.95, actual.vals, alpha)
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), sarima.lb.95)
    ub.95.ext <- c(rep(0, train.length), sarima.ub.95)
    point.ext <- c(rep(0, train.length), sarima.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin="1970-01-01"))
    influx_write(xts.obj, influx.con, db.name, "SARIMA")
  }
  
  sarimaoa.start.time <- Sys.time()
  sarimaoa.forecast.res = arima.forecast(example.ts, length(test.ts$series), "SOAARIMA")
  sarimaoa.end.time <- Sys.time()
  sarimaoa.duration <- difftime(sarimaoa.end.time, sarimaoa.start.time, units = "secs")
  sarimaoa.lb.95 <- sarimaoa.forecast.res$lower[,2]
  sarimaoa.ub.95 <- sarimaoa.forecast.res$upper[,2]
  sarimaoa.point <- sarimaoa.forecast.res$mean
  sarimaoa.score <- interval.score(sarimaoa.lb.95, sarimaoa.ub.95, actual.vals, alpha)
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), sarimaoa.lb.95)
    ub.95.ext <- c(rep(0, train.length), sarimaoa.ub.95)
    point.ext <- c(rep(0, train.length), sarimaoa.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin="1970-01-01"))
    influx_write(xts.obj, influx.con, db.name, "SARIMAOA")
  }
  
  sarima.garch.start.time <- Sys.time()
  sarima.garch.forecast.res = arima.forecast(example.ts, length(test.ts$series), "SARIMA+GARCH")
  sarima.garch.end.time <- Sys.time()
  sarima.garch.duration <- difftime(sarima.garch.end.time, sarima.garch.start.time, units = "secs")
  sarima.garch.lb.95 <- sarima.garch.forecast.res$lower[,2]
  sarima.garch.ub.95 <- sarima.garch.forecast.res$upper[,2]
  sarima.garch.point <- sarima.garch.forecast.res$mean
  sarima.garch.score <- interval.score(sarima.garch.lb.95, sarima.garch.ub.95, actual.vals, alpha)
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), sarima.garch.lb.95)
    ub.95.ext <- c(rep(0, train.length), sarima.garch.ub.95)
    point.ext <- c(rep(0, train.length), sarima.garch.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin="1970-01-01"))
    influx_write(xts.obj, influx.con, db.name, "SARIMA.GARCH")
  }
  
  sarimaoa.garch.start.time <- Sys.time()
  sarimaoa.garch.forecast.res = arima.forecast(example.ts, length(test.ts$series), "SOAARIMA+GARCH")
  sarimaoa.garch.end.time <- Sys.time()
  sarimaoa.garch.duration <- difftime(sarimaoa.garch.end.time, sarimaoa.garch.start.time, units = "secs")
  sarimaoa.garch.lb.95 <- sarimaoa.garch.forecast.res$lower[,2]
  sarimaoa.garch.ub.95 <- sarimaoa.garch.forecast.res$upper[,2]
  sarimaoa.garch.point <- sarimaoa.garch.forecast.res$mean
  sarimaoa.garch.score <- interval.score(sarimaoa.garch.lb.95, sarimaoa.garch.ub.95, actual.vals, alpha)
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), sarimaoa.garch.lb.95)
    ub.95.ext <- c(rep(0, train.length), sarimaoa.garch.ub.95)
    point.ext <- c(rep(0, train.length), sarimaoa.garch.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin="1970-01-01"))
    influx_write(xts.obj, influx.con, db.name, "SARIMAOA.GARCH")
  }
  
  sarfima.start.time <- Sys.time()
  sarfima.forecast.res = arima.forecast(example.ts, length(test.ts$series), "SARFIMA")
  sarfima.end.time <- Sys.time()
  sarfima.duration <- difftime(sarfima.end.time, sarfima.start.time, units = "secs")
  sarfima.lb.95 <- sarfima.forecast.res$lower
  sarfima.ub.95 <- sarfima.forecast.res$upper
  sarfima.point <- sarfima.forecast.res$mean
  sarfima.score <- interval.score(sarfima.lb.95, sarfima.ub.95, actual.vals, alpha)
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), sarfima.lb.95)
    ub.95.ext <- c(rep(0, train.length), sarfima.ub.95)
    point.ext <- c(rep(0, train.length), sarfima.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin="1970-01-01"))
    influx_write(xts.obj, influx.con, db.name, "SARFIMA")
  }
  
  sarfima.garch.start.time <- Sys.time()
  sarfima.garch.forecast.res = arima.forecast(example.ts, length(test.ts$series), "SARFIMA+GARCH")
  sarfima.garch.end.time <- Sys.time()
  sarfima.garch.duration <- difftime(sarfima.garch.end.time, sarfima.garch.start.time, units = "secs")
  sarfima.garch.lb.95 <- sarfima.garch.forecast.res$lower
  sarfima.garch.ub.95 <- sarfima.garch.forecast.res$upper
  sarfima.garch.point <- sarfima.garch.forecast.res$mean
  sarfima.garch.score <- interval.score(sarfima.garch.lb.95, sarfima.garch.ub.95, actual.vals, alpha)
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), sarfima.garch.lb.95)
    ub.95.ext <- c(rep(0, train.length), sarfima.garch.ub.95)
    point.ext <- c(rep(0, train.length), sarfima.garch.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin="1970-01-01"))
    influx_write(xts.obj, influx.con, db.name, "SARFIMA.GARCH")
  }
  
  lm.start.time <- Sys.time()
  lm.forecast.res = linear.regression.forecast(example.ts, length(test.ts$series))
  lm.end.time <- Sys.time()
  lm.duration <- difftime(lm.end.time, lm.start.time, units = "secs")
  lm.lb.95 <- lm.forecast.res$lower
  lm.ub.95 <- lm.forecast.res$upper
  lm.point <- lm.forecast.res$mean
  lm.score <- interval.score(lm.lb.95, lm.ub.95, actual.vals, alpha)
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), lm.lb.95)
    ub.95.ext <- c(rep(0, train.length), lm.ub.95)
    point.ext <- c(rep(0, train.length), lm.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin="1970-01-01"))
    influx_write(xts.obj, influx.con, db.name, "LM")
  }
  
  ssa.start.time <- Sys.time()
  ssa.forecast.res = ssa.forecast(train.ts, length(test.ts$series))
  ssa.end.time <- Sys.time()
  ssa.duration <- difftime(ssa.end.time, ssa.start.time, units = "secs")
  ssa.lb.95 <- ssa.forecast.res$lower[,2]
  ssa.ub.95 <- ssa.forecast.res$upper[,2]
  ssa.point <- ssa.forecast.res$mean
  ssa.score <- interval.score(ssa.lb.95, ssa.ub.95, actual.vals, alpha)
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), ssa.lb.95)
    ub.95.ext <- c(rep(0, train.length), ssa.ub.95)
    point.ext <- c(rep(0, train.length), ssa.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin="1970-01-01"))
    influx_write(xts.obj, influx.con, db.name, "SSA")
  }
  
  svr.start.time <- Sys.time()
  svr.forecast.res = svr.forecast(train.ts, length(test.ts$series))
  svr.end.time <- Sys.time()
  svr.duration <- difftime(svr.end.time, svr.start.time, units = "secs")
  svr.lb.95 <- svr.forecast.res$lower[,2]
  svr.ub.95 <- svr.forecast.res$upper[,2]
  svr.point <- svr.forecast.res$mean
  svr.score <- interval.score(svr.lb.95, svr.ub.95, actual.vals, alpha)
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), svr.lb.95)
    ub.95.ext <- c(rep(0, train.length), svr.ub.95)
    point.ext <- c(rep(0, train.length), svr.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin="1970-01-01"))
    influx_write(xts.obj, influx.con, db.name, "SVR")
  }

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
overall.testing <- function(list.of.data,
                            cluster,
                            start.time,
                            prediction.steps.num = -1,
                            influx.con = NULL,
                            db.name = "") {
  
  scores.and.models <- parLapply(cluster,
                                 list.of.data,
                                 testing.of.single.timeseries,
                                 start.time,
                                 prediction.steps.num,
                                 influx.con,
                                 db.name)
  
  scores <- lapply(scores.and.models, extract.scores)
  score.table <- as.data.frame(do.call(rbind, scores))
  models.boundaries <- lapply(scores.and.models, extract.models)
  res <- list()
  res$models.boundaries <- models.boundaries
  res$scores <- score.table
  return(res)
}