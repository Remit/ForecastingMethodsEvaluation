# Forecast model creation, forecasting and scoring for the single time series.
testing.of.single.timeseries <- function(time.series, start.time, prediction.steps.num, influx.con, db.name, mongo.con) {
  # Functions to create the insertion/update JSON sequences
  create.JSON.sequence.for.method <- function(method.name, score, params.duration, fitting.duration) {
    JSON.string <- paste0('"',
                          method.name,
                          '" : { "score" : ',
                          score,
                          ', "params.selection.duration" : ',
                          params.duration,
                          ', "fitting.duration" : ',
                          fitting.duration,
                          '}')
    return(JSON.string)
  }
  
  create.JSON.sequence.for.insertion <- function(client, method.name, score, duration) {
    JSON.string.method <- create.JSON.sequence.for.method(method.name, score, duration)
    JSON.string <- paste0('{"username" : "',
                          client,
                          '", ',
                          JSON.string.method,
                          '}')
    return(JSON.string)
  }
  
  create.JSON.sequence.for.update.adr <- function(client) {
    JSON.string <- paste0('{"username" : "',
                          client,
                          '"}')
    return(JSON.string)
  }
  
  create.JSON.sequence.for.update.content <- function(client, method.name, score, params.duration, fitting.duration) {
    JSON.string.method <- create.JSON.sequence.for.method(method.name, score, params.duration, fitting.duration)
    JSON.string <- paste0('{"$set":{',
                          JSON.string.method,
                          '}}')
    return(JSON.string)
  }
  
  # Currently processing info:
  TS.ID <- names(time.series)[1]
  print(paste0("Currently processing TS with ID # ", TS.ID))
  
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
  
  # Cleaning the database from previous entries for this user
  if(!is.null(mongo.con)) {
    removal.str <- paste0('{"username" : "', db.name, '"}')
    mongo.con$remove(removal.str)
  }

  ets.name <- "ETS"
  sarima.name <- "SARIMA"
  sarimaoa.name <- "SOAARIMA"
  sarima.garch.name <- "SARIMA+GARCH"
  sarimaoa.garch.name <- "SOAARIMA+GARCH"
  sarfima.name <- "SARFIMA"
  sarfima.garch.name <-"SARFIMA+GARCH"
  lm.name <- "LM"
  ssa.name <- "SSA"
  svr.name <- "SVR"
  
  origin.time <- "1970-01-01"
  
  # Deriving different forecasting models and forecasts with prediction intervals
  
  ets.forecast.res = ets.forecast(train.ts$series, length(test.ts$series))
  ets.parameters.selection.duration <- ets.forecast.res$parameters.selection.duration
  ets.model.fitting.duration <- ets.forecast.res$model.fitting.duration
  ets.lb.95 <- ets.forecast.res$lower[,2]
  ets.ub.95 <- ets.forecast.res$upper[,2]
  ets.point <- ets.forecast.res$mean
  ets.score <- interval.score(ets.lb.95, ets.ub.95, actual.vals, alpha)
  if(!is.null(mongo.con)) {
    insertion.string <- create.JSON.sequence.for.insertion(db.name, ets.name, ets.score, ets.parameters.selection.duration, ets.model.fitting.duration)
    mongo.con$insert(insertion.string)
  }
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), ets.lb.95)
    ub.95.ext <- c(rep(0, train.length), ets.ub.95)
    point.ext <- c(rep(0, train.length), ets.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin=origin.time))
    influx_write(xts.obj, influx.con, db.name, ets.name)
  }

  sarima.forecast.res = arima.forecast(example.ts, length(test.ts$series), sarima.name)
  sarima.parameters.selection.duration <- sarima.forecast.res$parameters.selection.duration
  sarima.model.fitting.duration <- sarima.forecast.res$model.fitting.duration
  sarima.lb.95 <- sarima.forecast.res$lower[,2]
  sarima.ub.95 <- sarima.forecast.res$upper[,2]
  sarima.point <- sarima.forecast.res$mean
  sarima.score <- interval.score(sarima.lb.95, sarima.ub.95, actual.vals, alpha)
  if(!is.null(mongo.con)) {
    update.adr <- create.JSON.sequence.for.update.adr(db.name)
    update.str <- create.JSON.sequence.for.update.content(db.name, sarima.name, sarima.score, sarima.parameters.selection.duration, sarima.model.fitting.duration)
    mongo.con$update(update.adr, update.str)
  }
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), sarima.lb.95)
    ub.95.ext <- c(rep(0, train.length), sarima.ub.95)
    point.ext <- c(rep(0, train.length), sarima.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin=origin.time))
    influx_write(xts.obj, influx.con, db.name, sarima.name)
  }
  
  sarimaoa.forecast.res = arima.forecast(example.ts, length(test.ts$series), sarimaoa.name)
  sarimaoa.parameters.selection.duration <- sarimaoa.forecast.res$parameters.selection.duration
  sarimaoa.model.fitting.duration <- sarimaoa.forecast.res$model.fitting.duration
  sarimaoa.lb.95 <- sarimaoa.forecast.res$lower[,2]
  sarimaoa.ub.95 <- sarimaoa.forecast.res$upper[,2]
  sarimaoa.point <- sarimaoa.forecast.res$mean
  sarimaoa.score <- interval.score(sarimaoa.lb.95, sarimaoa.ub.95, actual.vals, alpha)
  if(!is.null(mongo.con)) {
    update.adr <- create.JSON.sequence.for.update.adr(db.name)
    update.str <- create.JSON.sequence.for.update.content(db.name, sarimaoa.name, sarimaoa.score, sarimaoa.parameters.selection.duration, sarimaoa.model.fitting.duration)
    mongo.con$update(update.adr, update.str)
  }
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), sarimaoa.lb.95)
    ub.95.ext <- c(rep(0, train.length), sarimaoa.ub.95)
    point.ext <- c(rep(0, train.length), sarimaoa.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin=origin.time))
    influx_write(xts.obj, influx.con, db.name, sarimaoa.name)
  }
  
  sarima.garch.forecast.res = arima.forecast(example.ts, length(test.ts$series), sarima.garch.name)
  sarima.garch.parameters.selection.duration <- sarima.garch.forecast.res$parameters.selection.duration
  sarima.garch.model.fitting.duration <- sarima.garch.forecast.res$model.fitting.duration
  sarima.garch.lb.95 <- sarima.garch.forecast.res$lower[,2]
  sarima.garch.ub.95 <- sarima.garch.forecast.res$upper[,2]
  sarima.garch.point <- sarima.garch.forecast.res$mean
  sarima.garch.score <- interval.score(sarima.garch.lb.95, sarima.garch.ub.95, actual.vals, alpha)
  if(!is.null(mongo.con)) {
    update.adr <- create.JSON.sequence.for.update.adr(db.name)
    update.str <- create.JSON.sequence.for.update.content(db.name, sarima.garch.name, sarima.garch.score, sarima.garch.parameters.selection.duration, sarima.garch.model.fitting.duration)
    mongo.con$update(update.adr, update.str)
  }
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), sarima.garch.lb.95)
    ub.95.ext <- c(rep(0, train.length), sarima.garch.ub.95)
    point.ext <- c(rep(0, train.length), sarima.garch.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin=origin.time))
    influx_write(xts.obj, influx.con, db.name, sarima.garch.name)
  }
  
  sarimaoa.garch.forecast.res = arima.forecast(example.ts, length(test.ts$series), sarimaoa.garch.name)
  sarimaoa.garch.parameters.selection.duration <- sarimaoa.garch.forecast.res$parameters.selection.duration
  sarimaoa.garch.model.fitting.duration <- sarimaoa.garch.forecast.res$model.fitting.duration
  sarimaoa.garch.lb.95 <- sarimaoa.garch.forecast.res$lower[,2]
  sarimaoa.garch.ub.95 <- sarimaoa.garch.forecast.res$upper[,2]
  sarimaoa.garch.point <- sarimaoa.garch.forecast.res$mean
  sarimaoa.garch.score <- interval.score(sarimaoa.garch.lb.95, sarimaoa.garch.ub.95, actual.vals, alpha)
  if(!is.null(mongo.con)) {
    update.adr <- create.JSON.sequence.for.update.adr(db.name)
    update.str <- create.JSON.sequence.for.update.content(db.name, sarimaoa.garch.name, sarimaoa.garch.score, sarimaoa.garch.parameters.selection.duration, sarimaoa.garch.model.fitting.duration)
    mongo.con$update(update.adr, update.str)
  }
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), sarimaoa.garch.lb.95)
    ub.95.ext <- c(rep(0, train.length), sarimaoa.garch.ub.95)
    point.ext <- c(rep(0, train.length), sarimaoa.garch.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin=origin.time))
    influx_write(xts.obj, influx.con, db.name, sarimaoa.garch.name)
  }
  
  sarfima.forecast.res = arima.forecast(example.ts, length(test.ts$series), sarfima.name)
  sarfima.parameters.selection.duration <- sarfima.forecast.res$parameters.selection.duration
  sarfima.model.fitting.duration <- sarfima.forecast.res$model.fitting.duration
  sarfima.lb.95 <- sarfima.forecast.res$lower
  sarfima.ub.95 <- sarfima.forecast.res$upper
  sarfima.point <- sarfima.forecast.res$mean
  sarfima.score <- interval.score(sarfima.lb.95, sarfima.ub.95, actual.vals, alpha)
  if(!is.null(mongo.con)) {
    update.adr <- create.JSON.sequence.for.update.adr(db.name)
    update.str <- create.JSON.sequence.for.update.content(db.name, sarfima.name, sarfima.score, sarfima.parameters.selection.duration, sarfima.model.fitting.duration)
    mongo.con$update(update.adr, update.str)
  }
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), sarfima.lb.95)
    ub.95.ext <- c(rep(0, train.length), sarfima.ub.95)
    point.ext <- c(rep(0, train.length), sarfima.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin=origin.time))
    influx_write(xts.obj, influx.con, db.name, sarfima.name)
  }
  
  sarfima.garch.forecast.res = arima.forecast(example.ts, length(test.ts$series), sarfima.garch.name)
  sarfima.garch.parameters.selection.duration <- sarfima.garch.forecast.res$parameters.selection.duration
  sarfima.garch.model.fitting.duration <- sarfima.garch.forecast.res$model.fitting.duration
  sarfima.garch.lb.95 <- sarfima.garch.forecast.res$lower
  sarfima.garch.ub.95 <- sarfima.garch.forecast.res$upper
  sarfima.garch.point <- sarfima.garch.forecast.res$mean
  sarfima.garch.score <- interval.score(sarfima.garch.lb.95, sarfima.garch.ub.95, actual.vals, alpha)
  if(!is.null(mongo.con)) {
    update.adr <- create.JSON.sequence.for.update.adr(db.name)
    update.str <- create.JSON.sequence.for.update.content(db.name, sarfima.garch.name, sarfima.garch.score, sarfima.garch.parameters.selection.duration, sarfima.garch.model.fitting.duration)
    mongo.con$update(update.adr, update.str)
  }
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), sarfima.garch.lb.95)
    ub.95.ext <- c(rep(0, train.length), sarfima.garch.ub.95)
    point.ext <- c(rep(0, train.length), sarfima.garch.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin=origin.time))
    influx_write(xts.obj, influx.con, db.name, sarfima.garch.name)
  }
  
  lm.forecast.res = linear.regression.forecast(example.ts, length(test.ts$series))
  lm.parameters.selection.duration <- lm.forecast.res$parameters.selection.duration
  lm.model.fitting.duration <- lm.forecast.res$model.fitting.duration
  lm.lb.95 <- lm.forecast.res$lower
  lm.ub.95 <- lm.forecast.res$upper
  lm.point <- lm.forecast.res$mean
  lm.score <- interval.score(lm.lb.95, lm.ub.95, actual.vals, alpha)
  if(!is.null(mongo.con)) {
    update.adr <- create.JSON.sequence.for.update.adr(db.name)
    update.str <- create.JSON.sequence.for.update.content(db.name, lm.name, lm.score, lm.parameters.selection.duration, lm.model.fitting.duration)
    mongo.con$update(update.adr, update.str)
  }
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), lm.lb.95)
    ub.95.ext <- c(rep(0, train.length), lm.ub.95)
    point.ext <- c(rep(0, train.length), lm.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin=origin.time))
    influx_write(xts.obj, influx.con, db.name, lm.name)
  }
  
  ssa.forecast.res = ssa.forecast(train.ts, length(test.ts$series))
  ssa.parameters.selection.duration <- ssa.forecast.res$parameters.selection.duration
  ssa.model.fitting.duration <- ssa.forecast.res$model.fitting.duration
  ssa.lb.95 <- ssa.forecast.res$lower[,2]
  ssa.ub.95 <- ssa.forecast.res$upper[,2]
  ssa.point <- ssa.forecast.res$mean
  ssa.score <- interval.score(ssa.lb.95, ssa.ub.95, actual.vals, alpha)
  if(!is.null(mongo.con)) {
    update.adr <- create.JSON.sequence.for.update.adr(db.name)
    update.str <- create.JSON.sequence.for.update.content(db.name, ssa.name, ssa.score, ssa.parameters.selection.duration, ssa.model.fitting.duration)
    mongo.con$update(update.adr, update.str)
  }
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), ssa.lb.95)
    ub.95.ext <- c(rep(0, train.length), ssa.ub.95)
    point.ext <- c(rep(0, train.length), ssa.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin=origin.time))
    influx_write(xts.obj, influx.con, db.name, ssa.name)
  }
  
  svr.forecast.res = svr.forecast(train.ts, length(test.ts$series))
  svr.parameters.selection.duration <- svr.forecast.res$parameters.selection.duration
  svr.model.fitting.duration <- svr.forecast.res$model.fitting.duration
  svr.lb.95 <- svr.forecast.res$lower[,2]
  svr.ub.95 <- svr.forecast.res$upper[,2]
  svr.point <- svr.forecast.res$mean
  svr.score <- interval.score(svr.lb.95, svr.ub.95, actual.vals, alpha)
  if(!is.null(mongo.con)) {
    update.adr <- create.JSON.sequence.for.update.adr(db.name)
    update.str <- create.JSON.sequence.for.update.content(db.name, svr.name, svr.score, svr.parameters.selection.duration, svr.model.fitting.duration)
    mongo.con$update(update.adr, update.str)
  }
  if(!is.null(influx.con)) {
    lb.95.ext <- c(rep(0, train.length), svr.lb.95)
    ub.95.ext <- c(rep(0, train.length), svr.ub.95)
    point.ext <- c(rep(0, train.length), svr.point)
    df <- data.frame(lb = lb.95.ext,
                     ub = ub.95.ext,
                     pf = point.ext,
                     av = actual.vals.ext)
    xts.obj <- xts(df, as.POSIXct(timeline.ext, origin=origin.time))
    influx_write(xts.obj, influx.con, db.name, svr.name)
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
                       ets.parameters.selection.duration = ets.parameters.selection.duration,
                       sarima.parameters.selection.duration = sarima.parameters.selection.duration,
                       sarimaoa.parameters.selection.duration = sarimaoa.parameters.selection.duration,
                       sarima.garch.parameters.selection.duration = sarima.garch.parameters.selection.duration,
                       sarimaoa.garch.parameters.selection.duration = sarimaoa.garch.parameters.selection.duration,
                       sarfima.parameters.selection.duration = sarfima.parameters.selection.duration,
                       sarfima.garch.parameters.selection.duration = sarfima.garch.parameters.selection.duration,
                       lm.parameters.selection.duration = lm.parameters.selection.duration,
                       ssa.parameters.selection.duration = ssa.parameters.selection.duration,
                       svr.parameters.selection.duration = svr.parameters.selection.duration,
                       ets.model.fitting.duration = ets.model.fitting.duration,
                       sarima.model.fitting.duration = sarima.model.fitting.duration,
                       sarimaoa.model.fitting.duration = sarimaoa.model.fitting.duration,
                       sarima.garch.model.fitting.duration = sarima.garch.model.fitting.duration,
                       sarimaoa.garch.model.fitting.duration = sarimaoa.garch.model.fitting.duration,
                       sarfima.model.fitting.duration = sarfima.model.fitting.duration,
                       sarfima.garch.model.fitting.duration = sarfima.garch.model.fitting.duration,
                       lm.model.fitting.duration = lm.model.fitting.duration,
                       ssa.model.fitting.duration = ssa.model.fitting.duration,
                       svr.model.fitting.duration = svr.model.fitting.duration)
  
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
  res$ts.id <- TS.ID
  
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

# Function to extract ts IDs from the list (as list)
extract.ids <- function(list.elem) {
  return(list.elem$ts.id)
}

# Construction of the scoring table encompassing scores for all forecasting methods tested with
# identified best methods for each data set.
overall.testing <- function(list.of.data,
                            cluster,
                            start.time,
                            prediction.steps.num = -1,
                            influx.con = NULL,
                            db.name = "",
                            mongo.con = NULL) {
  
  scores.and.models <- parSapply(cluster,
                                 list.of.data,
                                 testing.of.single.timeseries,
                                 start.time,
                                 prediction.steps.num,
                                 influx.con,
                                 db.name,
                                 mongo.con,
                                 simplify = FALSE,
                                 USE.NAMES = TRUE)
  
  scores <- lapply(scores.and.models, extract.scores)
  score.table <- as.data.frame(do.call(rbind, scores))
  models.boundaries <- lapply(scores.and.models, extract.models)
  ts.ids <- lapply(scores.and.models, extract.ids)
  ts.ids.col <- as.data.frame(do.call(rbind, ts.ids))
  names(ts.ids.col) <- "ts.ID"
  score.table <- cbind(score.table, ts.ids.col)
  res <- list()
  res$models.boundaries <- models.boundaries
  res$scores <- score.table
  return(res)
}