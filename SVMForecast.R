require(e1071)

days.name <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Function to derive the forecasts using SVR model
svr.forecast <- function(train.timeseries, pred.steps) {
  teaching.window.width <- 1
  training.num <- length(train.timeseries$series)
  training.set.length <- training.num - teaching.window.width
  training.set.index.l.predictor <- rep(0, training.set.length)
  training.set.index.l.predictor <- training.set.index.l.predictor + c(1 : training.set.length)
  training.set.index.u.predictor <- rep((teaching.window.width - 1), training.set.length)
  training.set.index.u.predictor <- training.set.index.u.predictor + c(1 : training.set.length)
  training.set.index.outcome <- training.set.index.u.predictor + 1
  
  indices.df <- data.frame(predictor.lb = training.set.index.l.predictor,
                           predictor.ub = training.set.index.u.predictor,
                           outcome.index = training.set.index.outcome)
  
  adapted.series <- as.numeric(train.timeseries$series)
  
  mark.time.vector <- function(adapted.series.extracted) {
    adapted.series.extracted.time <- as.POSIXct(adapted.series.extracted, origin="1970-01-01")
    adapted.series.extracted.date <- as.Date(adapted.series.extracted.time)
    #month.id <- match(months(adapted.series.extracted.date), month.name) # TODO: uncomment when more than one month data available
    day.id <- match(weekdays(adapted.series.extracted.date), days.name)
    is.weekend <- as.numeric(day.id > 5)
    hour.in.day <- strftime(adapted.series.extracted.time, format="%H")
    time.vector <- c(is.weekend, day.id, as.numeric(hour.in.day))
    return(time.vector)
  }
  
  extract.values.for.boundary <- function(data.row, train.timeseries) {
    adapted.series <- train.timeseries$series
    predictor.lb <- data.row[1][[1]]
    predictor.ub <- data.row[2][[1]]
    outcome.index <- data.row[3][[1]]
    adapted.series.extracted <- time(adapted.series)[seq(predictor.lb, predictor.ub)]
    predictors.vals <- as.numeric(adapted.series)[seq(predictor.lb, predictor.ub)]
    time.marks <- mark.time.vector(adapted.series.extracted)
    predictors <- c(time.marks, predictors.vals)
    outcome <- adapted.series[outcome.index]
    result <- c(predictors, outcome)
    return(result)
  }
  
  values.for.training <- data.frame(t(apply(indices.df, 1, extract.values.for.boundary, train.timeseries)))

  name.outcome.variable <- colnames(values.for.training)[ncol(values.for.training)]
  training.formula <- as.formula(paste0(name.outcome.variable," ~ ."))
  svr.model <- svm(training.formula,
                   values.for.training,
                   type = "eps-regression",
                   gamma = 0.9,
                   cost = 0.3,
                   epsilon = 0.52)
  
  # svr.model.opt = tune(svm,
  #                      training.formula,
  #                      data = values.for.training,
  #                      ranges=list(epsilon = seq(0.5,0.7,0.01),
  #                                  cost = seq(0.1,1.0,0.1),
  #                                  gamma = seq(0.1,1.0,0.1)))
  # 
  # svr.model <- svr.model.opt$best.model
  
  start.index.test.set <- length(adapted.series) - teaching.window.width + 1
  
  blueprint.testing.times <- time(train.timeseries$series)[start.index.test.set : length(adapted.series)]
  time.marks <- mark.time.vector(blueprint.testing.times)
  
  blueprint.testing.set.small <- adapted.series[start.index.test.set : length(adapted.series)]
  blueprint.testing.set <- c(time.marks, blueprint.testing.set.small)
  names.predictors <- colnames(values.for.training)[1 : ncol(values.for.training) - 1]
  prediction.df <- data.frame(t(blueprint.testing.set))
  colnames(prediction.df) <- names.predictors
  
  prediction <- predict(svr.model, prediction.df)[[1]]
  predictions <- c(prediction)
  
  # Rest of rolling prediction
  i <- 1
  while(i < pred.steps) {
    last.time <- blueprint.testing.times[length(blueprint.testing.times)]
    blueprint.testing.times.shifted <- blueprint.testing.times[-1]
    blueprint.testing.times <- c(blueprint.testing.times.shifted, last.time + 3600)
    time.marks <- mark.time.vector(blueprint.testing.times)
    blueprint.testing.set.small <- c(blueprint.testing.set.small[-1], prediction)
    blueprint.testing.set <- c(time.marks, blueprint.testing.set.small)
    prediction.df <- data.frame(t(blueprint.testing.set))
    colnames(prediction.df) <- names.predictors
    prediction <- predict(svr.model, prediction.df)[[1]]
    predictions <- c(predictions, prediction)
    i <- i + 1
  }
  
  # plot(c(adapted.series, predictions))
  
  sd.fc <- sd(svr.model$fitted)
  lb.80 <- predictions - 1.28 * sd.fc
  lb.95 <- predictions - 1.96 * sd.fc
  lower <- data.frame(lb.80 = lb.80,
                      lb.95 = lb.95)
  
  ub.80 <- predictions + 1.28 * sd.fc
  ub.95 <- predictions + 1.96 * sd.fc
  upper <- data.frame(ub.80 = ub.80,
                      ub.95 = ub.95)
  
  svr.forecast <- list()
  svr.forecast$lower <- lower
  svr.forecast$upper <- upper
  
  return(svr.forecast)
}

#https://www.kdnuggets.com/2017/03/building-regression-models-support-vector-regression.html
# for prediction intervals: http://homepage.univie.ac.at/robert.kunst/pres09_prog_turyna_hrdina.pdf 