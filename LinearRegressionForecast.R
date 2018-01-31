# Function to derive the forecasts using the simple linear model
linear.regression.forecast <- function(example.ts, pred.steps) {
  test.set.length.days <- 7
  train.timeseries <- list()
  train.timeseries$series <- window(example.ts$series, as.numeric(example.ts$start), as.numeric(example.ts$end - test.set.length.days * 24 * 3600))
  train.timeseries$start <- example.ts$start
  train.timeseries$end <- example.ts$end - test.set.length.days * 24 * 3600
  train.timeseries$discretion <- example.ts$discretion
  
  test.ts <- list()
  test.ts$series <- window(example.ts$series, as.numeric(example.ts$end - test.set.length.days * 24 * 3600 + 1), as.numeric(example.ts$end))
  test.ts$start <- example.ts$end - test.set.length.days * 24 * 3600 + 1
  test.ts$end <- example.ts$end
  test.ts$discretion <- example.ts$discretion
  
  time.series <- train.timeseries$series
  
  trended.data <- data.frame(time = time(time.series), value = time.series)
  ts.trend <- lm(value ~ time, data = trended.data)
  trend.predictions <- predict.lm(ts.trend, data.frame(time = seq(as.numeric(test.ts$start), as.numeric(test.ts$end), 3600)), se.fit = TRUE)
  resulting.model <- list()
  resulting.model$mean <- trend.predictions$fit
  resulting.model$lower <- resulting.model$mean - trend.predictions$se.fit
  resulting.model$upper <- resulting.model$mean + trend.predictions$se.fit
  
  return(resulting.model)
}