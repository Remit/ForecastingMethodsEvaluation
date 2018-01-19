require(forecast)

source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Facilities.R"))

# Function to derive the forecasts using the ANN models
# Feed-forward neural networks with a single hidden layer and lagged inputs for forecasting univariate time series.
ann.forecast <- function(train.timeseries, pred.steps) {
  marked.days <- mark.time(train.timeseries$start,
                           train.timeseries$end,
                           train.timeseries$discretion)
  
  nn.model = nnetar(train.timeseries$series, xreg = marked.days)
  
  start = train.timeseries$end
  discretion = train.timeseries$discretion
  duration = pred.steps * discretion
  marked.ts.for.prediction = mark.time(start + discretion, start + duration, discretion)
  
  nn.forecast = forecast(nn.model, pred.steps, PI = TRUE, xreg = marked.ts.for.prediction)
  return(nn.forecast)
}



library(deepnet)



# train.window.length <- 24 * 7
# start.point <- seq(1, length(train.timeseries$series) - train.window.length - 1)
# end.point <- start.point + train.window.length
# interval.points <- data.frame(start.point = start.point, end.point = end.point)
# 
# extract.training.points <- function(interval.limits, time.series) {
#   interval <- time.series[interval.limits[1] : interval.limits[2]]
#   return(interval)
# }
# 
# training.x.array <- t(apply(interval.points, 1, extract.training.points, train.timeseries$series))
# 
# y.indices <- end.point + 1
# output.y.vector <- train.timeseries$series[y.indices]
# 
# deep.nn.trained <- dbn.dnn.train(training.x.array, output.y.vector, hidden = c(169, 169, 169, 169, 169), activationfun = "sigm", learningrate = 0.05, 
#               momentum = 0.5, learningrate_scale = 1, output = "sigm", numepochs = 30000, 
#               batchsize = 24, hidden_dropout = 0, visible_dropout = 0, cd = 1)
# 
# test.start <- length(train.timeseries$series) - train.window.length
# test.end <- length(train.timeseries$series)
# test.vector <- train.timeseries$series[test.start : test.end]
# predicted.value <- nn.predict(deep.nn.trained, t(test.vector))[1,1]
# predicted.vals <- c(predicted.value)
# 
# i <- 1
# while(i < length(test.ts$series)) {
#   test.vector <- c(test.vector[-1], predicted.value)
#   predicted.value <- nn.predict(deep.nn.trained, t(test.vector))[1,1]
#   predicted.vals <- c(predicted.vals, predicted.value)
#   i <- i + 1
# }
# 
# https://jeremiedb.github.io/mxnet_R_bucketing/TimeSeries_CPU