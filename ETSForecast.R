require(forecast)

# Function to derive the forecasts using the ANN models
ets.forecast <- function(train.timeseries, pred.steps) {
  model.fitting.start <- Sys.time()
  ets.model = ets(train.timeseries, model = "ZAA")
  ets.forecast = forecast(ets.model, pred.steps)
  model.fitting.end <- Sys.time()
  ets.forecast$model.fitting.duration <- difftime(model.fitting.end, model.fitting.start, units = "secs")
  ets.forecast$parameters.selection.duration = 0
  return(ets.forecast)
}