require(forecast)

# Function to derive the forecasts using the ANN models
ets.forecast <- function(train.timeseries, pred.steps) {
  ets.model = ets(train.timeseries, model = "ZAA")
  ets.forecast = forecast(ets.model, pred.steps)
  return(ets.forecast)
}