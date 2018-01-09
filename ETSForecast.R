require(forecast)

# Function to derive the forecasts using the ANN models
ets.forecast <- function(train.timeseries, pred.steps) {
  ets.model = ets(train.timeseries)
  ets.forecast = forecast(ets.model, pred.steps)
  return(nn.forecast)
}