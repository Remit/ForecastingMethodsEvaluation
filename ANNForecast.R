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