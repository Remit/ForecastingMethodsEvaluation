require(forecast)

source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Facilities.R"))

get.best.arima <- function(request.time.series.list, maxord = c(1,1,1,1,1,1)) {
  best.aic <- 1e8
  n <- length(request.time.series.list$series)
  marked.days <- mark.time(request.time.series.list$start,
                           request.time.series.list$end,
                           request.time.series.list$discretion)
  
  for(p in 0:maxord[1])
    for(d in 0:maxord[2])
      for(q in 0:maxord[3])
        for(P in 0:maxord[4])
          for(D in 0:maxord[5])
            for(Q in 0:maxord[6])
            {
              fit <- arima(request.time.series.list$series,
                           order = c(p, d, q),
                           seasonal = list(order = c(P, D, Q),
                                           period = compute.ts.highest.period(request.time.series.list$series)),
                           method = "CSS",
                           xreg = marked.days)
              fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
              if(fit.aic < best.aic)
              {
                best.aic <- fit.aic
                best.fit <- fit
                best.model <- c(p, d, q, P, D, Q)
              }
            }
  return(list(AIC = best.aic, SARIMA = best.fit, coef = best.model))
}

# A function to compute the time series highest-order period in order to be used for seasonality estimates
# for 1 hour discretion
compute.ts.highest.period <- function(time.series) {
  ts.spectrum <- spectrum(time.series)
  period <- (1 / ts.spectrum$freq[which(ts.spectrum$spec == max(ts.spectrum$spec))]) / 3600
  return(round(period))
}

# A function to fit SARIMA with the specified parameters
get.SARIMA.with.defined.parameters <- function(request.time.series.list, parameters) {
  marked.days <- mark.time(request.time.series.list$start,
                           request.time.series.list$end,
                           request.time.series.list$discretion)
  
  arima.period <- compute.ts.highest.period(request.time.series.list$series)
  # To avoid errors occuring due to large period value (defaulting to day-period)
  if(arima.period > 24) {
    arima.period <- 24
  }
  print(paste0("ARIMA period is ", arima.period))
  
  fit <- forecast::Arima(request.time.series.list$series,
                         order = c(parameters[1], parameters[2], parameters[3]),
                         seasonal = list(order = c(parameters[4], parameters[5], parameters[6]),
                                         period = arima.period),
                         method = "CSS",
                         xreg = marked.days)
  return(fit)
}

# A function to create the SARIMA model either using best AIC automatic fit model or the model with the specified
# parameters. The period is derived automatically.
create.SARIMA.model.weekly <- function(request.time.series.list, auto = TRUE, parameters = c(0,0,0,0,0,0)) {
  if(auto) {
    ARIMA.model = get.best.arima(request.time.series.list, maxord = c(2,2,2,2,2,2))
    return(ARIMA.model$SARIMA)
  } else {
    ARIMA.model.fit = get.SARIMA.with.defined.parameters(request.time.series.list, parameters)
    return(ARIMA.model.fit)
  }
}

forecast.requests <- function(request.time.series.list, ARIMA.model, n.predicted.values) {
  start = request.time.series.list$end
  discretion = request.time.series.list$discretion
  duration = n.predicted.values * discretion
  marked.ts.for.prediction = mark.time(start + discretion, start + duration, discretion)
  prediction = forecast::forecast(ARIMA.model, h = n.predicted.values, xreg = marked.ts.for.prediction)
  return(prediction)
}

# Function to derive the forecasts using the ANN models
arima.forecast <- function(train.timeseries, pred.steps) {
  SARIMA.model = create.SARIMA.model.weekly(train.timeseries, FALSE, c(2,0,0,1,1,1))
  SARIMA.forecast = forecast.requests(train.timeseries, SARIMA.model, pred.steps)
  return(SARIMA.forecast)
}