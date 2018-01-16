require(forecast)
require(tseries)
require(rugarch)

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
  
  arima.period <- parameters[4]
  
  print(paste0("ARIMA period is ", arima.period))
  
  fit <- forecast::Arima(request.time.series.list$series,
                         order = c(parameters[1], parameters[2], parameters[3]),
                         seasonal = list(order = c(parameters[5], parameters[6], parameters[7]),
                                         period = arima.period),
                         method = "CSS",
                         xreg = marked.days)
  return(fit)
}

# A function to create the SARIMA model either using best AIC automatic fit model or the model with the specified
# parameters. The period is derived automatically.
create.SARIMA.model.weekly <- function(request.time.series.list, auto = TRUE, parameters = c(0,0,0,0,0,0,0)) {
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

# Function to compute the limits for autocorrelation values
corellogram.limits <- function(acf.obj) {
  n <- acf.obj$n.used
  ret <- list()
  ret$lb <- - 1 / n - 2 / sqrt(n)
  ret$ub <- - 1 / n + 2 / sqrt(n)
  return(ret)
}

# Function to estimate the MA/AR coefficients for ARIMA model based on ACF analysis
determine.coefficient <- function(lags, interval.width) {
  fraction.of.interval.width <- 0.6 # raw estimate
  important.lags <- abs(lags) > (fraction.of.interval.width * interval.width)
  important.lags.shifted <- c(important.lags[1], important.lags[-length(important.lags)])
  difference.in.important.lags <- abs(important.lags.shifted - important.lags)
  intervals.of.important.lags <- split(important.lags, cumsum(difference.in.important.lags))
  first.interval <- intervals.of.important.lags$`0`
  c <- 0
  if(first.interval[1] == TRUE) {
    c <- length(first.interval)
  }
  
  return(c)
}

# Function to derive the GARCH model
create.GARCH.model.weekly <- function(train.timeseries, arima.residuals, coefs) {
  marked.days <- mark.time(train.timeseries$start,
                           train.timeseries$end,
                           train.timeseries$discretion)
  
  spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(coefs[1], coefs[2]), 
                                           submodel = NULL, external.regressors = as.matrix(marked.days), variance.targeting = FALSE), 
                     distribution.model = "norm", start.pars = list(), fixed.pars = list())
  garch <- ugarchfit(spec = spec,
                     data = arima.residuals,
                     solver.control=list(trace=0))
}

# Function to derive the forecasts using the ANN models
arima.forecast <- function(train.timeseries, pred.steps) {
  # Preliminary analysis to choose ARIMA parameters
  # I. Getting rid of the seasonality
  time.series <- train.timeseries$series
  # s
  s <- compute.ts.highest.period(time.series)
  # To avoid errors occuring due to large period value (defaulting to day-period)
  if(s > 24) {
    s <- 24
  }
  
  unseasoned.ts <- diff(time.series, s)
  
  # II. Checking for trend-stationarity with KPSS test
  border.kpss.p.val <- 0.05
  trend.stationary.ts <- unseasoned.ts
  kpss.stat <- kpss.test(trend.stationary.ts, null = "Trend")
  # d
  d <- 0
  d.limit <- 12 # stop-condition on d value
  
  while((kpss.stat$p.value <= border.kpss.p.val) & (d <= d.limit)) { #non-stationarity in trend, needs more differencing
    trend.stationary.ts <- diff(trend.stationary.ts)
    d <- d + 1
    kpss.stat <- kpss.test(trend.stationary.ts, null = "Trend")
  }
  
  # FracDiFF?
  
  # III. Estimating the trend
  ts.trend <- lm(trend.stationary.ts ~ time(trend.stationary.ts))
  detrend.stationary <- trend.stationary.ts - ts.trend$fitted.values
  
  # IV. Estimating the MA component coefficients for ARIMA
  acf.detrend <- acf(detrend.stationary)
  acf.ts.limits.detrend <- corellogram.limits(acf.detrend)
  acf.interval.width <- acf.ts.limits.detrend$ub - acf.ts.limits.detrend$lb
  non.seasonal.acf <- acf.detrend$acf[2:(s-1)]
  seasonal.acf <- acf.detrend$acf[(s+1):length(acf.detrend$acf)]
  
  # q
  q <- determine.coefficient(non.seasonal.acf, acf.interval.width)
  
  # Q
  Q <- determine.coefficient(seasonal.acf, acf.interval.width)
  
  # TODO: analyzing leftover outliers in ACF if any - indicate possible need for further differencing
  
  # V. Estimating the AR component coefficients for ARIMA
  pacf.detrend <- pacf(detrend.stationary)
  pacf.ts.limits.detrend <- corellogram.limits(pacf.detrend)
  pacf.interval.width <- pacf.ts.limits.detrend$ub - pacf.ts.limits.detrend$lb
  non.seasonal.pacf <- pacf.detrend$acf[2:(s-1)]
  seasonal.pacf <- pacf.detrend$acf[(s+1):length(pacf.detrend$acf)]
  
  # p
  p <- determine.coefficient(non.seasonal.pacf, pacf.interval.width)
  
  # P
  P <- determine.coefficient(seasonal.pacf, pacf.interval.width)
  
  # TODO: analyzing leftover outliers in PACF if any - indicate possible need for further differencing
  
  # VI. Estimating the number of seasonal differences
  # D
  acf.seasonality <- acf(detrend.stationary, 5 * s)
  acf.seasonality.lags <- acf.seasonality$acf
  acf.seasonality.lags.selected <- c(acf.seasonality.lags[2 * s], acf.seasonality.lags[3 * s], acf.seasonality.lags[4 * s], acf.seasonality.lags[5 * s])
  D <- sum(acf.seasonality.lags.selected > acf.interval.width)
  
  # VII. Deriving ARIMA model based on the derived parameters
  SARIMA.model = create.SARIMA.model.weekly(train.timeseries, FALSE, c(p,d,q,s,P,D,Q))
  # TODO: testing for lags that were not captured
  
  # normality - shapiro.test(trend.stationary.ts) --- low p-val - non-normal
  
  # VIII. Fitting GARCH models to the variance if necessary
  squared.residuals <- SARIMA.model$residuals ^ 2
  squared.residuals.acf <- acf(squared.residuals)
  squared.residuals.acf.lags <- squared.residuals.acf$acf
  squared.residuals.limits.acf <- corellogram.limits(squared.residuals.acf)
  garch.acf.interval.width <- squared.residuals.limits.acf$ub - squared.residuals.limits.acf$lb
  q.garch <- determine.coefficient(squared.residuals.acf.lags, garch.acf.interval.width)
  
  sGARCH.model <- NULL
  if(q.garch > 0) { # Variance could be described by some model
    squared.residuals.pacf <- pacf(squared.residuals)
    squared.residuals.pacf.lags <- squared.residuals.pacf$acf
    squared.residuals.limits.pacf <- corellogram.limits(squared.residuals.pacf)
    garch.pacf.interval.width <- squared.residuals.limits.pacf$ub - squared.residuals.limits.pacf$lb
    p.garch <- determine.coefficient(squared.residuals.pacf.lags, garch.pacf.interval.width)
    if(p.garch == 0) {
      p.garch <- 1 # Otherwise the model won't work
    }
    
    sGARCH.model <- create.GARCH.model.weekly(train.timeseries, SARIMA.model$residuals, c(p.garch, q.garch))
    
    # Checking GARCH model whether it captures all the information about variance
    check.garch.acf <- acf(sGARCH.model@fit$residuals / sGARCH.model@fit$sigma)
    check.garch.acf.lags <- check.garch.acf$acf
    check.garch.limits.acf <- corellogram.limits(check.garch.acf)
    check.garch.acf.interval.width <- check.garch.limits.acf$ub - check.garch.limits.acf$lb
    fraction.of.interval.width <- 0.6 # raw estimate
    important.lags <- abs(check.garch.acf.lags) > (fraction.of.interval.width * check.garch.acf.interval.width)
    fraction.of.important.lags <- sum(important.lags) / length(important.lags)
    if(fraction.of.important.lags > 0.05) {
      print("We need to adjust GARCH model because some information was not captured by it. Source: ACF of standardized residuals.")
    }
    
    check.garch.pacf <- pacf(sGARCH.model@fit$residuals / sGARCH.model@fit$sigma)
    check.garch.pacf.lags <- check.garch.pacf$acf
    check.garch.limits.pacf <- corellogram.limits(check.garch.pacf)
    check.garch.pacf.interval.width <- check.garch.limits.pacf$ub - check.garch.limits.pacf$lb
    fraction.of.interval.width <- 0.6 # raw estimate
    important.lags <- abs(check.garch.pacf.lags) > (fraction.of.interval.width * check.garch.pacf.interval.width)
    fraction.of.important.lags <- sum(important.lags) / length(important.lags)
    if(fraction.of.important.lags > 0.05) {
      print("We need to adjust GARCH model because some information was not captured by it. Source: PACF of standardized residuals.")
    }
  }
  
  # TODO: IX. Using ARIMA/GARCH combination for forecasts of mean/variance plus trend forecast
  
    
    
    

  
  # Next, plot the residuals (ordinary or raw) and standardized residuals in various
  # ways using the code below. The standardized residuals are best for checking
  # the model, but the residuals are useful to see if there are GARCH effects in
  # the series.
  # res = residuals(garch.model.Tbill)
  # res_std = res / garch.model.Tbill@sigma.t
  #par(mfrow=c(2,3))
  #plot(res)
  #acf(res)
  #acf(res^2)
  #plot(res_std)
  #acf(res_std)
  #acf(res_std^2)
  #https://faculty.washington.edu/ezivot/econ589/ch18-garch.pdf

  
  # plus trend forecast
  SARIMA.forecast = forecast.requests(train.timeseries, SARIMA.model, pred.steps)
  return(SARIMA.forecast)
}
