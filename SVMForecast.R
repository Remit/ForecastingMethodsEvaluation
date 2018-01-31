require(e1071)

# Function to derive the forecasts using SVR model
svr.forecast <- function(train.timeseries, pred.steps) {
  
  # svr.model <- svm(requests~time,data.for.training, gamma = 1500)
  # prediction <- predict(svr.model, data.for.training)
  # plot(data.for.training)
  # points(data.for.training$time, prediction, col = "red", pch=16)
  
  timeline <- seq(from = train.timeseries$start,
                  to = train.timeseries$end,
                  by = train.timeseries$discretion)
  data.for.training <- data.frame(time = timeline,
                                  requests = train.timeseries$series)
  svr.model.opt = tune(svm,
                       requests ~ time,
                       data = data.for.training,
                       ranges=list(epsilon = seq(0,1,0.1),
                                   cost = 1:5,
                                   gamma = 1400:1500))
  
  svr.model <- svr.model.opt$best.model
  pred.start <- train.timeseries$end + train.timeseries$discretion
  prediction.df <- data.frame(time = seq(from = pred.start,
                                         to = pred.start + (pred.steps - 1) * train.timeseries$discretion,
                                         by = train.timeseries$discretion))
  mean.fc <- predict(svr.model, prediction.df)
  sd.fc <- sd(mean.fc)
  lb.80 <- mean.fc - 1.28 * sd.fc
  lb.95 <- mean.fc - 1.96 * sd.fc
  lower <- data.frame(lb.80 = lb.80,
                      lb.95 = lb.95)
  
  ub.80 <- mean.fc + 1.28 * sd.fc
  ub.95 <- mean.fc + 1.96 * sd.fc
  upper <- data.frame(ub.80 = ub.80,
                      ub.95 = ub.95)
  
  svr.forecast <- list()
  svr.forecast$lower <- lower
  svr.forecast$upper <- upper
  
  return(svr.forecast)
}

#https://www.kdnuggets.com/2017/03/building-regression-models-support-vector-regression.html