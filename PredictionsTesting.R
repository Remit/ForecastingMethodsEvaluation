############ SETTINGS ############
# Input here path to your data.
# In the scope of forecasting, we omit trivial time series that are constant around zero.
file.path <- "/home/remit/dissCloud/Instana/data/metrics.csv"
#file.path <- "D:/B/Core/testdata/metrics.csv"
############ SETTINGS ############

# Automatic installation of necessary packages in case of abscence (might be some special cases with installation)
list.of.packages <- c("imputeTS",
                      "ggplot2",
                      "tseries",
                      "forecast",
                      "rugarch",
                      "tsoutliers",
                      "reshape2",
                      "Rssa",
                      "e1071")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Getting scores and prediction intervals for the time series data
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

clusterEvalQ(cl, {
  source("ARIMAForecast.R")
  source("ETSForecast.R")
  source("SSAForecast.R")
  source("SVMForecast.R")
  source("ForecastMetric.R")
  source("DataPreprocessing.R")
  source("LinearRegressionForecast.R")
  }) 

data.raw <- read.csv2(file = file.path, header = F, sep = ",", stringsAsFactors = F)
lst <- ts.preprocessing.matrix.Instana(data.raw)
scores.and.models <- overall.testing(lst, cl)#lst[1:10]
stopCluster(cl)

# Input - filename
# Interface for to use the forecasting function and store the results in the InfluxDB databse
library(influxdbr)
influx.con <- influx_connection(scheme = "http", host = "localhost", port = 8086)
db.name <- "ForecastTEST" # TODO: mechanism to use the credentials to create the database
create_database(influx.con, db.name)