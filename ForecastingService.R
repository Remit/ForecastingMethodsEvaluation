#!/usr/bin/env Rscript
# Execution format for the command line
# - Single File Mode: Rscript ForecastingService.R --target=data.csv --starttime=1518524056 --type=SINGLE --client=client1 [--predsteps=20]
# - Batch Mode [with parallelization]: Rscript ForecastingService.R --target=data.csv --starttime=1518524056 --type=BATCH
# Real example: Rscript ForecastingService.R --target=/home/remit/dissCloud/Instana/data/metrics.csv -type=SINGLE --client=client1 --predsteps=10
cmd.args <- commandArgs()
script.file.prefix <- "--file="
full.script.path <- cmd.args[which(grepl(script.file.prefix, cmd.args))]
full.script.path <- substring(full.script.path,
                              nchar(script.file.prefix) + 1,
                              nchar(full.script.path))
script.directory <- dirname(full.script.path)
setwd(script.directory)
options(warn=-1)

source("PredictionCore.R")

target.prefix <- "--target="
type.of.processing.prefix <- "--type="

target <- cmd.args[which(grepl(target.prefix, cmd.args))]

if(length(target) == 0) {
  print("No 'target' command line parameter found. Please, specify the '--target' parameter with an appropriate value.")
} else {
  target <- substring(target,
                      nchar(target.prefix) + 1,
                      nchar(target))
  
  if(!file.exists(target)) {
    print("The specified target does not exist. Please, specify another target.")
  } else {
    processing.type <- cmd.args[which(grepl(type.of.processing.prefix, cmd.args))]
    
    if(length(processing.type) == 0) {
      print("No 'type' command line parameter found. Please, specify the '--type' parameter with an appropriate value.")
    } else {
      processing.type <- substring(processing.type,
                                   nchar(type.of.processing.prefix) + 1,
                                   nchar(processing.type))
      
      if((processing.type != "BATCH") && (processing.type != "SINGLE")) {
        print("Inappropriate value for 'type' command line parameter. Appropriate values are: 'SINGLE', 'BATCH'.")
      } else {
        if(!grepl(".csv", target)) {
          print("Specified file is not of type 'csv'. Please, provide the correct file.")
        } else {
          predsteps.prefix <- "--predsteps="
          predsteps.num <- -1
          predsteps <- cmd.args[which(grepl(predsteps.prefix, cmd.args))]
          if(length(predsteps) > 0) {
            # Number of prediction steps (in hours!)
            predsteps.num <- as.numeric(substring(predsteps,
                                                  nchar(predsteps.prefix) + 1,
                                                  nchar(predsteps)))
          }
          
          starttime.prefix <- "--starttime="
          start.time <- as.POSIXct(Sys.time())
          starttime <- cmd.args[which(grepl(starttime.prefix, cmd.args))]
          if(length(starttime) > 0) {
            # Number of prediction steps (in hours!)
            start.time <- as.POSIXct(as.numeric(substring(starttime,
                                                  nchar(starttime.prefix) + 1,
                                                  nchar(starttime))), origin="1970-01-01")
          }
          
          # Creating connection to InfluxDB for time series in case of SINGLE type of processing
          influx.con <- NULL
          db.name <- ""
          if(processing.type == "SINGLE") {
            client.prefix <- "--client="
            
            client <- cmd.args[which(grepl(client.prefix, cmd.args))]
            if(length(client) == 0) {
              print("No 'client' command line parameter found. Please, specify the '--client' parameter with an appropriate value.")
            } else {
              client <- substring(client,
                                  nchar(client.prefix) + 1,
                                  nchar(client))
              # Used by service to process and store the results in InfluxDB
              library(influxdbr)
              influx.con <- influx_connection(scheme = "http", host = "localhost", port = 8086)
              db.name <- client
              if(!grepl(client, show_databases(influx.con))) {
                create_database(influx.con, db.name)
              }
            }
          }
          
          suppressMessages(library(parallel))
          source("DataPreprocessing.R")
          data.raw <- read.csv2(file = target, header = F, sep = ",", stringsAsFactors = F)
          lst <- ts.preprocessing.matrix(data.raw, start.time)
          
          no_cores <- detectCores() - 1
          cl <- makeCluster(no_cores)
          
          clusterEvalQ(cl, {
            source("ARIMAForecast.R")
            source("ETSForecast.R")
            source("SSAForecast.R")
            source("SVMForecast.R")
            source("ForecastMetric.R")
            source("DataPreprocessing.R")
            source("LinearRegressionForecast.R")
            suppressMessages(library(imputeTS))
            suppressMessages(library(ggplot2))
            suppressMessages(library(tseries))
            suppressMessages(library(forecast))
            suppressMessages(library(xts))
            suppressMessages(library(influxdbr))
          }) 
          
          # Main Computation
          scores.and.models <- overall.testing(lst[11],
                                               cl,
                                               start.time,
                                               predsteps.num,
                                               influx.con,
                                               db.name)#lst[1:10]
          stopCluster(cl)
          
          if(processing.type == "BATCH") {
            save(scores.and.models, file = "ScoresAndModels.RData")
          }
        }
      }
    }
  }
}

# Installation (external) - https://stackoverflow.com/questions/1474081/how-do-i-install-an-r-package-from-source 
# https://deanattali.com/2015/05/09/setup-rstudio-shiny-server-digital-ocean/
