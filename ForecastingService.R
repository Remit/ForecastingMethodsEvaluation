#!/usr/bin/env Rscript
# Execution format for the command line
# - Single File Mode: Rscript ForecastingService.R --target=data.csv --starttime=1518524056 --type=SINGLE --client=client1 [--predsteps=20] --influx.dbhost=localhost:8086 --influx.dbuser=admin --influx.dbpassword=admin --mongo.dbhost=localhost --mongo.dbuser=admin --mongo.dbpassword=admin
# - Batch Mode [with parallelization]: Rscript ForecastingService.R --target=data.csv --starttime=1518524056 --type=BATCH

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
          
          # Creating connections to databases
          influx.con <- NULL
          mongo.con <- NULL
          influx.db.name <- ""
          if(processing.type == "SINGLE") {
            client.prefix <- "--client="
            
            client <- cmd.args[which(grepl(client.prefix, cmd.args))]
            if(length(client) == 0) {
              print("No 'client' command line parameter found. Please, specify the '--client' parameter with an appropriate value.")
            } else {
              client <- substring(client,
                                  nchar(client.prefix) + 1,
                                  nchar(client))
              
              # Creating connection to InfluxDB for time series in case of SINGLE type of processing
              # Used by service to process and store the results in InfluxDB
              influx.dbhost.prefix <- "--influx.dbhost="
              
              influx.dbhost <- cmd.args[which(grepl(influx.dbhost.prefix, cmd.args))]
              if(length(influx.dbhost) == 0) {
                print("No 'influx.dbhost' command line parameter found. Please, specify the '--influx.dbhost' parameter with an appropriate value.")
              } else {
                influx.dbhost <- substring(influx.dbhost,
                                    nchar(influx.dbhost.prefix) + 1,
                                    nchar(influx.dbhost))
                influx.dbhost.parts <- strsplit(influx.dbhost, split=':', fixed=TRUE)[[1]]
                if(length(influx.dbhost.parts) != 2) {
                  print("'influx.dbhost' command line parameter is incorrect. The format should be: <IP ADDRESS>:<PORT>.")
                } else {
                  influx.host <- influx.dbhost.parts[1]
                  influx.port <- as.numeric(influx.dbhost.parts[2])
                  
                  # Getting user and password
                  influx.dbuser.prefix <- "--influx.dbuser="
                  influx.dbpassword.prefix <- "--influx.dbpassword="
                  
                  influx.dbuser <- cmd.args[which(grepl(influx.dbuser.prefix, cmd.args))]
                  influx.dbpassword <- cmd.args[which(grepl(influx.dbpassword.prefix, cmd.args))]
                  
                  influx.user <- "root"
                  influx.password <- "root"
                  if((length(influx.dbuser) != 0) && (length(influx.dbpassword) != 0)) {
                    influx.user <- substring(influx.dbuser,
                                      nchar(influx.dbuser.prefix) + 1,
                                      nchar(influx.dbuser))
                    influx.password <- substring(influx.dbpassword,
                                          nchar(influx.dbpassword.prefix) + 1,
                                          nchar(influx.dbpassword))
                  }
                  
                  library(influxdbr)
                  influx.con <- influx_connection(scheme = "http",
                                                  host = influx.host,
                                                  port = influx.port,
                                                  user = influx.user,
                                                  pass = influx.password)
                  influx.db.name <- client
                  if(!grepl(client, show_databases(influx.con))) {
                    create_database(influx.con, influx.db.name)
                  }
                }
              }
            
              # Creating connection to MongoDB for score and duration data in case of SINGLE type of processing
              
              mongo.db.name <- "performancedata"
              collection.name <- "metrics"
              mongo.dbhost.prefix <- "--mongo.dbhost="
              
              mongo.dbhost <- cmd.args[which(grepl(mongo.dbhost.prefix, cmd.args))]
              if(length(mongo.dbhost) == 0) {
                print("No 'mongo.dbhost' command line parameter found. Please, specify the '--mongo.dbhost' parameter with an appropriate value.")
              } else {
                mongo.dbhost <- substring(mongo.dbhost,
                                           nchar(mongo.dbhost.prefix) + 1,
                                           nchar(mongo.dbhost))
                  
                # Getting user and password
                mongo.dbuser.prefix <- "--mongo.dbuser="
                mongo.dbpassword.prefix <- "--mongo.dbpassword="
                
                mongo.dbuser <- cmd.args[which(grepl(mongo.dbuser.prefix, cmd.args))]
                mongo.dbpassword <- cmd.args[which(grepl(mongo.dbpassword.prefix, cmd.args))]
                
                mongo.user <- "root"
                mongo.password <- "root"
                if((length(mongo.dbuser) != 0) && (length(mongo.dbpassword) != 0)) {
                  mongo.user <- substring(mongo.dbuser,
                                           nchar(mongo.dbuser.prefix) + 1,
                                           nchar(mongo.dbuser))
                  mongo.password <- substring(mongo.dbpassword,
                                               nchar(mongo.dbpassword.prefix) + 1,
                                               nchar(mongo.dbpassword))
                  
                  # Special situation in case of symbol '@' and ':' usage for username and/or password
                  mongo.user <- gsub("@", "%40", mongo.user)
                  mongo.password <- gsub("@", "%40", mongo.password)
                  mongo.user <- gsub(":", "%3A", mongo.user)
                  mongo.password <- gsub(":", "%3A", mongo.password)
                }
                
                mongo.url <- paste0("mongodb://", mongo.user, ":", mongo.password, "@", mongo.dbhost)
                library(mongolite)
                mongo.con <- mongo(collection = collection.name,
                                   db = mongo.db.name,
                                   url = mongo.url)
              }
            }
          }
          
          suppressMessages(library(parallel))
          source("DataPreprocessing.R")
          data.raw <- read.csv2(file = target, header = F, sep = ",", stringsAsFactors = F)
          lst <- ts.preprocessing.matrix(data.raw, start.time)
          
          no_cores <- detectCores()
          if(no_cores > 1) {
            no_cores <- no_cores - 1
          }
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
            suppressMessages(library(mongolite))
          }) 
          
          # Main Computation
          scores.and.models <- overall.testing(lst,
                                               cl,
                                               start.time,
                                               predsteps.num,
                                               influx.con,
                                               influx.db.name,
                                               mongo.con)
          stopCluster(cl)
          
          if(processing.type == "BATCH") {
            save(scores.and.models, file = "ScoresAndModels.RData")
          }
        }
      }
    }
  }
}