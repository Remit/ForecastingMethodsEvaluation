#!/usr/bin/env Rscript
# Execution format for the command line
# - Single File Mode: Rscript ForecastingService.R --target=data.csv --type=FILE
# - Batch Mode [with parallelization]: Rscript ForecastingService.R --target=/data --type=BATCH
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
      
      if(processing.type == "BATCH") {
        
        if(!grepl(".csv", target)) {
          print("Specified file is not of type 'csv'. Please, provide the correct file.")
        } else {
          suppressMessages(library(parallel))
          source("DataPreprocessing.R")
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
          }) 
          
          data.raw <- read.csv2(file = target, header = F, sep = ",", stringsAsFactors = F)
          lst <- ts.preprocessing.matrix.Instana(data.raw)
          scores.and.models <- overall.testing(lst[11], cl)#lst[1:10]
          save(scores.and.models, file = "ScoresAndModels.RData")
          stopCluster(cl)
        }
      } else if(processing.type == "SINGLE") {
        
      } else {
        print("Inappropriate value for 'type' command line parameter. Appropriate values are: 'SINGLE', 'BATCH'.")
      }
    }
  }
}


# https://deanattali.com/2015/05/09/setup-rstudio-shiny-server-digital-ocean/
