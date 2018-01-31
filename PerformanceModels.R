library(influxdbr)
library(mongolite)
library(dplyr)
library(xts)
library(astsa)

influx.con <- influx_connection(scheme = "http", host = "34.217.33.66", port = 32589, user = "root", pass = "root")
dbs <- show_databases(influx.con)[[1]]
db.name <- dbs[which(dbs != "_internal")]

# Function to get all possible values for a specific metric for a specific node for a specific namespace for a specific pod name
get.values.by.pod.name <- function(pod.name = "",
                                   influx.con = NULL,
                                   db.name = "",
                                   metric = "",
                                   node = "",
                                   name.space = "") {
  time.series.df <- tryCatch(
    { 
      time.series.raw <- influx_select(influx.con,
                                       db.name,
                                       field_keys = "value",
                                       rp = "default",
                                       measurement = metric,
                                       where = paste0("type = 'pod' AND pod_name = '",
                                                      pod.name,
                                                      "' AND namespace_name = '",
                                                      name.space,
                                                      "' AND nodename = '",
                                                      node,
                                                      "'"),
                                       group_by = "*")
      time.series <- time.series.raw[[1]][[1]]
      data.frame(pod.name = rep(pod.name, length(time.series)),
                 time = as.numeric(time(time.series)),
                 value = time.series$value)
    },
    error=function(cond) {
      er.ret <- data.frame(pod.name = character(),
                           time = numeric(),
                           value = numeric())
      return(er.ret)
    })
  
  return(time.series.df)
}

# Function to get all possible values for a specific metric for a specific node for a specific namespace
get.values.by.namespace <- function(name.space = "",
                                    influx.con = NULL,
                                    db.name = "",
                                    pod.names = NULL,
                                    metric = "",
                                    node = "") {
  if(is.null(pod.names)) {
    pod.names.raw <- influx_query(influx.con,
                                  db = db.name,
                                  query = paste0("SHOW TAG VALUES FROM uptime WITH KEY = pod_name WHERE nodename = '", node, "' AND namespace_name = '", name.space, "'"),
                                  return_xts = FALSE)
    pod.names <- pod.names.raw[[1]]$value
  }
  
  vals.by.name.space.lst <- lapply(pod.names, get.values.by.pod.name, influx.con, db.name, metric, node, name.space)
  vals.by.name.space <- do.call("rbind", vals.by.name.space.lst)
  vals.by.name.space$name.space <- rep(name.space, nrow(vals.by.name.space))
  return(vals.by.name.space)
}

# Function to get values for all node-wide metrics
get.node.wide.values <- function(node = "",
                                 influx.con = NULL,
                                 db.name = "",
                                 metric = "") {
  
  time.series.df <- tryCatch(
    { 
      time.series.raw <- influx_select(influx.con,
                                       db.name,
                                       field_keys = "value",
                                       rp = "default",
                                       measurement = metric,
                                       where = paste0("type = 'node' AND nodename = '",
                                                      node,
                                                      "'"),
                                       group_by = "*")
      time.series <- time.series.raw[[1]][[1]]
      data.frame(pod.name = rep("", length(time.series)),
                 time = as.numeric(time(time.series)),
                 value = time.series$value,
                 name.space = rep("", length(time.series)))
    },
    error=function(cond) {
      er.ret <- data.frame(pod.name = character(),
                           time = numeric(),
                           value = numeric(),
                           name.space = character())
      return(er.ret)
    })
  
  return(time.series.df)
}

# Function to get all possible values for a specific metric for a specific node
get.values.by.node <- function(node = "",
                               influx.con = NULL,
                               db.name = "",
                               name.spaces = NULL,
                               pod.names = NULL,
                               metric = "") {
  vals.by.node <- NULL
  if(!grepl("node", metric)) {
    if(is.null(name.spaces)) {
      ns.raw <- influx_query(influx.con,
                             db = db.name,
                             query = paste0("SHOW TAG VALUES FROM uptime WITH KEY = namespace_name WHERE nodename = '",node,"'"),
                             return_xts = FALSE)
      name.spaces <- ns.raw[[1]]$value
    }
    
    vals.by.node.lst <- lapply(name.spaces, get.values.by.namespace, influx.con, db.name, pod.names, metric, node)
    vals.by.node <- do.call("rbind", vals.by.node.lst)
  } else {
    vals.by.node <- get.node.wide.values(node, influx.con, db.name, metric)
  }
  
  vals.by.node$node <- rep(node, nrow(vals.by.node))
  return(vals.by.node)
}

# Function to get all possible values for a specific metric
get.values.by.metric <- function(metric = "",
                                 influx.con = NULL,
                                 db.name = "",
                                 nodes = NULL,
                                 name.spaces = NULL,
                                 pod.names = NULL) {
  if(is.null(nodes)) {
    nodes.raw <- influx_query(influx.con,
                              db = db.name,
                              query = 'SHOW TAG VALUES FROM uptime WITH KEY = nodename',
                              return_xts = FALSE)
    nodes <- nodes.raw[[1]]$value
  }
  
  vals.by.metric.lst <- lapply(nodes, get.values.by.node, influx.con, db.name, name.spaces, pod.names, metric)
  vals.by.metric <- do.call("rbind", vals.by.metric.lst)
  vals.by.metric$metric <- rep(metric, nrow(vals.by.metric))
  return(vals.by.metric)
}

# Function to get all possible value from InfluxDB, includes some filters (by default - extracts everything)
get.values <- function(influx.con = NULL,
                       db.name = "",
                       metrics = NULL,
                       nodes = NULL,
                       name.spaces = NULL,
                       pod.names = NULL) {
  
  if(is.null(metrics)) {
    metrics.raw <- influx_query(influx.con,
                                db = db.name,
                                query = 'SHOW MEASUREMENTS',
                                return_xts = FALSE)
    
    metrics <- metrics.raw[[1]]$name
  }
  
  vals.lst <- lapply(metrics, get.values.by.metric, influx.con, db.name, nodes, name.spaces, pod.names)
  vals <- do.call("rbind", vals.lst)
  return(vals)
}

performance.data <- get.values(influx.con, db.name)
#TODO: delete when it is not necessary anymore [Adjustment for wrong clocks]
performance.data$time <- performance.data$time - 3600

# Getting requests data from the mongo
mongo.url <- "mongodb://MongoServer3:cloud%40SecureMongod75@34.217.33.66"
collection.name <- "kubeserver"
db.names <- c("t2small", "t2large", "t22xlarge")

extract.data.for.db <- function(db.name, mongo.url, collection.name) {
  conMongo.requests <- mongo(collection.name, db.name, mongo.url)
  
  db.data <- list()
  if(conMongo.requests$count() > 0) {
    Requests.data <- conMongo.requests$find()
    
    errors.ts.adapted <- Requests.data$aggregate$errors$ETIMEDOUT
    errors.ts.adapted.vec <- as.vector(coredata(errors.ts.adapted))
    errors.ts.adapted.vec[is.na(errors.ts.adapted.vec)] <- 0
    errors.ts <- xts(errors.ts.adapted.vec, strptime(Requests.data$aggregate$timestamp, "%Y-%m-%dT%H:%M:%S"))
    
    latency.ts.adapted <- Requests.data$aggregate$latency$p99
    latency.ts.adapted.vec <- as.vector(coredata(latency.ts.adapted))
    latency.ts.adapted.vec[is.na(latency.ts.adapted.vec)] <- 0
    latency.ts <- xts(latency.ts.adapted.vec, strptime(Requests.data$aggregate$timestamp, "%Y-%m-%dT%H:%M:%S"))
    
    requests.ts <- xts(Requests.data$aggregate$rps, strptime(Requests.data$aggregate$timestamp, "%Y-%m-%dT%H:%M:%S"))
    
    db.data$type <- db.name
    db.data$errors.ts <- errors.ts[,1]
    db.data$latency.ts <- latency.ts[,1]
    db.data$requests.ts <- requests.ts$count
    
  }
  
  return(db.data)
}

data.for.vm.types <- lapply(db.names, extract.data.for.db, mongo.url, collection.name)

# TODO: add wrapper functions to make graphs for diff. metrics and analyse time series (deriving the model)
# TODO: add flexible analysis for diff. metrics and pods

preprocess.data.for.metric.and.pod.and.vm.type <- function(performance.data,
                                                           data.for.vm.types,
                                                           metric,
                                                           pod = NULL,
                                                           vm.type) {
  performance.data.for.metric.and.vm <- NULL
  if(is.null(pod)) {
    performance.data.for.metric.and.vm <- performance.data[(performance.data$metric == metric), ]
    performance.data.for.metric.and.vm.grpd <- group_by(performance.data.for.metric.and.vm, pod.name)
    performance.data.for.metric.and.vm <- summarise(performance.data.for.metric.and.vm.grpd,
                                                    value = sum(value),
                                                    metric = unique(metric)
    )
  } else {
    performance.data.for.metric.and.vm <- performance.data[(performance.data$metric == metric) & (performance.data$pod.name == pod), ]
  }
  
  performance.data.time <- as.POSIXct(performance.data.for.metric.and.vm$time, origin="1970-01-01")
  performance.data.ts <- xts(performance.data.for.metric.and.vm$value, performance.data.time)
  
  data.index <- which(grepl(vm.type, data.for.vm.types))
  data.for.vm.type <- data.for.vm.types[[data.index]]
  
  # Adjusting timelines for different timeseries in order to conduct the analysis
  performance.data.ts.adapted <- as.numeric(performance.data.time)
  requests.ts.adapted <- as.numeric(time(data.for.vm.type$requests.ts))
  
  baseline.ts <- NULL
  adaptable.ts <- NULL
  adaptable.vals <- list()
  requests.ts <- data.for.vm.type$requests.ts
  errors.ts <- data.for.vm.type$errors.ts
  latency.ts <- data.for.vm.type$latency.ts
  
  if(length(requests.ts.adapted) < length(performance.data.ts.adapted)) {
    baseline.ts <- requests.ts.adapted
    adaptable.ts <- performance.data.ts.adapted
    adaptable.vals$vals <- performance.data.for.metric.and.vm$value
  } else {
    baseline.ts <- performance.data.ts.adapted
    adaptable.ts <- requests.ts.adapted
    adaptable.vals$rps <- requests.ts
    adaptable.vals$errors <- errors.ts
    adaptable.vals$latency <- latency.ts
  }
  
  baseline.ts.diff <- diff(baseline.ts)
  baseline.ts.diff.half.intvl <- baseline.ts.diff / 2
  intervals <- data.frame(lb = baseline.ts,
                          ub = baseline.ts)
  intervals[1,"lb"] <- 0
  intervals[2:nrow(intervals),"lb"] <- intervals[2:nrow(intervals),"lb"] - ceiling(baseline.ts.diff.half.intvl)
  intervals[1:(nrow(intervals) - 1),"ub"] <- intervals[1:(nrow(intervals) - 1),"ub"] + floor(baseline.ts.diff.half.intvl)
  intervals[nrow(intervals),"ub"] <- Inf
  
  check.interval <- function(value, intervals) {
    which((value >= intervals$lb) & (value < intervals$ub))
  }
  
  marked.ts.values <- unlist(sapply(adaptable.ts, check.interval, intervals))
  
  adapt.ts.vals <- function(adaptable.vals, adaptable.ts, marked.ts.values) {
    adabtable.df <- data.frame(adaptable.time.series = adaptable.ts,
                               adaptable.values = adaptable.vals,
                               interval = marked.ts.values)
    adabtable.df.grp <- group_by(adabtable.df, adabtable.df$interval)
    names(adabtable.df.grp) <- c("adaptable.time.series", "adaptable.values", "interval", "group")
    adabtable.df.grp.summary <- summarise(adabtable.df.grp, new.val = mean(adaptable.values))
    names(adabtable.df.grp.summary) <- c("interval", "val")
    '%!in%' <- function(x,y)!('%in%'(x,y))
    adj.indices <- which(seq(1,length(baseline.ts)) %!in% adabtable.df.grp.summary$interval)
    
    fill.in.gaps <- function(index.val, adabtable.df.grp.summary) {
      ret <- NULL
      if(index.val < min(adabtable.df.grp.summary$interval)) {
        ret <- adabtable.df.grp.summary[adabtable.df.grp.summary$interval == min(adabtable.df.grp.summary$interval),"val"][[1]]
      } else if(index.val > max(adabtable.df.grp.summary$interval)) {
        ret <- adabtable.df.grp.summary[adabtable.df.grp.summary$interval == max(adabtable.df.grp.summary$interval),"val"][[1]]
      } else {
        differences <- data.frame(intvl = adabtable.df.grp.summary$interval,
                                  diff = adabtable.df.grp.summary$interval - index.val)
        differences.pos <- differences[differences$diff > 0,]
        differences.neg <- differences[differences$diff < 0,]
        start.ind <- differences.neg[which.max(differences.neg$diff), "intvl"]
        end.ind <- differences.pos[which.min(differences.neg$diff), "intvl"]
        start.val <- adabtable.df.grp.summary[adabtable.df.grp.summary$interval == start.ind,"val"][[1]]
        end.val <- adabtable.df.grp.summary[adabtable.df.grp.summary$interval == end.ind,"val"][[1]]
        ret <- mean(c(start.val, end.val))
      }
      
      return(ret)
    }
    
    vals.for.gaps <- unlist(sapply(adj.indices, fill.in.gaps, adabtable.df.grp.summary))
    extended.grps <- data.frame(interval = adj.indices,
                                val = vals.for.gaps)
    adabtable.df.grp.summary.adjusted <- rbind(adabtable.df.grp.summary, extended.grps)
    adabtable.df.grp.summary.adjusted <- adabtable.df.grp.summary.adjusted[with(adabtable.df.grp.summary.adjusted, order(interval)), ]
    
    adapted.ts <- xts(adabtable.df.grp.summary.adjusted$val,
                      as.POSIXct(baseline.ts,  origin="1970-01-01"))
  }
  
  adapted.ts <- lapply(adaptable.vals, adapt.ts.vals, adaptable.ts, marked.ts.values)
  
  # Reassigning adapted timelines to sources
  if(length(requests.ts.adapted) < length(performance.data.ts.adapted)) {
    performance.data.ts <- adapted.ts$vals
  } else {
    latency.ts <- adapted.ts$latency
    requests.ts <- adapted.ts$rps
    errors.ts <- adapted.ts$errors
  }
  
  result <- list()
  result$latency.ts <- latency.ts
  result$requests.ts <- requests.ts
  result$errors.ts <- errors.ts
  result$performance.data.ts <- performance.data.ts
  result$performance.data.metric <- metric
  
  return(result)
}


list.available.pods.for.vm.type <- function(vm.type, performance.data) {
  vm.types <- c("t2small", "t2large", "t22xlarge")
  nodes <- c("ip-172-31-41-54", "ip-172-31-44-145", "ip-172-31-34-70")
  node <- nodes[which(vm.types == vm.type)]
  
  pods <- unique(performance.data[performance.data$node == node, "pod.name"])
  return(pods)
}

list.available.pods.for.vm.type("t2small", performance.data)
list.available.pods.for.vm.type("t2large", performance.data)
list.available.pods.for.vm.type("t22xlarge", performance.data)

vm.types <- c("t2small", "t2large", "t22xlarge")
nodes <- c("ip-172-31-41-54", "ip-172-31-44-145", "ip-172-31-34-70")

t2.small.data <- preprocess.data.for.metric.and.pod.and.vm.type(performance.data[performance.data$node == nodes[which(vm.types == "t2small")],],
                                                                data.for.vm.types,
                                                                "cpu/usage_rate",
                                                                "server-7f95cf4d9d-zf89g",
                                                                "t2small")
t2.large.data <- preprocess.data.for.metric.and.pod.and.vm.type(performance.data[performance.data$node == nodes[which(vm.types == "t2large")],],
                                                                data.for.vm.types,
                                                                "cpu/usage_rate",
                                                                "server-7f95cf4d9d-8zlgz",
                                                                "t2large")
t2.2xlarge.data <- preprocess.data.for.metric.and.pod.and.vm.type(performance.data[performance.data$node == nodes[which(vm.types == "t22xlarge")],],
                                                                  data.for.vm.types,
                                                                  "cpu/usage_rate",
                                                                  "server-7f95cf4d9d-7rk6j",
                                                                  "t22xlarge")


par(mfrow = c(4,3))
plot(t2.small.data$performance.data.ts)
plot(t2.large.data$performance.data.ts)
plot(t2.2xlarge.data$performance.data.ts)

plot(t2.small.data$errors.ts)
plot(t2.large.data$errors.ts)
plot(t2.2xlarge.data$errors.ts)

plot(t2.small.data$latency.ts)
plot(t2.large.data$latency.ts)
plot(t2.2xlarge.data$latency.ts)

plot(t2.small.data$requests.ts)
plot(t2.large.data$requests.ts)
plot(t2.2xlarge.data$requests.ts)

start.t <- as.numeric(strptime("2018-01-29T12:01:00","%Y-%m-%dT%H:%M:%S"))
end.t <- as.numeric(strptime("2018-01-29T12:43:00","%Y-%m-%dT%H:%M:%S"))
requests.ts.regular <- window(testing.data$requests.ts, start = start.t, end = end.t)
performance.data.ts.regular <- window(testing.data$performance.data.ts, start = start.t, end = end.t)

predictor <- as.vector(coredata(requests.ts.regular))
result <- as.vector(coredata(performance.data.ts.regular))
#lag2.plot(diff(predictor), result, 20)

predictor.arima <- forecast::Arima(predictor,
                                   order = c(0, 1, 0),
                                   method = "CSS")
predictor.residuals <- predictor.arima$residuals
result.residuals <- residuals(forecast::Arima(result, model=predictor.arima))
par(mfrow = c(1,1))
ccf(predictor.residuals, result.residuals,na.action=na.omit)

#Deriving the service performance model
# Service model includes:
# @ for each tested VM type:
#   - overall bottleneck (numer of requests when the microservices achieves the peak performance)
#   - performance chracteristics as dependent on the number of requests

# To determine the bottleneck
t2.small.QoS.errors.total <- 1000
t2.small.total.errors <- cumsum(coredata(t2.small.data$errors.ts))
t2.small.bottleneck.index <- min(which(t2.small.total.errors > t2.small.QoS.errors.total)) - 1
t2.small.bottleneck <- t2.small.data$requests.ts[bottleneck.index][[1]]

#SHOW MEASUREMENTS