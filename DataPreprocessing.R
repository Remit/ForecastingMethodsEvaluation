# A function to preprocess a timeseries row in order to introduce NAs instead of negative values
ts.preprocessing <- function(row) {
  num.row = as.numeric(row)
  num.row.NA = replace(num.row, num.row == -1, NA)
  if(length(num.row) - sum(is.na(num.row.NA)) >= 2) {
    num.row.interpolated = na.interpolation(num.row.NA)
  } else {
    # In the worst case, replace NA value with a non-NA value from the same time series
    num.row.interpolated = na.replace(num.row.NA, fill = num.row.NA[which(!is.na(num.row.NA))[[1]]])
  }
  end   <- as.POSIXct("2017-11-06 10:41:23", "%Y-%m-%d %H:%M:%S")
  start <- end - length(num.row) * 60 * 60 + 3600
  row.ts <- ts(num.row.interpolated, start = as.numeric(start), end = as.numeric(end), deltat = 3600)
  
  # Getting rid of time series with only zero values
  if(sum((row.ts < -1e-03) | (row.ts > 1e-03)) == 0) {
    row.ts <- NULL
  }
  
  return(list(row.ts))
}

# A function to preprocess the whole matrix of the data
ts.preprocessing.matrix.Instana <- function(data.raw) {
  lst = apply(data.raw, 1, ts.preprocessing)
  i <- 1
  lst.filtered <- list()
  while(i <= length(lst)) {
    if(!is.null(lst[[i]][[1]])) {
      lst.filtered <- c(lst.filtered, lst[[i]])
    }
    i <- i + 1
  }
  return(lst.filtered)
}

# A function to plot a list of time series on the same plot
plot.ts.list <- function(list.of.ts) {
  end   = as.POSIXct("2017-11-06 10:41:23", "%Y-%m-%d %H:%M:%S")
  start = end - length(list.of.ts[[1]]) * 60 * 60 + 3600
  df <- data.frame(x=rep(seq(as.numeric(start), as.numeric(end), 3600), length(list.of.ts)),
                   val=unlist(list.of.ts), 
                   variable=rep(paste0("category", 1:length(list.of.ts)), each=length(list.of.ts[[1]])))
  return(ggplot(data = df, aes(x=x, y=val)) + geom_line(aes(colour=variable)))
}