library(reshape2)

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

# A function to determine different characteristics of the single time series
compute.characteristics.for.single.ts <- function(time.series) {
  general.char <- summary(time.series)
  names(general.char) <- c("min", "1stQu", "median", "mean", "3rdQu", "max")
  char.df <- as.data.frame(as.array(general.char))
  names(char.df) <- c("var.name", "val")
  char.df$id <- rep(1, nrow(char.df))
  char.df <- dcast(data = char.df, formula = id~var.name, value.var = "val")
  char.df <- char.df[,names(char.df) != "id"]
  perc.zeros <- sum(time.series == 0) / length(time.series)
  char.df$zeros.perc <- perc.zeros
  ts.spectrum <- spectrum(time.series)
  period <- (1 / ts.spectrum$freq[which(ts.spectrum$spec == max(ts.spectrum$spec))]) / 3600
  char.df$period <- period
  return(char.df)
}

# A function to compute characteristics of all the available time series
compute.characteristics <- function(ts.list) {
  chars.list <- lapply(ts.list, compute.characteristics.for.single.ts)
  chars.df <- as.data.frame(do.call(rbind, chars.list))
  return(chars.df)
}

corellogram.limits <- function(acf.obj) {
  n <- acf.obj$n.used
  ret <- list()
  ret$lb <- - 1 / n - 2 / sqrt(n)
  ret$ub <- - 1 / n + 2 / sqrt(n)
  return(ret)
}
#hclust
clusters <- hclust(dist(iris[, 3:4]))
plot(clusters)