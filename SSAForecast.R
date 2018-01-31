require(forecast)
require(Rssa)

source(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Facilities.R"))

# Function to derive the forecasts using SSA model
ssa.forecast <- function(train.timeseries, pred.steps) {
  
  ssa.model = ssa(train.timeseries$series)
  # Heuristic to determine the groups using correlations
  wcor.matrix <- wcor(ssa.model, groups = 1:20)
  unnaceptable.wcor.pairs <- (wcor.matrix < 0.95) & (wcor.matrix > 0.01) # To folter approximate zeros
  # We will be interested in top-left quadrant of pairs with FALSE values (~ correlations close to 1) using symmetry
  unnaceptable.wcor.pairs.cols <- split(unnaceptable.wcor.pairs,
                                        rep(1:ncol(unnaceptable.wcor.pairs),
                                            each = nrow(unnaceptable.wcor.pairs)))
  
  contains.true <- function(vector.data) {
    index <- which(vector.data == TRUE)
    if(length(index) != 0) {
      index <- min(index)
    } else {
      index <- -1
    }
    
    return(index)
  }
  
  indices <- lapply(unnaceptable.wcor.pairs.cols, contains.true)
  
  indices.pairs.df <- data.frame(col.index = c(1:length(indices)),
                                 row.index = unlist(indices))
  indices.pairs.df.filtered <- indices.pairs.df[indices.pairs.df$row.index > 0, ]
  col.index.max <- min(indices.pairs.df.filtered[indices.pairs.df.filtered$row.index > indices.pairs.df.filtered$col.index, "row.index"]) - 1
  row.index.max <- min(indices.pairs.df.filtered[indices.pairs.df.filtered$row.index < indices.pairs.df.filtered$col.index, "col.index"]) - 1
  
  group.max <- 10 #default value
  if(col.index.max == row.index.max) {
    group.max <- col.index.max
  }
  
  ssa.forecast <- forecast(ssa.model,
                           groups = list(1:group.max),
                           method = "recurrent",
                           bootstrap = TRUE,
                           len = pred.steps,
                           R = 1000)
  
  mean.fc <- ssa.forecast$mean
  sd.fc <- sd(mean.fc)
  lb.80 <- mean.fc - 1.28 * sd.fc
  lb.95 <- mean.fc - 1.96 * sd.fc
  lower <- data.frame(lb.80 = lb.80,
                      lb.95 = lb.95)
  
  ub.80 <- mean.fc + 1.28 * sd.fc
  ub.95 <- mean.fc + 1.96 * sd.fc
  upper <- data.frame(ub.80 = ub.80,
                      ub.95 = ub.95)
  
  ssa.forecast$lower <- lower
  ssa.forecast$upper <- upper
  
  return(ssa.forecast)
}
