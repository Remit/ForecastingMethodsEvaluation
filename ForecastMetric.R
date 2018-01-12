# An interval forecast metric for forecast objects.
# Originally introduced in https://www.stat.washington.edu/raftery/Research/PDF/Gneiting2007jasa.pdf

# Sums the calculated metric for each pair of prediction interval boundaries for all the
# predicted points.
interval.score <- function(lb.95, ub.95, actual.vals, alpha){
  testing.data <- data.frame(lb.95 = lb.95, ub.95 = ub.95, actual.val = actual.vals)
  scores <- apply(testing.data, 1, estimate.interval.score, alpha)
  return(sum(scores))
}

# Computes the metric for the single prediction interval and actual value
estimate.interval.score <- function(row.to.estimate, alpha) {
  l <- row.to.estimate[1]
  u <- row.to.estimate[2]
  x <- row.to.estimate[3]
  estimate <- (u - l) + (2 / alpha) * (l - x) * identity(x < l) + (2 / alpha) * (x - u) * identity(x > u)
  return(estimate)
}