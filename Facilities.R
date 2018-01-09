is.weekend <- function(wkd) {
  if((wkd == "Saturday") || (wkd == "Sunday")) {
    return(1)
  } else {
    return(0)
  }
}

mark.time <- function(start, end, discretion) {
  discretion.string = paste0(discretion, " sec")
  ts.raw = seq(start, end, discretion.string)
  marks = weekdays(ts.raw)
  marked.vector = sapply(marks, is.weekend)
  return(marked.vector)
}