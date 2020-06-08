tolerated_bouts <- function(bouts_values, bouts_lengths,
    timethresholds = c(5, 10, 30, 60), tolerance_class = c(4, 3, 2),
    Nepoch_per_minute) {
  
  timethresholds <- sort(timethresholds, decreasing = TRUE)
  for (c in 1:length(tolerance_class)) {
      for (t in 1:(length(timethresholds) - 1)) {
          bb <- tolerance(bouts_values, bouts_lengths, tolerance_class[c], timethresholds[t + 1], timethresholds[t], Nepoch_per_minute)
      }
  }
  return(bb)
}
