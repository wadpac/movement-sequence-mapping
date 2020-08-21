#' tolerance
#'
#' @param bouts_values ...
#' @param bouts_lengths ...
#' @param allow_bout ...
#' @param timethreshold1 ...
#' @param timethreshold2 ...
#' @param Nepoch_per_minute ...
#' @return tol
#' @export

tolerance <- function(bouts_values, bouts_lengths, allow_bout,
                      timethreshold1, timethreshold2, Nepoch_per_minute) {
  index = 1:length(bouts_lengths)
  index = index[!is.na(index)]
  Nbouts <- length(index)
  i = 1
  # Time thresholds are in minutes, but for the calculation they need to be expressed in epochs
  timethreshold1 = timethreshold1 * Nepoch_per_minute
  timethreshold2 = timethreshold2 * Nepoch_per_minute
    if (Nbouts >= 2) {
    while (i <= (Nbouts - 1)) {
      flag = 0 # to stop the while loop when it is changed to 1
      total_time = bouts_lengths[index[i]]
      tolerance_time = 0
      while (flag == 0 & total_time <= timethreshold2 & i <= (Nbouts - 1)) {
        total_time1 = total_time
        t1 <- bouts_lengths[(index[i] ):(index[i + 1])] # Look at successive epochs
        t2 <- bouts_values[(index[i] ):(index[i + 1])]
        cur_tol_low = sum(t1[t2 < allow_bout])
        cur_tol_upp = sum(t1[t2 > allow_bout])
        total_time = total_time + sum(bouts_lengths[(index[i] ):(index[i + 1])])
        tolerance_time1 = tolerance_time
        if (allow_bout == 4) { # Tolerance normal for MVPA (tor_flex_constant)
          tolerance_time = tolerance_time + cur_tol_low + cur_tol_upp
        } else {# Tolerance with allen lower bound (tor_flex_below)
          tolerance_time = tolerance_time + cur_tol_low
        }
        # tolerance_time1: total time spent outside the bout from previous while iteraturation
        # tolerance_time: total time spent outside the bout
        # total_time1: duration of this bout
        # total_time: duration of this bout and next bout combined
        if(total_time1 <= timethreshold1 & total_time > timethreshold2 & tolerance_time < total_time * 0.1
           & cur_tol_low < 3 * Nepoch_per_minute & i <= (Nbouts - 1) &  bouts_values[(index[i])] > 0 &
            bouts_values[(index[i+1])] > 0) {
          bouts_lengths[index[i+1]] = sum(bouts_lengths[(index[i]):(index[i+1])])
          bouts_lengths[(index[i])] = -1
          bouts_values[(index[i])] = -1
        }
        if(total_time > timethreshold2 | tolerance_time > total_time * 0.1 |
           cur_tol_low > 3 * Nepoch_per_minute) {
          flag = 1 # This causes the while loop to stop
        }
      }
      i = i + 1
    }
  }
  bouts_values = bouts_values[bouts_lengths != -1]
  bouts_lengths = bouts_lengths[bouts_lengths != -1]
  # Collapse succeeding segments with same value:
  lastvalue = bouts_values[1]
  lastindex = 1
  j  = 2
  while (j > 1) {
    if (bouts_values[j] == lastvalue) {
      # update last occurence with duration from current segment
      bouts_lengths[lastindex] = bouts_lengths[lastindex] + bouts_lengths[j]
      # remove current value
      bouts_lengths = bouts_lengths[-j]
      bouts_values = bouts_values[-j]
    } else {
      lastindex = j
      lastvalue = bouts_values[j]
      j = j + 1
    }
    if (j > length(bouts_values)) {
      j = 0
    }
  }
  Nbouts = length(bouts_values)
  tol <- list(values = bouts_values, lengths = bouts_lengths)
  return(tol)
}
