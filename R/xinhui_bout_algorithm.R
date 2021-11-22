#' xinhui_bout_algorithm
#'
#' @description 'xinhui_bout_algorithm' identifies and updates the segments (based on intensity) that fall within a tolerance of 10% by looking at pairs of segments
#'
#' @param bouts_values A vector representing the cut-point classes of the corresponding bouts (e.g. 1 = SB, 2 = LPA, 3 = MPA, 4 = VPA)
#' @param bouts_lengths A vector representing the lengths (number of epochs) of the corresponding bouts
#' @param allow_bout An integer specifying the cut-point intensity class for which tolerated bouts need to be identified
#' @param timethreshold1 An integer specifying the lower threshold for bout length
#' @param timethreshold2 An integer specifying the upper threshold for bout length
#' @param Nepoch_per_minute An integer specifying the number of epochs per minute
#'
#' @return tol A list of: \item{values}{A vector with the updated bout values} \item{lengths}{A vector with the updated bout lengths}
#' @export

xinhui_bout_algorithm <- function(bouts_values, bouts_lengths, allow_bout,
                      timethreshold1, timethreshold2, Nepoch_per_minute) {
  index = 1:length(bouts_lengths)
  #index = index[bouts_values == allow_bout]
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
        if (allow_bout == 4) { # Tolerance normal for (M)VPA (tor_flex_constant)
          tolerance_time = tolerance_time + cur_tol_low + cur_tol_upp
        } else {# Tolerance with allen lower bound (tor_flex_below)
          tolerance_time = tolerance_time + cur_tol_low
        }
        # tolerance_time1: total time spent outside the bout from previous while iteration
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
  lastindex = 1
  lastvalue = bouts_values[lastindex]
  j  = 2
  while (j > 1) {
    if (bouts_values[j] == lastvalue) {
      # update last occurrence with duration from current segment
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
