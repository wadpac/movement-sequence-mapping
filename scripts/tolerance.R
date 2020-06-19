tolerance <- function(bouts_values, bouts_lengths, allow_bout,
    timethreshold1, timethreshold2, Nepoch_per_minute) {
  index = 1:length(bouts_lengths)
  index = index[bouts_values == allow_bout]
  index = index[!is.na(index)]
  Nbouts <- length(index)
  i = 1
  # Time thresholds are in minutes, but for the calculation they need to be expressed in epochs
  timethreshold1 = timethreshold1 * Nepoch_per_minute
  timethreshold2 = timethreshold2 * Nepoch_per_minute
  if(Nbouts >= 2) {
    while(i <= (Nbouts - 1)) {
      flag = 0 # to stop the while loop when it is changed to 1
      total_time = bouts_lengths[index[i]]
      tolerance_time = 0
      tt = i
      if (total_time > timethreshold2)
        i = i + 1
      while(flag == 0 & total_time <= timethreshold2 & i <= (Nbouts - 1)) {
        total_time1 = total_time
        t1 <- bouts_lengths[(index[i] + 1):(index[i + 1])] # Look at successive epochs
        t2 <- bouts_values[(index[i] + 1):(index[i + 1])]
        cur_tol_low = sum(t1[t2 < allow_bout])
        cur_tol_upp = sum(t1[t2 > allow_bout])
        total_time = total_time + sum(bouts_lengths[(index[i] + 1):(index[i + 1])])
        tolerance_time1 = tolerance_time
        if (allow_bout == 4) # Tolerance normal for MVPA (tor_flex_constant)
           tolerance_time = tolerance_time + cur_tol_low + cur_tol_upp
        else # Tolerance with allen lower bound (tor_flex_below)
           tolerance_time = tolerance_time + cur_tol_low
        
        #print(c(tolerance_time,tolerance_time1,total_time,total_time1,i))
        #print(total_time1>=timethreshold1  &  tolerance_time1<total_time1*0.1 & (total_time>timethreshold2 |tolerance_time>total_time*0.1 | cur_tol_low> 3*Nepoch_per_minute))
        if(total_time1 >= timethreshold1 & tolerance_time1 < total_time1 * 0.1 &
          (total_time > timethreshold2 | tolerance_time > total_time * 0.1 |
           cur_tol_low > 3 * Nepoch_per_minute) | (total_time1 >= timethreshold1 &
           tolerance_time < total_time1 * 0.1 &
           total_time < timethreshold2 & i == (Nbouts - 1))) {
            if(i >= 2) {
                bouts_lengths[index[tt]] = sum(bouts_lengths[(index[tt]):(index[i])])
                bouts_lengths[(index[tt] + 1):(index[i])] = -1
            }
            if(i == 1) {
                print(i)
                bouts_lengths[index[1]] = sum(bouts_lengths[(index[1]):(index[2])])
                bouts_lengths[(index[1] + 1):(index[2])] = -1
            }
        }
        if(total_time > timethreshold2 | tolerance_time > total_time * 0.1 |
           cur_tol_low > 3 * Nepoch_per_minute)
          flag = 1 # This causes the while loop to stop
        i = i + 1
      }
    }
    bouts_values = bouts_values[bouts_lengths != -1]
    bouts_lengths = bouts_lengths[bouts_lengths != -1]
  }
  Nbouts = length(bouts_values)
  tol <- list(values = bouts_values, lengths = bouts_lengths)
  return(tol)
}
