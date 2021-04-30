#' tolerated_bouts
#'
#' @description 'tolerated_bouts' identifies the bouts of which the length lie within a tolerance of 10% below the lower threshold with a maximum of three consecutive minutes and updates the vector of bouts accordingly
#'
#' @param bouts_values A vector representing the cut-point intensity classes of the corresponding bouts (e.g. 1 = SB, 2 = LPA, 3 = MPA, 4 = VPA)
#' @param bouts_lengths A vector representing the lengths (number of epochs) of the corresponding bouts
#' @param timethresholds A vector specifying the upper thresholds of the bout durations in minutes (Default : c(5, 10, 30, 60))
#' @param tolerance_class A vector specifying the order of the cut-point intensity classes for which tolerance is allowed (Default : c(4, 3, 2))
#' @param Nepoch_per_minute An integer defining the numer of epochs per minute
#' @param tolerance_function One of c("V1", "V2") defining the tolerance function used for the bout tolerance, where "V1" looks whether a pair of segments (based on intensity) is within a tolerance of 10% using the function 'tolerance'; "V2" looks at the whole of segments to identify breaks in bouts within this tolerance (Default : "V2")
#'
#' @return bb A list of: \item{values}{A vector with the updated bout values} \item{lengths}{A vector with the updated bout lengths}
#' @export

tolerated_bouts <- function(bouts_values, bouts_lengths,
                            timethresholds = c(5, 10, 30, 60), tolerance_class = c(4, 3, 2),
                            Nepoch_per_minute, tolerance_function="V1") {
  getbout = function(x, boutduration, boutcriter=0.8, epoch.size=15, maximum.break.dur= 3) {
    # function adapted from R package GGIR (bout.metric 6)
    # x: vector of 0 and 1, where we are interested in detect bouts of 1
    # boutduration: bout duration in minutes
    # boutcriter: ratio of bout that needs to meet boutcriteria
    # epoch.size: in seconds
    # maximum.break.dur: in minutes
    p = which(x == 1)
    x[is.na(x)] = 0 # ignore NA values in the unlikely event that there are any
    xt = x
    #look for breaks larger than 1 minute
    # Note: we do + 1 to make sure we look for breaks larger than but not equal to a minute,
    # this is critical when working with 1 minute epoch data
    lookforbreaks = zoo::rollmean(x=x, k=(60/epoch.size)+1, align="center", fill=rep(0,3))
    #insert negative numbers to prevent these minutes to be counted in bouts
    #in this way there will not be bouts breaks lasting longer than 1 minute
    xt[lookforbreaks == 0] = -boutduration
    RM = zoo::rollmean(x=xt, k=boutduration, align="center", fill=rep(0,3))
    p = which(RM >=boutcriter)
    starti = round(boutduration/2)
    # # only consider windows that at least start and end with value that meets criterium
    p = c(0, p, 0)
    if (epoch.size > 60) {
      epochs2check = 1
    } else {
      epochs2check = (60/epoch.size)
    }
    for (ii in 1:epochs2check) { # only check the first and last minutes of each bout
      # p are all epochs at the centre of the windows that meet the bout criteria
      # we want to check the start and end of sequence of which centres whether
      # the the epoch half the bout length before and the epoch half the bout
      # length after this centre meet the threshold criteria
      # So, we first zoom in on the edges of the sequence
      edges = which(diff(p) != 1)
      seq_start = p[edges + 1] # bout centre starts
      seq_start = seq_start[-1]
      seq_end = p[edges] # bout centre starts
      seq_end = seq_end[-1]
      length_xt = length(xt)
      seq_start = seq_start[which(seq_start > starti & seq_start < length_xt - starti)]
      seq_end = seq_end[which(seq_end > starti & seq_end < length_xt - starti)]
      if (length(seq_start) > 0) {
        for (bi in seq_start) {
          if (length_xt >= (bi - starti)) {
            if (xt[bi - starti] != 1) { # if it does not meet criteria then remove this p value
              p = p[-which(p == bi)]
            }
          }
        }
      }
      if (length(seq_end) > 0) {
        for (bi in seq_end) {
          if (length_xt >= (bi - starti)) {
            if (xt[bi + starti] != 1) {
              p = p[-which(p == bi)]
            }
          }
        }
      }

    }
    p = p[which(p != 0)]
    # now mark all epochs that are covered by the remaining windows
    for (gi in 0:boutduration) {
      inde = p-starti+gi
      xt[inde[which(inde > 0 & inde < length(xt))]] = 2
    }
    x[xt != 2] = 0
    x[which(xt == 2 & x != 0)] = 1
    return(x)
  }
  timethresholds <- sort(timethresholds, decreasing = TRUE)
  for (c in 1:length(tolerance_class)) {
    for (t in 1:(length(timethresholds) - 1)) {
      if (tolerance_function == "V1") {
        # old tolerance function
        bb <- tolerance(bouts_values, bouts_lengths, tolerance_class[c], timethresholds[t + 1], timethresholds[t], Nepoch_per_minute)
      } else if (tolerance_function == "V2") {
        # new approach (adapted from R package GGIR)
        ts = rep(bouts_values, times=bouts_lengths) # convert to time series
        ts2 = rep(0, length(ts))
        ts2[which(ts == tolerance_class[c])] = 1 # create binary time series
        padding = rep(0,Nepoch_per_minute)
        out = getbout(x=c(padding, ts2, padding), boutduration=timethresholds[t],
                      boutcriter=0.9, epoch.size=60/Nepoch_per_minute, maximum.break.dur= 3)
        out = out[Nepoch_per_minute:(length(out)-Nepoch_per_minute)]
        ts[which(out == 1)] = tolerance_class[c] # update time series with newly detected bouts
        bb = rle(ts) # convert back to rle format
      }
    }
  }
  return(bb)
}
