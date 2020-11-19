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
                            Nepoch_per_minute, tolerance_function = "V2") {
  getbout = function(x, boutduration, boutcriter = 0.8, epoch.size = 5, maximum.break.dur = 3) {
    # function addapted from R package GGIR
    # x: vector of 0 and 1, where we are interested in detect bouts of 1
    # boutduration: bout duration in minutes
    # boutcriter: ratio of bout that needs to meet boutcriteria
    # epoch.size: in seconds
    # maximum.break.dur: in minutes
    p = which(x == 1)
    x[is.na(x)] = 0 # ignore NA values in the unlikely event that there are any
    xt = x
    #----------------------------------------
    # look for breaks larger than X minutes, and make sure these cannot be part of bouts
    lookforbreaks = zoo::rollmean(x=x,k=(60/epoch.size)*maximum.break.dur,align="center",fill=rep(0,3)) #
    #insert negative numbers to prevent these minutes to be counted in bouts
    #in this way there will not be bouts breaks lasting longer than X minutes
    xt[lookforbreaks == 0] = -(60/epoch.size) * boutduration # the negaitve value needs to be large enough to prevent bout detection
    #----------------------------------------
    # apply rolling mean to identify bouts
    # if there are no breaks this is 1, and we want it to be larger than boutcriter
    rollmean = zoo::rollmean(x=xt,k=boutduration,align="center",fill=rep(0,3)) #RollingMean
    p = which(rollmean > boutcriter)
    starti = round(boutduration/2)
    # only consider windows that at least start and end with value that meets criteria
    tri = p-starti
    keep = which(tri > 0 & tri < (length(x)-(boutduration-1)))
    if (length(keep) > 0) tri = tri[keep]
    p = p[which(x[tri] == 1 & x[tri+(boutduration-1)] == 1)]
    # now mark all epochs that are covered by the remaining windows
    for (gi in 1:boutduration) {
      inde = p-starti+(gi-1)
      xt[inde[which(inde > 0 & inde < length(xt))]] = 2 # assigning it 2 makes that we will keep these values below
    }
    x[xt != 2] = 0
    x[xt == 2] = 1
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
