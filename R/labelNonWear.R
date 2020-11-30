#' labelNonWear
#'
#' @description 'labelNonWear' labels non-wear bouts as -999, without tolerance
#'
#' @param counts A vector containing the (aggregated) accelerometer counts
#' @param zerocount An integer that defines the non-wear time as number of consecutive epochs containing zero counts
#' @param epochlengthPerMin An integer that defines the number og epochs per minute
#'
#' @return counts A list that consists of \item{values}{A vector representing the cut-point classes of the corresponding bouts (e.g. 0 = non-wear, 1 = SB, 2 = LPA, 3 = MPA, 4 = VPA)} \item{lengths}{A vector representing the lengths (number of epochs) of the corresponding bout values}
#' @export

labelNonWear <- function(counts, zerocount, epochlengthPerMin) {
  bouts <- rle(counts)
  zeros <- which(bouts$values == 0 & bouts$lengths >= (zerocount * epochlengthPerMin)) # Find indices of the sequences with (> 60 min) of consecutive zero counts
  if (any(zeros)) {
    lengthsCumsum <- cumsum(bouts$lengths)
    ends    <- lengthsCumsum[zeros] # Find end positions of consecutive zero runs
    index   <- ifelse(zeros > 1, zeros - 1, 0)
    starts  <- lengthsCumsum[index] + 1 # Find start positions of consecutive zero runs
    if (0 %in% index) {
      starts<- c(1,starts)
    }
    for (i in 1:length(zeros)) {
      counts[starts[i]:ends[i]] <- -999 # Label positions in data as non-wear
      }
  }
  return(counts)
}
