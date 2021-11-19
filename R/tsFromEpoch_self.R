#' tsFromEpoch_self
#'
#' @description 'tsFromEpoch_self' calculates the timestamp from epoch number or epoch number from timestamp
#'
#' @param object An object of class accfile
#' @param minn An integer, either specifying the epoch number or the timestamp in aPOSIXformat(e.g., ’%Y-%m-%d %H:%M:%S’)
#'
#' @return ss An integer that defines the epoch number  corresponding  to  a timestamp
#' @export

tsFromEpoch_self <- function (object, minn) {
  if (any(minn <= 0)) {
    stop("x must be positive")
  }
  if (any(minn > object$info$nobs)) {
    warning("Timestamp is outside observed time interval")
  }
  ss = object$info$ts_start + object$info$epoch * (as.integer(minn) -  1)
  even_indices = seq(2, length(ss), by = 2)
  ss[even_indices] = ss[even_indices] + 1
  return(ss)
}
