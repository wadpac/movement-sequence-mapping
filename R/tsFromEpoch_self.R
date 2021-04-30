#' tsFromEpoch_self
#'
#' @param object ...
#' @param minn ...
#' @return ss
#' @export

tsFromEpoch_self <- function (object, minn) {
  if (any(minn <= 0)) 
    stop("x must be positive")
  if (any(minn > object$info$nobs)) 
    warning("Timestamp is outside observed time interval")
  ss = object$info$ts_start + object$info$epoch * (as.integer(minn) -  1)
  even_indices = seq(2, length(ss), by = 2)
  ss[even_indices] = ss[even_indices] + 1
  return(ss)
}
