tsFromEpoch_self <- function (object, minn) {
  if (any(minn <= 0)) 
    stop("x must be positive")
  if (any(minn > object$info$nobs)) 
    warning("Timestamp is outside observed time interval")
  object$info$ts_start + object$info$epoch * (as.integer(minn) -  1)
  ss = object$info$ts_start + object$info$epoch * (as.integer(minn) -  1)
  d = length(ss)
  ss[2 * (1:(d / 2))] = ss[2 * (1:(d / 2))] + 1
  return(ss)
}
