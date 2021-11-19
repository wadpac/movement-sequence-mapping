#' aggAccFile_self
#'
#' @description 'aggAccFile_self' aggregates the count values
#'
#' @param object An object of class accfile
#' @param by An integer that defines the epoch length in seconds by which counts or steps are aggregated. Note: it cannot be less that the accelerometer epoch length itself (object$info$epoch)
#' @param which A string one of c("counts", "steps") for gt1m files or one of c("x", "y", "z", "steps") for gt3x files (Default : "counts")
#' @param x Optional argument. If NULL, this is set to "counts" (Default : NULL)
#' @param keep.error A boolean that flags wheter errors should be omitted (Default : FALSE)
#'
#' @return out A list of \item{outcome}{Aggregated values} \item{ts_agg}{Time stamping}
#' @export

# With this function, could transfer the epoch 2 as epoch 15
aggAccFile_self <- function (object, by, which = "counts", x = NULL, keep.error = FALSE) {
  info <- object$info
  sparse <- attr(object, "sparse")
  if (info$epoch > by) 
    stop(paste("Epoch is longer than ", by, sep = ""))
  f <- by / info$epoch
  
  if (sparse) {
    Data <- as.data.frame(as.matrix(object$df))
    colnames(Data) <- attr(object, "labels")
  } else {
    Data <- object$df
  }
  if (is.null(x)) {
    nn <- intersect(c("x", "y", "z", "counts", "steps"), colnames(Data))
    if ("gt1m" %in% class(object)) {
      if (!which %in% nn) 
        stop(cat("Argument 'which' must be one of", nn, "\n"))
      x <- Data[, which]
      err <- paste("error", substr(which, 1, 1), sep = "_")
      err <- Data[, err]
    } else if ("gt3x" %in% class(object)) {
      if (!which %in% nn) 
        stop(cat("Argument 'which' must be one of", nn, "\n"))
      x <- Data[, which]
      err <- paste("error", substr(which, 1, 1), sep = "_")
      err <- Data[, err]
    }
    x <- pawacc::handleError(x, err, code = "all", na = TRUE, keep.error = keep.error)
  }
  minn <- seq(1, info$nobs, by = f)
  maxn <- seq(f, info$nobs, by = f)
  if (info$nobs%%f != 0) 
    maxn <- c(maxn, info$nobs)
  if (length(minn) != length(maxn)) 
    minn <- minn[1:min(length(minn), length(maxn))]
  maxn <- maxn[1:min(length(minn), length(maxn))]
  indexn = (minn[-1] + maxn[-length(maxn)])/2
  d = length(indexn)
  xx = NULL
  for(i in 1:(d/2)) {
    xx = c(xx, sum(x[minn[2 * i - 1]:(indexn[2 * i - 1] - 1)], na.rm = TRUE) + x[indexn[2 * i - 1]]/2)
    xx = c(xx, sum(x[indexn[2 * i - 1]:(maxn[2 * i])], na.rm = TRUE) - x[indexn[2 * i - 1]]/2)
  }
  
  #fun.do <- function(a, b, x) sum(x[a:b], na.rm = TRUE)
  #x <- mapply(fun.do, a = minn, b = maxn, MoreArgs = list(x = x))
  x = xx

  if (sparse) {
    x <- SparseM::as.matrix.csr(x)
  }
  TimeStamp <- tsFromEpoch_self(object, minn)
  out <- list(outcome = x, ts_agg = TimeStamp)
  attr(out, "sparse") <- sparse
  class(out) <- "accfile_agg"
  return(out)
}
