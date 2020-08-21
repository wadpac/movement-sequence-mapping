#' shorting.barcode
#'
#' @param short_barcoding ...
#' @param barcode_per_day ...
#' @return short_barcoding
#' @export

# get the new sequciencing of every day
shorting.barcode<-function(short_barcoding, barcode_per_day){
  # the trick.we assume the new sequencing is less than 1440.
  # Actually, it shoud be as 1440*f in worst case. However, 1440 is far far enough
  tt=rep(NA,1440)
  if (length(barcode_per_day) > 1440) {
    tt=barcode_per_day[1:1440]
  } else {
    tt[1:length(barcode_per_day)] = barcode_per_day
  }
  if (is.null(short_barcoding) == FALSE) {
    short_barcoding = rbind(short_barcoding, tt)
  } else {
    short_barcoding = tt
  }
  return(short_barcoding)
}
