#' long.barcode
#'
#' @param short_barcoding ...
#' @param sub_barcode ...
#' @return short_barcoding
#' @export

# get the new sequciencing of every day
long.barcode<-function(short_barcoding,sub_barcode){
  tt = rep(NA,10080)
  if (length(sub_barcode) > 10080){
    tt = sub_barcode[1:10080]
  } else {
    tt[1:length(sub_barcode)] = sub_barcode
  }
  short_barcoding = rbind(short_barcoding,tt)
  return(short_barcoding)
}
