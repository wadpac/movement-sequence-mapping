#' generate_barcode
#'
#' @param bouts_values ...
#' @param bouts_lengths ...
#' @param f ...
#' @param bts ...
#' @return barcodes
#' @export

# rewrite teh sequencing
generate_barcode <- function(bouts_values, bouts_lengths, f, bts) {
  BLcopy = bouts_lengths
  btss = bts * f # probably better to simply ask for btss as input
  # change lengths to 1 of 4 classes:
  bouts_lengths[which(BLcopy >= btss[1] & BLcopy < btss[2])] = 1
  bouts_lengths[which(BLcopy >= btss[2] & BLcopy < btss[3])] = 2
  bouts_lengths[which(BLcopy >= btss[3] & BLcopy < btss[4])] = 3
  bouts_lengths[which(BLcopy >= btss[4])] = 4
  # generate barcodes
  df <- data.frame(bvalue = bouts_values,
                   blength = bouts_lengths,
                   code = rep(NA, length(bouts_lengths)))
  
  # Non-wear
  df$code[df$bvalue == 0 & df$blength == 1] <- 0
  df$code[df$bvalue == 0 & df$blength == 2] <- 0
  df$code[df$bvalue == 0 & df$blength == 3] <- 0
  df$code[df$bvalue == 0 & df$blength == 4] <- 0
  # SB
  df$code[df$bvalue == 1 & df$blength == 1] <- 3 # 0 - 5 minutes
  df$code[df$bvalue == 1 & df$blength == 2] <- 3 # 5 - 10 minutes
  df$code[df$bvalue == 1 & df$blength == 3] <- 2 # 10 - 30 minutes
  df$code[df$bvalue == 1 & df$blength == 4] <- 1 # > 30 minutes
  # light
  df$code[df$bvalue == 2 & df$blength == 1] <- 4 # note that order of symbols changes relative to SB
  df$code[df$bvalue == 2 & df$blength == 2] <- 4
  df$code[df$bvalue == 2 & df$blength == 3] <- 5
  df$code[df$bvalue == 2 & df$blength == 4] <- 6
  # moderate
  df$code[df$bvalue == 3 & df$blength == 1] <- 7
  df$code[df$bvalue == 3 & df$blength == 2] <- 8
  df$code[df$bvalue == 3 & df$blength == 3] <- 9
  df$code[df$bvalue == 3 & df$blength == 4] <- 9
  # vigorous
  df$code[df$bvalue == 4 & df$blength == 1] <- 10
  df$code[df$bvalue == 4 & df$blength == 2] <- 11
  df$code[df$bvalue == 4 & df$blength == 3] <- 12
  df$code[df$bvalue == 4 & df$blength == 4] <- 12
  
  # df$code[df$bvalue == 5 & df$blength == 1] <- 15
  # df$code[df$bvalue == 5 & df$blength == 2] <- 16
  # df$code[df$bvalue == 5 & df$blength == 3] <- 17
  # df$code[df$bvalue == 5 & df$blength == 4] <- 18
  
  # Note: df$code is directly used as barcode, but this is not described in the paper.
  # We only know this from  talking to first author:
  barcodes = df$code
  print(barcodes)
  rm(df)
  cnt = 1
  barcodes_tmp = barcodes
  
  if (length(which(is.na(barcodes_tmp) == TRUE)) > 0) barcodes_tmp[is.na(barcodes_tmp)] = -99
  while(cnt > 0 & length(barcodes) > 1){
    
    if (barcodes_tmp[cnt] == barcodes_tmp[cnt + 1]) {
        barcodes = barcodes[-cnt] # remove succeeding duplicate
        barcodes_tmp = barcodes_tmp[-cnt] # remove succeeding duplicate
    }
    cnt = cnt + 1
    if (cnt + 1 > length(barcodes)) {
        break()
    }
  }
  
  # Old code provided by Xinhui (updated with new object names in this function)
  # does not match the description in the paper or the description by first author:
  # df <- df[order(df$code),] # Comment by me: Why order? That would remove all sequence knowledge?
  # N = length(bouts_values)
  # bouts_lengths_intervals <- findInterval(bouts_lengths, btss, all.inside = F)
  # barcodes= rep(0,N)
  # for (k in 1:N) { 
  #   # Comment by me: Indicies either seems to be multiple values or empty?
  #   indices = which(df$bvalue == bouts_values[k] & df$blength == bouts_lengths_intervals[k])
  #   barcodes[k]=df$code[indices]
  # }
  
  return(barcodes)
}
