#' generate_sequence_map
#'
#' @description 'generate_sequence_map' generates the corresponding sequence map from the bout values and lengths obtained with the function 'tolerated_bouts' using the symbols as defined in ChinaPaw et al (2019)
#'
#' @param bouts_values A vector representing the cut-point classes of the corresponding bouts (e.g. 1 = SB, 2 = LPA, 3 = MPA, 4 = VPA)
#' @param bouts_lengths A vector representing the lengths (number of epochs) of the corresponding bouts
#' @param f An integer specifying the number of epochs per minute
#' @param bts A vector of integers that defines the bout durations in minutes
#'
#' @return maps A vector of integers specifying a movement sequence
#' @export

# rewrite the sequencing
generate_sequence_map <- function(bouts_values, bouts_lengths, f, bts) {0
  BLcopy = bouts_lengths
  btss = bts * f # probably better to simply ask for btss as input
  # change lengths to 1 of 4 classes:
  bouts_lengths[which(BLcopy >= btss[1] & BLcopy < btss[2])] = 1
  bouts_lengths[which(BLcopy >= btss[2] & BLcopy < btss[3])] = 2
  bouts_lengths[which(BLcopy >= btss[3] & BLcopy < btss[4])] = 3
  bouts_lengths[which(BLcopy >= btss[4])] = 4
  # generate sequence maps
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
  
  # Note: df$code is directly used as map, but this is not described in the paper.
  # We only know this from  talking to first author:
  maps = df$code
  print(maps)
  rm(df)
  cnt = 1
  maps_tmp = maps
  
  if (length(which(is.na(maps_tmp) == TRUE)) > 0) maps_tmp[is.na(maps_tmp)] = -99
  while(cnt > 0 & length(maps) > 1){
    
    if (maps_tmp[cnt] == maps_tmp[cnt + 1]) {
        maps = maps[-cnt] # remove succeeding duplicate
        maps_tmp = maps_tmp[-cnt] # remove succeeding duplicate
    }
    cnt = cnt + 1
    if (cnt + 1 > length(maps)) {
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
  
  return(maps)
}
