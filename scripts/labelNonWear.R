# Label the bouts with value (0) with length (> 60 min of consecutive zero counts) in the data; without tolerance
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
