# new sequencing
bouts_length_filter <- function(counts, timeline, file_name, epochsize, validdays, minwear, zerocounts, cutpoints, bts, tz) {
  recording_date = as.Date(timeline, tz = tz)
  ucf = unique(recording_date)
  nucf <- length(ucf) #number of unique days in aggregated values
  Nepoch_per_minute = 60 / epochsize # frequency expressed as number of epochs per minute
  cutpoints = cutpoints / Nepoch_per_minute #cutpoints for the specified (epoch length of aggregated values) epoch length
  # Initialize variables:
  days = 0
  long_barcoding = short_barcoding = NULL
  long_barcoding_length = short_barcoding_length = NULL
  ucfs = NULL
  
  for (j in 1:nucf) { # loop over the days
    counts.subset <- counts[recording_date == ucf[j]]
    z <- findInterval(counts.subset, vec = cutpoints, all.inside = F)
    bouts <- rle(z)
    # bouts has two elements:
    # - length: bout length in epochs
    # - value: bout class
    
    # Wear / Non-wear detection: 
    # !!! We are not removing non-wear from the data at this point !!!
    weartime = length(counts.subset)
    noweartime = sum(bouts$lengths[bouts$length >= zerocounts * Nepoch_per_minute &  bouts$values == 1]) # non-wear time is => 60 minutes (=default for zerocounts) consecutive sedentary behavior
    weartime = weartime - noweartime
    
    # Only consider bouts that last less than 60 minutes:
    bt_values = bouts$values[bouts$lengths < 60 * Nepoch_per_minute]
    bt_lengths = bouts$lengths[bouts$lengths < 60 * Nepoch_per_minute]
    
    if (weartime > minwear * Nepoch_per_minute) { # default valid day = 480/60 = 8 hours
      days = days + 1
      ucfs = c(ucfs, ucf[j])
      
      # MVPA (class 4): time thresholds 5, 10, 30, 60 minutes
      bb <- tor_flex_constant(bt_values, bt_lengths, 4, 30, 60, Nepoch_per_minute)
      bb <- tor_flex_constant(bb$values, bb$lengths, 4, 10, 30, Nepoch_per_minute)
      bb <- tor_flex_constant(bb$values, bb$lengths, 4, 5, 10, Nepoch_per_minute)
      # LIGHT (class 3): time thresholds 5, 10, 30, 60 minutes
      bb <- tor_flex_below(bb$values, bb$lengths, 3, 30, 60, Nepoch_per_minute)
      bb <- tor_flex_below(bb$values, bb$lengths, 3, 10, 30, Nepoch_per_minute)
      bb <- tor_flex_below(bb$values, bb$lengths, 3, 5, 10, Nepoch_per_minute)
      # INACTIVITY / SB (class 2): time thresholds 5, 10, 30, 60
      bb <- tor_flex_below(bb$values, bb$lengths, 2, 30, 60, Nepoch_per_minute)
      bb <- tor_flex_below(bb$values, bb$lengths, 2, 10, 30, Nepoch_per_minute)
      bb <- tor_flex_below(bb$values, bb$lengths, 2, 5, 10, Nepoch_per_minute)
      
      barcode_calculation = barcodeMapping::generate_barcode(bb$values, bb$lengths, Nepoch_per_minute, bts)
      sub_length <- bb$lengths
      sub_barcode <- barcode_calculation
      long_barcoding = c(long_barcoding, sub_barcode)
      short_barcoding = barcodeMapping::shorting.barcode(short_barcoding, sub_barcode)
      long_barcoding_length = c(long_barcoding_length, sub_length)
      short_barcoding_length = barcodeMapping::shorting.barcode( short_barcoding_length, sub_length)
    }
    if (length(long_barcoding) == 0) {
      long_barcoding = 0
      long_barcoding_length = 0
    }
  }
  
  if (length(ucfs) > 0) {
    row.names(short_barcoding) = paste(file_name, ucfs, sep = "_")
    row.names(short_barcoding_length) = paste(file_name, ucfs, sep = "_")
  }
  result <- list(days = days, long_barcoding = long_barcoding, short_barcoding = short_barcoding, long_barcoding_length = long_barcoding_length, short_barcoding_length = short_barcoding_length)
  return(result)
}
