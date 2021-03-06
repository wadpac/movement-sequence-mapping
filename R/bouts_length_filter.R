#' bouts_length_filter
#'
#' @param counts ...
#' @param timeline ...
#' @param file_name ...
#' @param epochsize ...
#' @param minwear ...
#' @param zerocounts ...
#' @param cutpoints ...
#' @param bts ...
#' @param tz ...
#' @param tolerance_function ...
#' @return result
#' @export


# New sequencing
bouts_length_filter <- function(counts, timeline, file_name, epochsize,
    minwear, zerocounts, cutpoints, bts, tz,
    tolerance_function="V1") {
  recording_date = as.Date(timeline, tz = tz)
  ucf = unique(recording_date)
  nucf <- length(ucf) # number of unique days in aggregated values
  Nepoch_per_minute = 60 / epochsize # frequency expressed as number of epochs per minute
  cutpoints = cutpoints / Nepoch_per_minute # cutpoints for the specified (epoch length of aggregated values) epoch length
  # Initialize variables:
  days = 0
  long_barcoding = short_barcoding = NULL
  long_barcoding_length = short_barcoding_length = NULL
  ucfs = NULL
  for (j in 1:nucf) { # loop over the days
      counts.subset <- counts[recording_date == as.Date(ucf[j])]
      # print("---")
      # print(counts.subset[(61*4):(140*4)])
      # print(cutpoints)
    # Wear / Non-wear detection:
    # !!! We are not removing non-wear from the data at this point; non-wear data is labeled as -999 !!!
    countsNonWear <- labelNonWear(counts.subset, zerocounts, Nepoch_per_minute) #non-wear time is => 60 minutes (= default for zerocounts) consecutive zeros
    #z <- findInterval(counts.subset, vec = cutpoints, all.inside = F)
    #bouts <- rle(z)
    z <- findInterval(countsNonWear, vec = c(-999, cutpoints), all.inside = F)
    bouts <- rle(z - 1)
    
    # bouts has two elements:
    # - length: bout length in epochs
    # - value: bout class (value 0 is non-wear time!)
    
    # print(length(which(bouts$values == 0))) # simulated data shows 1 bout as expected (61 minutes)
    # print(bouts$lengths[which(bouts$values == 0)]) # simulated data shows 244 epochs as expected (4 epochs x 61 minutes)
    #weartime = sum(bouts$lengths)
    #noweartime = sum(bouts$lengths[bouts$values == 0])
    weartime = length(countsNonWear)
    noweartime = sum(bouts$lengths[bouts$length >= zerocounts * Nepoch_per_minute
      & bouts$values == 0]) #non-wear time is => 60 minutes (= default for zerocounts) consecutive sedentary behavior
      #noweartime = sum(bouts$lengths[bouts$length >= zerocounts * Nepoch_per_minute
       #& bouts$values == 1]) #non-wear time is => 60 minutes (= default for zerocounts) consecutive sedentary behavior
    weartime = weartime - noweartime

    # Only consider bouts that last less than 60 minutes:
    ## If we do this, then no classification for non-wear in the sequence maps
    #bt_values = bouts$values[bouts$lengths < 60 * Nepoch_per_minute]
    #bt_lengths = bouts$lengths[bouts$lengths < 60 * Nepoch_per_minute]
    # Consider all bouts that last less than 60 minutes, but also include non-wear time bouts independent of their length
    bt_values = bouts$values[(bouts$lengths < 60 * Nepoch_per_minute) | (bouts$values == 0)]
    bt_lengths = bouts$lengths[(bouts$lengths < 60 * Nepoch_per_minute) | (bouts$values == 0)]

    if (weartime >= (minwear * Nepoch_per_minute)) { # valid day = 480 min (= default for minwear)
      days = days + 1
      ucfs = c(ucfs, as.Date(ucf[j]))

      # Make this more flexible, according to input bts & add extra variable for timethresholds?
      # tolerance classes MVPA (class 4), LPA (class 3), SB/inactivity (class 2): time thresholds 5, 10, 30, 60 minutes
      bb <- tolerated_bouts(bt_values, bt_lengths, tolerance_class = c(4, 3, 2),
            timethresholds = c(5, 10, 30, 60), Nepoch_per_minute, tolerance_function=tolerance_function)

      barcode_per_day = generate_barcode(bb$values,
        bb$lengths, Nepoch_per_minute, bts)
      sub_length <- bb$lengths
      # short_barcoding is to put the barcode_per_day from all days next to each other in columns
      short_barcoding = shorting.barcode(short_barcoding, barcode_per_day) #
      # short_barcoding is to put the barcode_per_day from all days after each other in one long vector
      long_barcoding = c(long_barcoding,barcode_per_day)
      # keep track of lengths corresponding to all bar-codes:
      long_barcoding_length = c(long_barcoding_length, sub_length)
      # keep track of lengths corresponding to all bar-codes:
      short_barcoding_length = shorting.barcode(short_barcoding_length, sub_length) #
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
  result <- list(days = days, long_barcoding = long_barcoding,
    short_barcoding = short_barcoding, long_barcoding_length = long_barcoding_length,
    short_barcoding_length = short_barcoding_length)
  return(result)
}
