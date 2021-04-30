#' bouts_length_filter
#'
#' @description 'bouts_length_filter' generates the short and long sequence maps
#'
#' @details This function applies the cut-point classes for each epoch in a data file. Then the lengths of the consecutive epochs in the same cut-point class and their corresponding values are determined. After the non-wear time (zerocounts) and invalid days (minwear) are filtered out and the tolerance is incorporated (using function 'tolerated_bouts'), the sequence maps are calculated (using function 'generate_sequence_map') and the long and short sequence maps are generated.
#'
#' @param counts A vector containing the (aggregated) accelerometer counts from the data file
#' @param timeline A vector containing the time stamps corresponding to the accelerometer counts
#' @param file_name A string specifying the file name including file extension
#' @param epochsize An integer that defines the epoch length in seconds
#' @param minwear An integer that defines the minimum wear time in minutes that constitutes a valid day
#' @param zerocounts An integer that defines the non-wear time as number of consecutive epochs containing zero counts
#' @param cutpoints A vector of integers that defines the cut-point threshold values in counts per minute (cpm). For example if value = c(0, 100, 2296, 4012) the corresponding thresholds are: SB 0 - 100, LPA 100 - 2296, MPA 2296 - 4012, VPA >= 4012
#' @param bts A vector of integers that defines the bout durations in minutes (Default : c(0, 5, 10, 30)). Note: The function only considers bouts < 60 minutes.
#' @param tz A string specifying the time zone to be used for the conversion (see strptime)
#' @param tolerance_function One of c("V1", "V2") defining the tolerance function used for the bout calculation, where "V1" looks whether a pair of segments (based on intensity) is within a tolerance of 10%; "V2" looks at the whole of segments to identify breaks in bouts within this tolerance (Default : "V2")
#'
#' @return result A list of \item{days}{An integer indicating the number of days} \item{long_mapping}{A vector consisting of the sequence map for all days} \item{short_mapping}{A matrix in which each column represents a sequence map of one day} \item{long_mapping_length}{An integer representing the length of the long sequence map} \item{short_mapping_length}{A vector of integers representing the lengths of the short sequence maps}
#' @export


# New sequencing
bouts_length_filter <- function(counts, timeline, file_name, epochsize,
    minwear, zerocounts, cutpoints, bts, tz,
    tolerance_function="V2") {
  recording_date = as.Date(timeline, tz = tz)
  ucf = unique(recording_date)
  nucf <- length(ucf) # number of unique days in aggregated values
  Nepoch_per_minute = 60 / epochsize # frequency expressed as number of epochs per minute
  cutpoints = cutpoints / Nepoch_per_minute # cutpoints for the specified (epoch length of aggregated values) epoch length
  # Initialize variables:
  days = 0
  long_mapping = short_mapping = NULL
  long_mapping_length = short_mapping_length = NULL
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

      map_per_day = generate_sequence_map(bb$values,
        bb$lengths, Nepoch_per_minute, bts)
      sub_length <- bb$lengths
      # short_mapping is to put the map_per_day from all days next to each other in rows
      short_mapping = shorting.map(short_mapping, map_per_day) #
      # short_mapping is to put the map_per_day from all days after each other in one long vector
      long_mapping = c(long_mapping, map_per_day)
      # keep track of lengths corresponding to all maps:
      long_mapping_length = c(long_mapping_length, sub_length)
      # keep track of lengths corresponding to all maps:
      short_mapping_length = shorting.map(short_mapping_length, sub_length) #
    }
    if (length(long_mapping) == 0) {
      long_mapping = 0
      long_mapping_length = 0
    }
  }

  if (length(ucfs) > 0) {
    row.names(short_mapping) = paste(file_name, ucfs, sep = "_")
    row.names(short_mapping_length) = paste(file_name, ucfs, sep = "_")
  }
  result <- list(days = days, long_mapping = long_mapping,
    short_mapping = short_mapping, long_mapping_length = long_mapping_length,
    short_mapping_length = short_mapping_length)
  return(result)
}
