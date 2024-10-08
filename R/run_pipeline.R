#' run_pipeline
#'
#' @description 'run_pipeline' returns sequence maps structured on day and recording level
#'
#' @details This high-level function ties all functions together: 1) reads in the accelerometer data from .csv files using function 'read_file', 2) aggregates/resamples the accelerometer data using function 'resample', and 3) generates the sequence maps structured per day and per recording using the function 'generate_sequence'.
#'
#' @param path_input A string specifying the path to the folder containing the accelerometry (ActiGraph) .csv files
#' @param tz A string specifying the time zone to be used for the conversion (see strptime) (Default : "Europe/London")
#' @param fileid A string specifying the label for the file identifier (Default : "test")
#' @param epochsize An integer that defines the epoch length in seconds (Default : 15)
#' @param which A string one of c("counts", "steps") for gt1m files or one of c("x", "y", "z", "steps") for gt3x files (Default : "y")
#' @param rescale.epoch An integer that defines the epoch length in seconds to which the accelerometry data needs to be resampled (e.g. 60 to aggregate the data per minute) (Default : 15). Note: it cannot be less than epochsize.
#' @param minwear An integer that defines the minimum wear time in minutes that constitutes a valid day (Default : 480)
#' @param zerocounts An integer that defines the non-wear time as number of consecutive epochs containing zero counts (Default : 60)
#' @param cutpoints A vector of integers that defines the cut-point threshold values in counts per minute (cpm). For example if value = c(0, 100, 2296, 4012) the corresponding thresholds are: SB 0 - 100, LPA 100 - 2296, MPA 2296 - 4012, VPA >= 4012 (Default : c(0, 100, 2296, 4012))
#' @param bts A vector of integers that defines the bout duration in minutes (Default : c(0, 5, 10, 30))
#' @param collapse.by A string specifying the format of the date in the accelerometer data (Default : "%Y-%m-%d")
#' @param windows A vector of length 2 specifying the time window in a day considered for the sequence maps, e.g. only generate a sequence map between the start time 8 AM and end time 8 PM by defining: c("08:00:00", "20:00:00") (Default is no window selection: c())
#' @param keep.error A boolean that flags whether errors should be omitted (Default : FALSE)
#' @param bout_algorithm One of c("V1", "V2") defining the tolerance function used for the bout calculation, where "V1" looks whether a pair of segments (based on intensity) is within a tolerance of 10 % using the function 'xinhui_bout_algorithm'; "V2" looks at the whole of segments to identify breaks in bouts within this tolerance (Default : "V2")
#'
#' @return results A list of \item{sequence_day_level}{Contains a matrix of day level sequence maps of the data measured for each subject file} \item{sequence_recording_level}{Contains one sequence map for all data measured for each subject file}
#' @export

run_pipeline <- function(path_input, tz = "Europe/London",
                                fileid = "test", epochsize = 15,  which = "y", 
                                rescale.epoch = 15, minwear = 480, zerocounts = 60, 
                                cutpoints = c(0, 100, 2296, 4012), bts = c(0, 5, 10, 30), 
                                collapse.by = "%Y-%m-%d", windows = c(), keep.error = FALSE, bout_algorithm = "V2") {
  file_list = dir(path_input, full.names = F)
  n = length(file_list)
  days = NULL
  day_level_mapping = recording_level_mapping = NULL
  day_level_mapping_length = recording_level_mapping_length = NULL
  for(i in 1:n) { # loop over files
    cat(paste0("\n", i, ": "))
    file_name = gsub(".csv", "", file_list[i], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    file_name = gsub("CSV", "", file_name, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    cat("read data...")
    temp <-  read_file(file_list[i], path_input, fileid, tz = tz, sparse = FALSE, fault = 32767)
    #HIER KAN EEN IF STATEMENT  if (rescale.epoch %% epochsize == 0) { test <- pawacc::aggAccFile(temp, by = rescale.epoch, which = which, keep.error = keep.error) } else {    
    
    test <- resample(temp, by = rescale.epoch, keep.error = keep.error, which = which)
    # test is now a list with two vectors:
    # - outcome: numeric count values from the data file
    # - ts_agg: POSIXct timestamps
    
    # If there is a set time window to be considered as a day, select only these data
    if(length(windows) == 2){
      test <- get_windows(windows, test)
    }

    cat("process data...")
    calculation <- generate_sequence(counts = test$outcome, timeline = test$ts_agg, 
                                       file_list[i], epochsize, minwear, 
                                       zerocounts, cutpoints, bts, tz = tz,
                                       bout_algorithm = bout_algorithm)
    if(calculation$days > 0){
      # Now, attempt to combine the data frames
      if(!is.matrix(calculation$day_level_mapping)){
        calculation$day_level_mapping <- as.matrix(calculation$day_level_mapping)
      }
      day_level_mapping = rbind(day_level_mapping, calculation$day_level_mapping)
      if(!is.matrix(calculation$day_level_mapping_length)){
        calculation$day_level_mapping_length <- as.matrix(calculation$day_level_mapping_length)
      }
      day_level_mapping_length = rbind(day_level_mapping_length, calculation$day_level_mapping_length)
      recording_level_mapping = structure_per_recording(recording_level_mapping, calculation$recording_level_mapping)
      recording_level_mapping_length = structure_per_recording(recording_level_mapping_length, calculation$recording_level_mapping_length)
      days = c(days, calculation$days)
    } else{
      cat("no sequence generated for:")
      cat(file_list[i])
    }
    
    cat("done")
  }
  results = list(sequence_day_level = day_level_mapping, sequence_recording_level = recording_level_mapping)
  return(results)
}
