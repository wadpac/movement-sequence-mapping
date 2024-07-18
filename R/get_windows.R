#' get_windows
#'
#' @description 'get_windows' filters the read in data based on whether the time stamp (ts_agg) is between specified start and end times
#'
#' @param windows A character vector of length 2 specifying the start and end times in "HH:MM:SS" format.
#' @param test A data.frame object that contains the elements 'ts_agg' and 'outcome'.
#'
#' @return A data.frame object where the time stamp component of 'ts_agg' is between the specified start and end times and only the filtered elements 'outcome' and 'ts_agg' are returned.
#' @export

get_windows <- function(windows, test){
  
  tmp <- test
  # Extract the time portion from the datetime
  time <- format(tmp$ts_agg, format = "%H:%M:%S")
  
  # Filter rows where the time is between start_time and end_time
  index <- which(time >= windows[1] & time < windows[2])
  tmp$outcome <- tmp$outcome[index]
  tmp$ts_agg <- tmp$ts_agg[index]
  
  return(tmp)
}