#' structure_per_day
#'
#' @description 'structure_per_day' organizes the sequence map as a new row per day
#'
#' @param day_level_mapping A vector of integers specifying a movement sequence map
#' @param map_per_day A vector of integers specifying a movement sequence map of one day
#'
#' @return day_level_mapping A matrix of integers with rows specifying a movement sequence map of one day
#' @export

structure_per_day <- function(day_level_mapping, map_per_day){
  # the trick.we assume the new sequencing is less than 1440.
  # Actually, it should be as 1440*f in worst case. However, 1440 is far far enough
  tt = rep(NA, 1440)
  if (length(map_per_day) > 1440) {
    tt = map_per_day[1:1440]
  } else {
    tt[1:length(map_per_day)] = map_per_day
  }
  if (is.null(day_level_mapping) == FALSE) {
    day_level_mapping = rbind(day_level_mapping, tt)
  } else {
    day_level_mapping = tt
  }
  return(day_level_mapping)
}
