#' structure_per_recording
#'
#' @description 'structure_per_recording' reformats day level sequence maps a into recording level sequence map
#' @param day_level_mapping A vector of integers specifying a movement sequence map
#' @param sub_map A vector of integers specifying the movement sequence map of one day
#'
#' @return day_level_mapping A vector of integers specifying the movement sequence map of all (valid) days
#' @export

# get the new sequencing of every day
structure_per_recording <- function(day_level_mapping, sub_map){
  tt = rep(NA, 10080)
  if (length(sub_map) > 10080){
    tt = sub_map[1:10080]
  } else {
    tt[1:length(sub_map)] = sub_map
  }
  day_level_mapping = rbind(day_level_mapping, tt)
  return(day_level_mapping)
}

