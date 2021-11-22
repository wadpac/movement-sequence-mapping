#' structure_per_recording
#'
#' @description 'structure_per_recording' reformats short sequence maps into long format representing a sequence map for all (valid) days
#'
#' @param short_mapping A vector of integers specifying a movement sequence map
#' @param sub_map A vector of integers specifying the movement sequence map of one day
#'
#' @return short_mapping A vector of integers specifying the movement sequence map of all (valid) days
#' @export

# get the new sequencing of every day
structure_per_recording <- function(short_mapping, sub_map){
  tt = rep(NA, 10080)
  if (length(sub_map) > 10080){
    tt = sub_map[1:10080]
  } else {
    tt[1:length(sub_map)] = sub_map
  }
  short_mapping = rbind(short_mapping, tt)
  return(short_mapping)
}

