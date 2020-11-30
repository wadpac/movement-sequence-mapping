#' shorting.map
#'
#' @description 'shorting.map' binds the sequence maps for each day as new row
#'
#' @param short_mapping A vector of integers specifying a movement sequence map
#' @param map_per_day A vector of integers specifying a movement sequence map of one day
#'
#' @return short_mapping A matrix of integers with rows specifying a movement sequence map of one day
#' @export

shorting.map <- function(short_mapping, map_per_day){
  # the trick.we assume the new sequencing is less than 1440.
  # Actually, it shoud be as 1440*f in worst case. However, 1440 is far far enough
  tt = rep(NA, 1440)
  if (length(map_per_day) > 1440) {
    tt = map_per_day[1:1440]
  } else {
    tt[1:length(map_per_day)] = map_per_day
  }
  if (is.null(short_mapping) == FALSE) {
      short_mapping = rbind(short_mapping, tt)
  } else {
      short_mapping = tt
  }
  return(short_mapping)
}
