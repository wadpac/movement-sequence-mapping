#' long.map
#'
#' @definition 'long.map' binds the short maps together in a long map
#'
#' @param short_mapping ...
#' @param sub_map ...
#' @return short_mapping
#' @export

# get the new sequencing of every day
long.map <- function(short_mapping, sub_map){
  tt = rep(NA, 10080)
  if (length(sub_map) > 10080){
    tt = sub_map[1:10080]
  } else {
    tt[1:length(sub_map)] = sub_map
  }
  short_mapping = rbind(short_mapping, tt)
  return(short_mapping)
}

