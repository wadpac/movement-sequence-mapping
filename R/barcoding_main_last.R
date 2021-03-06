#' barcoding_main_last
#'
#' @param path_input ...
#' @param tz ...
#' @param fileid ...
#' @param epochsize ...
#' @param which ...
#' @param rescale.epoch ...
#' @param minwear ...
#' @param zerocounts ...
#' @param cutpoints ...
#' @param bts ...
#' @param collapse.by ...
#' @param keep.error ...
#' @param tolerance_function ...
#' @return results
#' @export

barcoding_main_last <- function(path_input, tz = "Europe/London",
                                fileid = "test", epochsize = 15,  which = "y", 
                                rescale.epoch = 15, minwear = 480, zerocounts = 60, 
                                cutpoints = c(0, 100, 2296, 4012), bts = c(0, 5, 10, 30), 
                                collapse.by = "%Y-%m-%d", keep.error = FALSE, tolerance_function="V1") {
  file_list = dir(path_input, full.names = F)
  n = length(file_list)
  days = NULL
  short_barcoding = long_barcoding = NULL
  short_barcoding_length = long_barcoding_length = NULL
  for(i in 1:n) { # loop over files
    cat(paste0("\n", i, ": "))
    file_name = gsub(".csv", "", file_list[i], ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    file_name = gsub("CSV", "", file_name, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    cat("read data...")
    # Why is timezone hardcoded to Europe/London? The data used was from the Denmark? AL: tz as parameter in function, default = Europe/London, can be adapted
    temp <-  readdata(file_list[i], path_input, fileid, tz = tz, sparse = FALSE, fault = 32767)
    #HIER KAN EEN IF STATEMENT: if rescale.epoch %% epoch == 0 then aggAccFile, else aggAccFile_self?
    #test<- aggAccFile_self(temp, by =epoch, keep.error = FALSE,which="y")
    test <- aggAccFile_self(temp, by = epochsize, keep.error = keep.error, which = "y")
    # test is now a list with two vectors:
    # - outcome: numeric count values from the data file
    # - ts_agg: POSIXct timestamps
    cat("process data...")
    calculation <- bouts_length_filter(counts = test$outcome, timeline = test$ts_agg, 
                                       file_list[i], epochsize, minwear, 
                                       zerocounts, cutpoints, bts, tz = tz,
                                       tolerance_function = tolerance_function)
    short_barcoding = rbind(short_barcoding, calculation$short_barcoding)
    long_barcoding = long.barcode(long_barcoding, calculation$long_barcoding)
    short_barcoding_length = rbind(short_barcoding_length, calculation$short_barcoding_length)
    long_barcoding_length = long.barcode(long_barcoding_length, calculation$long_barcoding_length)
    days = c(days, calculation$days)
    cat("done")
  }
  results = list(short_sequence = short_barcoding, long_sequence = long_barcoding)
  return(results)
}
