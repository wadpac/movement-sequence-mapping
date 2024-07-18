#' read_file
#'
#' @description 'read_file' reads in data from .csv accelerometry files stored in one directory
#'
#' @param file A string specifying the file name including file extension
#' @param path_input A string specifying the path to the folder containing the accelerometry files
#' @param fileid A string specifying the label for the file identifier
#' @param tz A string specifying the time zone to be used for the conversion (see strptime)
#' @param sparse A logical flag: should data be stored in sparse format? (Default : FALSE)
#' @param fault An integer value that indicates voltage signal saturation (temporarily used for both accelerometer counts and steps) (Default : 32767)
#'
#' @return object An object of two classes: accfile and additional device-specific class (i.e. gt3x). An object of class accfile is a list containing the following components \item{df}{A data.frame object with accelerometer values in columns counts and steps (if present), and coded error for each accelerometer data column. See errorChk for error codes. If sparse = TRUE, all variables of the data frame df are returned as vectors of a matrix in sparse format (see as.matrix.csr for details).} \item{info}{A data.frame object with file identifier (fileid), device serial number (serial), number of recorded measurements (nobs), epoch (epoch), accelerometer mode (mode), start date and time (ts_start), time zone (tz), battery voltage (voltage), download date and time (ts_dl).} \item{error_summary}{A list object with file identifier (fileid), summary tables of error codes for each accelerometer data column, error code for date (date), and logical flag for odd number of measurements (odd_number).}
#' @export

read_file <- function (file, path_input, fileid, tz, sparse = FALSE, fault = 32767) {
  filename <- paste(path_input, file, sep = "/")
  fileConnection <- file(filename, open = "r")
  if (isOpen(fileConnection, "r")) {
    seek(fileConnection, 0, rw = "r")
    Lines <- readLines(fileConnection)
    close(fileConnection)
  }
  Mode <- grep("Mode", Lines)
  Mode <- as.numeric(strsplit(Lines[Mode], "= ")[[1]][2])
  epoch <- grep("Epoch", Lines)
  epoch <- strsplit(Lines[epoch], ") ")[[1]][2]
  epoch <- as.numeric(strsplit(epoch, ":")[[1]])
  epoch <- epoch[1] * 3600 + epoch[2] * 60 + epoch[3]
  serial <- grep("Serial", Lines)
  serial <- strsplit(Lines[serial], ":")[[1]][2]
  serial <- sub("^\\s+", "", serial)
  Voltage <- grep("Voltage", Lines)
  #Voltage <- as.numeric(strsplit(strsplit(Lines[Voltage], "Mode")[[1]][1], ":")[[1]][2])
  formatDate <- strsplit(strsplit(Lines[1], "date format")[[1]][2], "at")[[1]][1]
  
  sel <- grep("Download Time", Lines)
  downTime <- gsub("Download Time ", "", Lines[sel])
  downTime <- gsub("[[:blank:]]", "", downTime)
  sel <- grep("Download Date ", Lines)
  downDate <- gsub("Download Date ", "", Lines[sel])
  downDate <- gsub("[[:blank:]]", "", downDate)
  downDate <- strsplit(downDate, "-")[[1]]
  
  downDate <- pawacc::infoDate(downDate, fileid)$date
  TS_dl <- paste(downDate, downTime, sep = " ")
  TS_dl <- as.POSIXlt(TS_dl, tz = tz)
  sel <- grep("Start Time", Lines)
  startTime <- gsub("Start Time ", "", Lines[sel])
  startTime <- gsub("[[:blank:]]", "", startTime)
  sel <- grep("Start Date ", Lines)
  startDate <- gsub("Start Date ", "", Lines[sel])
  startDate <- gsub("[[:blank:]]", "", startDate)
  startDate <- strsplit(startDate, "-")[[1]]
  if(length(startDate) < 3){
    startDate <- strsplit(startDate, "/")[[1]]
  }
  startDate <- pawacc::infoDate(startDate, fileid)
  TS_orig <- paste(startDate$date, startTime, sep = " ")
  TS_orig <- as.POSIXlt(TS_orig, tz = tz)

  tz <- format(TS_orig, "%Z")
  if (!(tz %in% c("GMT", "BST"))) {
    warning(paste("GMT/BST not determined for accelerometer ", fileid))
    tz <- "NA"
  }
  startL <- grep("-----", Lines)[2] + 1
  endL <- length(Lines)
  tmp <- Lines[startL:endL]
  tmp <- lapply(tmp, function(x) strsplit(gsub("[[:blank:]]+", " ", x), " ")[[1]])
  ncols <- length(strsplit(tmp[[1]], ",")[[1]])
  if (ncols > 4) 
    warning(paste("Number of accelerometer variables is", ncols))
  accData <- matrix(as.numeric(unlist(strsplit(unlist(tmp),  ","))), ncol = ncols, 
    byrow = TRUE)
  n <- nrow(accData)
  colnames(accData) <- c("y", "x", "z", "steps")[1:ncols]
  error <- NULL
  for (j in 1:ncols) {
    error <- cbind(error, pawacc::errorChk(accData[, j], fault = fault))
  }
  colnames(error) <- paste("error", substr(colnames(accData), 
                                           1, 1), sep = "_")
  summaryFile <- data.frame(fileid = fileid, serial = serial,  nobs = n, 
    epoch = epoch, mode = Mode, ts_start = TS_orig,  tz = tz, voltage = Voltage, 
    ts_dl = TS_dl, stringsAsFactors = FALSE)
  err_s <- apply(error, 2, table)
  err_s <- c(fileid = fileid, as.list(err_s), date = startDate$date_code, odd_number = NA)
  Data <- data.frame(accData, error)
  #out <- if (sparse) {
  #  list(df = as.matrix.csr(Data), info = summaryFile, error_summary = err_s)
  #}
  #else {
  object <- list(df = data.frame(Data), info = summaryFile, error_summary = err_s)
  # }
  attr(object, "sparse") <- sparse
  attr(object, "labels") <- colnames(accData)
  class(object) <- c("accfile", "gt3x")
  return(object)
}
