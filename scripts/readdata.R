readdata <- function (file, path_input, fileid, tz , sparse = FALSE, 
    fault = 32767) {
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
  Voltage <- as.numeric(strsplit(strsplit(Lines[Voltage], "Mode")[[1]][1], 
    ":")[[1]][2])
  formatDate <- strsplit(strsplit(Lines[1], "date format")[[1]][2], "at")[[1]][1]
  
  sel <- grep("Download Time", Lines)
  downTime <- gsub("Download Time ", "", Lines[sel])
  downTime <- gsub("[[:blank:]]", "", downTime)
  sel <- grep("Download Date ", Lines)
  downDate <- gsub("Download Date ", "", Lines[sel])
  downDate <- gsub("[[:blank:]]", "", downDate)
  downDate <- strsplit(downDate, "-")[[1]]
  
  downDate <- infoDate(downDate, fileid)$date
  TS_dl <- paste(downDate, downTime, sep = " ")
  TS_dl <- as.POSIXlt(TS_dl, tz = tz)
  sel <- grep("Start Time", Lines)
  startTime <- gsub("Start Time ", "", Lines[sel])
  startTime <- gsub("[[:blank:]]", "", startTime)
  sel <- grep("Start Date ", Lines)
  startDate <- gsub("Start Date ", "", Lines[sel])
  startDate <- gsub("[[:blank:]]", "", startDate)
  startDate <- strsplit(startDate, "-")[[1]]
  
  startDate <- infoDate(startDate, fileid)
  TS_orig <- paste(startDate$date, startTime, sep = " ")
  TS_orig <- as.POSIXlt(TS_orig, tz = tz)

  tz <- format(TS_orig, "%Z")
  if (!(tz %in% c("GMT", "BST"))) {
    warning(paste("GMT/BST not determined for accelerometer ",  fileid))
    tz <- "NA"
  }
  startL <- grep("-----", Lines)[2] + 1
  endL <- length(Lines)
  tmp <- Lines[startL:endL]
  tmp <- lapply(tmp, function(x) strsplit(gsub("[[:blank:]]+",  " ", x), " ")[[1]])
  ncols <- length(strsplit(tmp[[1]], ",")[[1]])
  if (ncols > 4) 
    warning(paste("Number of accelerometer variables is", ncols))
  accData <- matrix(as.numeric(unlist(strsplit(unlist(tmp),  ","))), ncol = ncols, 
    byrow = TRUE)
  n <- nrow(accData)
  colnames(accData) <- c("y", "x", "z", "steps")[1:ncols]
  error <- NULL
  for (j in 1:ncols) {
    error <- cbind(error, errorChk(accData[, j], fault = fault))
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
  out <- list(df = data.frame(Data), info = summaryFile, error_summary = err_s)
  # }
  attr(out, "sparse") <- sparse
  attr(out, "labels") <- colnames(accData)
  class(out) <- c("accfile", "gt3x")
  return(out)
}
