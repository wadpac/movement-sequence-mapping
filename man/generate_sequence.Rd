% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_sequence.R
\name{generate_sequence}
\alias{generate_sequence}
\title{generate_sequence}
\usage{
generate_sequence(
  counts,
  timeline,
  file_name,
  epochsize,
  minwear,
  zerocounts,
  cutpoints,
  bts,
  tz,
  bout_algorithm = "V2"
)
}
\arguments{
\item{counts}{A vector containing the (aggregated/resampled) accelerometer counts from the data file}

\item{timeline}{A vector containing the time stamps corresponding to the accelerometer counts}

\item{file_name}{A string specifying the file name including file extension}

\item{epochsize}{An integer that defines the epoch length in seconds}

\item{minwear}{An integer that defines the minimum wear time in minutes that constitutes a valid day}

\item{zerocounts}{An integer that defines the non-wear time as number of consecutive epochs containing zero counts}

\item{cutpoints}{A vector of integers that defines the cut-point threshold values in counts per minute (cpm). For example if value = c(0, 100, 2296, 4012) the corresponding thresholds are: SB 0 - 100, LPA 100 - 2296, MPA 2296 - 4012, VPA >= 4012}

\item{bts}{A vector of integers that defines the bout duration in minutes (Default : c(0, 5, 10, 30)). Note: The function only considers bouts < 60 minutes.}

\item{tz}{A string specifying the time zone to be used for the conversion (see strptime)}

\item{bout_algorithm}{One of c("V1", "V2") defining the tolerance function used for the bout calculation, where "V1" looks whether a pair of segments (based on intensity) is within a tolerance of 10 \%; "V2" looks at the whole of segments to identify breaks in bouts within this tolerance (Default : "V2")}
}
\value{
result A list of \item{days}{An integer indicating the number of (valid) days} \item{recording_level_mapping}{A vector consisting of the sequence map for all (valid) days in the recording} \item{day_level_mapping}{A matrix in which each row represents a sequence map of one day} \item{recording_level_mapping_length}{An integer representing the length of the recording level sequence map} \item{day_level_mapping_length}{A vector of integers representing the lengths of the day level sequence maps}
}
\description{
'generate_sequence' generates sequence maps structured per recording and per day
}
\details{
This function applies the cut-point classes for each epoch in a data file. Then the lengths of the consecutive epochs in the same cut-point class and their corresponding values are determined. After the non-wear time (zerocounts) and invalid days (minwear) are filtered out and the tolerance is incorporated (using function 'detect_bouts'), the detected segments (bout values and lengths) are labeled with the corresponding symbols (using function 'add_symbols') and the recording level (using the function 'structure_per_recording') and day level (using the function 'structure_per_day') sequence maps are generated.
}
