% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resample.R
\name{resample}
\alias{resample}
\title{resample}
\usage{
resample(object, by, which = "counts", x = NULL, keep.error = FALSE)
}
\arguments{
\item{object}{An object of class accfile}

\item{by}{An integer that defines the epoch length in seconds by which counts or steps are aggregated. Note: it cannot be less than the accelerometer epoch length itself (object$info$epoch)}

\item{which}{A string one of c("counts", "steps") for gt1m files or one of c("x", "y", "z", "steps") for gt3x files (Default : "counts")}

\item{x}{Optional argument. If NULL, this is set to "counts" (Default : NULL)}

\item{keep.error}{A boolean that flags whether errors should be omitted (Default : FALSE)}
}
\value{
out A list of \item{outcome}{Aggregated values} \item{ts_agg}{Time stamping}
}
\description{
'resample' aggregates the count values, which can be interpreted as a resampling step
}
