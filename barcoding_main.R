rm(list = ls())
# graphics.off()
# barcoding script Xinhui Wang, Vumc 02-02-2020
library(acc)
library(pawacc)
library(foreign)
library(devtools)
install_github("wadpac/barcode-mapping", force = T)
library(barcodeMapping)

#==================================
# User input needed:

# Specify root of repository with input and script folder:
#path = "/home/vincent/projects/cutpoint-approach-wang2019"
path = "/Users/annelinde/Documents/PROGRAMMING/cutpoint-approach-wang2019"
# Note: Xinhui's code expects us to use this as our
# working directory for data and scripts

minwear = 480 # Minimal 8h * 60 = 480 min for a valid day
zerocounts = 60 # 60 consecutive zeros will be considered non-wear time
cutpoints = c(0, 100, 2296, 4012)
bts = c(0, 5, 10, 30)

#===================================
# No user input needed from here onward
setwd(path)
# source all R script in the script folder:
dirR = paste0(path,"/scripts")
for (i in dir(dirR, full.names = T)) source(i)
# specify data location?
path_input = paste0(path, "/input")
file_list <- list.files("./input/", pattern ="*.csv", all.files = FALSE)

# Generate the sequence maps:
sequence_maps <- barcoding_main_last(file_list, path_input, tz = "Europe/London", 
  fileid = "test", epochsize = 15, which = "y", rescale.epoch = 15, 
  minwear = minwear, zerocounts = zerocounts, cutpoints = cutpoints, bts = bts,
  collapse.by = "%Y-%m-%d", keep.error = FALSE)
sm_short <- sequence_maps$short_sequence
sm_short <- data.frame(sm_short)
sm_long <- sequence_maps$long_sequence
sm_long <- data.frame(sm_long)
is.na(sm_long) <- !sm_long
