# rm(list=ls())
# graphics.off()
# barcoding script Xinhui Wang, Vumc 02-02-2020
library(acc)
library(pawacc)
library(foreign)
#==================================
# User input needed:

# Specify root of repository with input and script folder:
path = "/home/vincent/projects/barcode-mapping" 
# Note: Xinhui's code expects us to use this as our
# working directory for data and scripts

value=c(0,100,2296,4012)
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
sequence_maps <- barcoding_main_last(file_list, path_input, fileid = "test",
                                     epoch =15, which="y", rescale.epoch = 15,
                                     value=value, bts=bts,
                                     collapse.by = "%Y-%m-%d",
                                     keep.error = FALSE)
sm_short <- sequence_maps$short_sequence
sm_short <- data.frame(sm_short)
sm_long <- sequence_maps$long_sequence
sm_long <- data.frame(sm_long)
is.na(sm_long) <- !sm_long
