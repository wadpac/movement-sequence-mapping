rm(list = ls())
# graphics.off()
# barcoding script Xinhui Wang, Vumc 02-02-2020
library(acc)
library(pawacc)
library(foreign)
library(devtools)
install_github("wadpac/movement-sequence-mapping")
# install_github("wadpac/barcode-mapping", force = T, ref ="vincent_zero_investigation")
# for (i in dir("~/projects/barcode-mapping/R", full.names = T)) source(i)
# library(barcodeMapping)
#==================================
# User input needed:

# specify data location?
path_input ="~/data/dummy_data_sequence_mapping"


# Specify folder with R scripts (funtions):
path = "/home/vincent/projects/movement-sequence-mapping/R"
# path = "/Users/annelinde/Documents/PROGRAMMING/cutpoint-approach-wang2019"
# Note: Xinhui's code expects us to use this as our
# working directory for data and scripts

minwear = 480 # Minimal 8h * 60 = 480 min for a valid day
zerocounts = 60 # 60 consecutive zeros will be considered as non-wear time
cutpoints = c(0, 100, 2296, 4012)
bts = c(0, 5, 10, 30)

#===================================
# No user input needed from here onward
# setwd(path)
# source all R script in the script folder:
dirR = paste0(path)
for (i in dir(dirR, full.names = T)) source(i)

# Generate the sequence maps:
sequence_maps <- sequence_mapping_main_last(path_input, tz = "Europe/London",
  fileid = "test", epochsize = 15, which = "y", rescale.epoch = 15, 
  minwear = minwear, zerocounts = zerocounts, cutpoints = cutpoints, bts = bts,
  collapse.by = "%Y-%m-%d", keep.error = FALSE, tolerance_function="V2")

sm_short <- sequence_maps$short_sequence
sm_short <- data.frame(sm_short)
sm_long <- sequence_maps$long_sequence
sm_long <- data.frame(sm_long)
is.na(sm_long) <- !sm_long
# print frequency table to check how often clases occur
print(table(sequence_maps$short_sequence))
print(table(sequence_maps$long_sequence))

# Check that values in long and short are identical:
G = sequence_maps$long_sequence
G = G[1,which(is.na(G[1,])==F)]
S = sequence_maps$short_sequence
S = c(S[1,],S[2,],S[3,])
S = S[which(is.na(S)==F)]
G = G[which(is.na(G)==F)]
print(table(S == G))

TEST = tolerance(bouts_values=c(3,2,3,2,3,2,3,4), bouts_lengths=c(100,1,100,1,100, 1, 100,5), allow_bout=3,
                 timethreshold1=10, timethreshold2=5, Nepoch_per_minute=1)
print(TEST$values == c(3,4))
print(TEST$lengths == c(403,5))
