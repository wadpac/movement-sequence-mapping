rm(list=ls())

# Specify folder where dummy data needs to be stored
data_folder = "~/data"

#----------------------------------------------------------------------
# No user-input needed from here onward:
data_folder2 = paste0(data_folder, "/dummy_data_sequence_mapping")
if (!dir.exists(data_folder2)) {
  dir.create(data_folder2)
}
filename = paste0(data_folder2, "/3_123_01 (2020-01-01)15sec.csv")

epochsize = 15
serialnumber = "Simulation"
starttime = "00:00:00"
startdate = "1-1-2020"
downloaddate = "1-1-2020"
header = c("------------ Data File Created By ActiGraph wGT3XBT ActiLife v6.13.3 Firmware v1.9.2 date format d-M-yyyy Filter LowFrequencyExtension Multiple Incline Limb: Waist -----------",
           paste0("Serial Number: ",serialnumber),
           paste0("Start Time ", starttime),
           paste0("Start Date ",startdate),
           "Epoch Period (hh:mm:ss) 00:00:15",
           paste0("Download Time ",starttime),
           paste0("Download Date ",downloaddate),
           "Current Memory Address: 0",
           "Current Battery Voltage: 3    8     Mode = 45",
           "--------------------------------------------------")

create_bout = function(bout_dur=10, bout_int="MOD",
                          break_dur=0, break_int="SB", N_epoch_per_minute=4) {
  # Function to create bout based on specified duration, 
  # intensity, total break duration and break intensity
  if (bout_int == "NW") bout_int = 0 # nonwear
  if (bout_int == "SB") bout_int = 50 / 4 # divided by 4 because it is 15 second data
  if (bout_int == "LIG") bout_int = 1000 / 4
  if (bout_int == "MOD") bout_int = 3000 / 4
  if (bout_int == "VIG") bout_int = 5000 / 4
  if (break_int == "NW") break_int = 0
  if (break_int == "SB") break_int = 50 /4 
  if (break_int == "LIG") break_int = 1000 / 4
  if (break_int == "MOD") break_int = 3000 / 4
  if (break_int == "VIG") break_int = 5000 / 4
  segment = rep(bout_int, N_epoch_per_minute * bout_dur)
  if (break_dur > 0) {
    set.seed(1234)
    gaps  = sample(2:(length(segment)-1), size = break_dur * N_epoch_per_minute, replace = FALSE)
    segment[gaps] = break_int
  }
  return(segment)
}

simulated = c(create_bout(bout_dur = 61, bout_int = "NW"), #non-wear without break
            # without breaks
            create_bout(bout_dur = 20, bout_int = "MOD"), # moderate
            create_bout(bout_dur = 20, bout_int = "LIG"), # light
            create_bout(bout_dur = 20, bout_int = "VIG"), #vigorous
            create_bout(bout_dur = 20, bout_int = "SB"), #sedentary behaviour
            # 1 minute light break
            create_bout(bout_dur = 20, bout_int = "VIG", break_dur = 1, break_int = "LIG"),
            create_bout(bout_dur = 20, bout_int = "MOD", break_dur = 1, break_int = "LIG"),
            create_bout(bout_dur = 20, bout_int = "SB", break_dur = 1, break_int = "LIG"), 
            # 1 minute moderate break
            create_bout(bout_dur = 20, bout_int = "VIG", break_dur = 1, break_int = "MOD"),
            create_bout(bout_dur = 20, bout_int = "LIG", break_dur = 1, break_int = "MOD"),
            create_bout(bout_dur = 20, bout_int = "SB", break_dur = 1, break_int = "MOD"), 
            # 1 minute vigorous break
            create_bout(bout_dur = 20, bout_int = "MOD", break_dur = 1, break_int = "VIG"),
            create_bout(bout_dur = 20, bout_int = "LIG", break_dur = 1, break_int = "VIG"),
            create_bout(bout_dur = 20, bout_int = "SB", break_dur = 1, break_int = "VIG"),
            # 1 minute moderate break during non-wear
            create_bout(bout_dur = 61, bout_int = "NW", break_dur = 1, break_int = "MOD"), 
            # Long bouts without breaks
            create_bout(bout_dur = 59, bout_int = "SB"), 
            create_bout(bout_dur = 59, bout_int = "MOD"), 
            create_bout(bout_dur = 40, bout_int = "LIG"), 
            create_bout(bout_dur = 40, bout_int = "SB"), 
            create_bout(bout_dur = 40, bout_int = "LIG"), 
            create_bout(bout_dur = 40, bout_int = "MOD"), 
            create_bout(bout_dur = 40, bout_int = "VIG"), 
            create_bout(bout_dur = 40, bout_int = "SB"), 
            create_bout(bout_dur = 40, bout_int = "LIG"), 
            create_bout(bout_dur = 40, bout_int = "MOD"), 
            create_bout(bout_dur = 40, bout_int = "VIG"),
            create_bout(bout_dur = 40, bout_int = "SB"), 
            create_bout(bout_dur = 40, bout_int = "LIG"), 
            create_bout(bout_dur = 40, bout_int = "MOD"), 
            create_bout(bout_dur = 40, bout_int = "VIG"), 
            create_bout(bout_dur = 40, bout_int = "SB"), 
            create_bout(bout_dur = 40, bout_int = "LIG"), 
            create_bout(bout_dur = 40, bout_int = "MOD"), 
            create_bout(bout_dur = 40, bout_int = "VIG"),
            create_bout(bout_dur = 40, bout_int = "SB"), 
            create_bout(bout_dur = 40, bout_int = "LIG"), 
            create_bout(bout_dur = 40, bout_int = "MOD"), 
            create_bout(bout_dur = 40, bout_int = "VIG"), 
            create_bout(bout_dur = 40, bout_int = "SB"), 
            create_bout(bout_dur = 40, bout_int = "LIG"), 
            create_bout(bout_dur = 20, bout_int = "SB"))

# So we expect to find:
# 61 minutes of non-wear
# 4 x 20 + 61 + 59 + 6 * 40 + 20 minutes spent in SB bouts
# 6 x 20 + 59 + 8 * 40 minutes spent in MVPA bouts
# 3 x 20 + 7 x 40 minutes spent in light bouts
print(paste0("N hours in simulated data:", length(simulated)/(4*60)))

# Now replicate this day 3 times
simulated = rep(simulated, 3)

N_epoch_per_minute = (60/epochsize)
Ndays = length(simulated) / (1440 * N_epoch_per_minute)
print(paste0("Ndays of data: ",Ndays))

# create simulated data
# counts x, counts y, counts z, steps, 15, 0, 0, 0 # the last 5 columns we do not use
simdata = matrix(0, length(simulated), 8)
simdata[,5]  = 15
# Create sumulate y-axis data
simdata[,1] = simulated 

# Store header and data in csv file that has same file structure as original csv files:
cat(paste(header,"\n",collapse=""), file= filename)
write.table(simdata, file= filename,row.names = F, col.names = F,sep=",",fileEncoding="UTF-8", append=T)

# print(table(simdata[,1]))