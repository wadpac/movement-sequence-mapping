Code base for deriving movement sequences from accelerometer (Actigraph) data and mapping them to symbolic classes, as originally described in: Chin A Paw et al. MSSE 2019 (https://doi.org/10.1249/mss.0000000000001849). The original version of the code was written by [Xinhui Wang](https://www.researchgate.net/profile/Xinhui_Wang5). We are currently improving this repository to become more flexible to alternative ways of pre-processing the data.

### What does this repository do?
- File **main_script.R** shows how the code is used.
- Store the Actigraph count data .csv files in one directory, and provide the required user input.
- When you run the script, the function **run_pipeline** ties together the following functions to generate sequence maps:
  1. a) The accelerometer data is loaded (using function **read_file**);
     b) The accelerometer counts are aggregated (using function **resample**);
  2. Sequence maps are generated (using function **generate_sequence**):
      a) After the non-wear time (using function 'detect_nonwear') and invalid days are filtered out;
      b) For each epoch in a data file the cut-point intensity classes, followed by the lengths of the consecutive epochs in the same cut-point class (bout lengths) and their corresponding             values are determined (using function **detect_bouts**);
      c) Then the detected segments (bout values and lengths) are labeled with the corresponding symbols (using function **add_symbols**); 
      d) Finally, the recording level (using function 'structure_per_recording') and day level (using the function 'structure_per_day') sequence maps are generated.
  
Note for developers: Run script run_roxygen2.R to update the documentation files in the man folder.