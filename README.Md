Code base for deriving movement sequences from accelerometer (Actigraph) data and mapping them to symbolic classes, as originally described in: Chin A Paw et al. MSSE 2019 (https://doi.org/10.1249/mss.0000000000001849). The original version of the code was written by [Xinhui Wang](https://www.researchgate.net/profile/Xinhui_Wang5). We are currently improving this repository to become more flexible to alternative ways of pre-processing the data.

### What does this repository do?
- File **generate_sequence_maps.R** shows how the code is used.
- Place the Actigraph count data file in the input folder.
- When you run the script the Actigraph count data is loaded
- Next, the time series are labelled in behavioural classes based on the cut-point technique and bout duration criteria.
- These classes are then mapped to symbolic classes labelled from 1 to 12, and used to derive movement sequence per day and per recording. 
- In the original paper the resulting sequence are then fed into a clustering algorithm (not part of this repository).


Note for developers: Run script run_roxygen2.R to update the documentation files in the man folder.