# Optional script for preprocessing HADS met data after the download script
# This strips out the unnecessary variables, which saves import and processing time

# The HADS download brought in all the data, but we only want a few vars
# station = unique id in HADS metadata
# utc_valid = UTC datetime
# XRIRGZZ = relative humidity nominally instantaneous but various time steps
# TAIRGZZ = air temp nominally instantaneous but various time steps
# TDIRGZZ = dew point temp nominally instantaneous but various time steps
# The HADS var names use SHEF identifiers where:
# Positions 1+2 = var name (TA = air temp)
# Position 3 = time stamp (I = instantaneous)
# Positions 4+5 = source code (RG = GOES, RZ = nonspecified)
# Positions 6+7 = extremum and probability codes (we just want the null values, ZZ)

# Load packages
library(tidyverse)

# Name the files
hads_files <- c("data/NOSHARE/TEMP1_met_hads_20220503.RDS",
                "data/NOSHARE/TEMP2_met_hads_20220503.RDS",
                "data/NOSHARE/TEMP3_met_hads_20220503.RDS")
export_file = "data/NOSHARE/mros_met_hads_20220503.RDS"

# Create an empty dataframe to store data
df <- data.frame()

# Loop through the hads_files and remove unnecessary columns
# Note: this could be done without loops in lapply, but the memory balloons and 
# times out the run (can only be done one at a time, at least on my computer)
for(i in seq_along(hads_files)){
  # Read in data
  tmp <- readRDS(hads_files[i])
  
  # Downselect columns
  tmp <- tmp %>% 
    select(station, utc_valid, TAIRGZZ, TDIRGZZ, XRIRGZZ) %>% 
    mutate(across(3:5, as.numeric))
  
  # Bind to existing data
  df <- bind_rows(df, tmp)
  
}

# Export the data
saveRDS(df, export_file)
