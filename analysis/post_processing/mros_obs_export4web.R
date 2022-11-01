# Script for outputting processed citizen science data and ancillary info

# TODO: move other post-processing steps here (currently buried in other files)

# Load package
library(tidyverse)

# User input
obs.file = "data/processed/mros_obs_processed_2020_2021.RDS"
gpm.file = "data/processed/mros_gpm_processed_2020_2021.RDS"
output.file = "data/processed/mros_obs_processed_2020_2021.csv"

# Read in RDS files and join
df <- readRDS(obs.file) %>% 
  left_join(readRDS(gpm.file), by = "id") %>% 
  select(-time_diff) %>% 
  mutate(gpm_prob = round(gpm_prob, digits = 0))

# Export csv
write.csv(df, output.file, quote = F, row.names = F)
