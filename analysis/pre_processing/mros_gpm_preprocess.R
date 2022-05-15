# Script for processing GPM data
# This is version 2, which uses data from Google Earth Engine
# The original WGET download script caused timeout errors, resulting in missing files


library(tidyverse)

########## User input

# Input files
citsci.input = "data/processed/mros_obs_processed_20220503.RDS"
gpm.input = "data/NOSHARE/gpm_imerg_pts_20220503_wId.csv"

# Output files
gpm.output = "data/processed/mros_gpm_processed_20220503.RDS"

# Import processed GPM data
gpm <- read.csv(gpm.input) %>% 
  rename(gpm_prob = probabilityLiquidPrecipitation) # renamed for easier typing

# Format date information
gpm <- gpm %>% 
  mutate(datetime = as.POSIXct(str_sub(system.index, 1, 12),
                               format = "%Y%m%d%H%M",
                               tz = "UTC"))

# Import the citizen science observations
obs <- readRDS(citsci.input)

# Join gpm data with obs by id
gpm_obs <- left_join(select(gpm, id, gpm_prob, datetime),
                     select(obs, id, utc_datetime, phase, tair),
                     by = "id")

# Compute difference in time between gpm and obs
# Then get minimum time diff per obs
gpm_obs <- gpm_obs %>% 
  mutate(time_diff_abs = abs(datetime - utc_datetime)) %>% 
  group_by(id) %>% 
  mutate(time_diff_min = min(time_diff_abs, na.rm = T))

# Filter to the minimum time diff
# Some obs will still have two gpm values (i.e. obs taken at midpoint between gpm timestamps)
# Take mean of gpm_prob
gpm_obs <- gpm_obs %>% 
  filter(time_diff_min == time_diff_abs) %>% 
  group_by(id) %>% 
  summarise(gpm_prob = mean(gpm_prob), 
            time_diff = mean(time_diff_abs))

# Export
saveRDS(object = gpm_obs,
        file = gpm.output)

