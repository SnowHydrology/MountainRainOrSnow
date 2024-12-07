# Script for prepping data to share online

# Keith Jennings
# kjennings@lynker.com

# Load packages
library(tidyverse)
library(tidymodels)
library(humidity)

# Set data prefix
data_pre = "../../data/"

################################################################################
# Crowdsourced Data
################################################################################

# Data to import
df_full <- read.csv(paste0(data_pre, "mros_QAQCflags_2024_01_24.csv"))

# Remove obs that fail QC
df_full <- df_full %>% 
  filter(dupe_flag == "Pass",
         phase_flag == "Pass",
         temp_air_flag == "Pass",
         rh_flag == "Pass",
         nstation_temp_air_flag == "Pass",
         nstation_temp_dew_flag == "Pass",
  )

# Perform alternative dupe check to catch any that snuck through
df_full  <- df_full %>% 
  group_by(latitude, longitude, datetime_utc) %>% 
  mutate(n_obs = n()) %>% 
  filter(n_obs == 1)

# Prep dataset for all scenarios
df_full <- df_full %>% 
  ungroup() %>% # remove grouping vars
  select(latitude,
         longitude,
         datetime_utc,
         elevation.m,
         phase, 
         tair = temp_air, 
         tdew = temp_dew,
         twet = temp_wet,
         rh) %>% 
  na.omit() # some rows do have NA values

# Export these data
write.csv(df_full,
          file = "data/processed/mros_processed_2020_2023.csv", 
          row.names = F, quote = F)
