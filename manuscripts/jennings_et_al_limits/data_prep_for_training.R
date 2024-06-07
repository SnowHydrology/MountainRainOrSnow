# Script for prepping data
# Doing this once to prevent data leakage

# Keith Jennings
# kjennings@lynker.com

# Load packages
library(tidyverse)
library(tidymodels)
library(humidity)

# Set data prefix
data_pre = "../../data/"

# Set seed so that the analysis is reproducible
set.seed(6547)

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
  select(phase, 
         tair = temp_air, 
         tdew = temp_dew,
         twet = temp_wet,
         rh) %>% 
         #ecoregion3) %>% 
  na.omit() # some rows do have NA values

# Get train-test split for allphase scenario
df_split <- initial_split(data = df_full,
                          prop = 0.75,  # This is the proportion of data allocated to training
                          strata = "phase")  # Stratify the sampling based on this variable

# Make new data frames of the training and testing data
df_train <- training(df_split)
df_test <- testing(df_split)

# Export these data
saveRDS(df_train,
        paste0(data_pre, "cs_allphase_df_train.RDS"))
saveRDS(df_test,
        paste0(data_pre, "cs_allphase_df_test.RDS"))

# Get train-test split for nomix scenario
df_split <- initial_split(data = filter(df_full, phase != "Mix"),
                          prop = 0.75,  # This is the proportion of data allocated to training
                          strata = "phase")  # Stratify the sampling based on this variable

# Make new data frames of the training and testing data
df_train <- training(df_split)
df_test <- testing(df_split)

# Export these data
saveRDS(df_train,
        paste0(data_pre,"cs_nomix_df_train.RDS"))
saveRDS(df_test,
        paste0(data_pre,"cs_nomix_df_test.RDS"))

################################################################################
# Synoptic Data
################################################################################

# Import the data
df_full <- read.csv(paste0(data_pre, "jennings_et_al_2018_file2_ppt_phase_met_observations.csv"))

# Modify and select
df_full <- df_full %>% 
  mutate(phase = ifelse(Snow_Phase == 1, "snow", "rain"),
         twet = wetbulb(Air_Temp, RH)) %>% 
  select(phase, Air_Temp:gridded_data_pres, twet)

# Get train-test split
df_split <- initial_split(data = df_full,
                          prop = 0.75,  # This is the proportion of data allocated to training
                          strata = "phase")  # Stratify the sampling based on this variable

# Make new data frames of the training and testing data
df_train <- training(df_split)
df_test <- testing(df_split)

# Export these data
saveRDS(df_train,
        paste0(data_pre, "nh_nomix_df_train.RDS"))
saveRDS(df_test,
        paste0(data_pre, "nh_nomix_df_test.RDS"))