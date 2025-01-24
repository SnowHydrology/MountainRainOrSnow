# Script to partition rain and snow in citizen science and northern hemisphere 
# datasets using benchmark methods from the literature

# Keith Jennings
# kjennings@lynker.com

################################################################################
# Setup and data import

# Load packages
library(tidyverse)

# Data prefix and file names
# Note: data are local, so prefix needs to be changed if files are moved
data_pre = "../../data/"
obs_file = "mros_QAQCflags_2024_01_24.csv"
map_file = "nh_cs_thresh_ta_sv"

export_file = "cs_benchmark_preds.RDS"

# Import
obs <- read.csv(paste0(data_pre, obs_file))
map_thresh <- readRDS(paste0(data_pre, map_file))[["cs_thresh"]]

# Join the map thresholds to the obs
obs <- left_join(obs, map_thresh,
                 by = "all.id")

# Remove obs that fail QC
obs <- obs %>% 
  filter(dupe_flag == "Pass",
         phase_flag == "Pass",
         temp_air_flag == "Pass",
         rh_flag == "Pass",
         nstation_temp_air_flag == "Pass",
         nstation_temp_dew_flag == "Pass",
  )

# Rename columns for reproducibility across datasets
obs <- obs %>% 
  mutate(phase = tolower(phase)) %>% 
  rename(tair = temp_air,
         tdew = temp_dew,
         twet = temp_wet)

# Select just the needed columns
obs_select <- obs %>% 
  select(tair, twet, tdew, rh, phase, thresh_ta_sv)

################################################################################
# Create functions for predicting rain vs. snow

# Threshold
phaseThresh = function(temp, thresh){
  ifelse(temp <= thresh,
         "snow",
         "rain")
}

# Binary logistic regression with coefficients from Jennings et al. 2018
phaseBinlog <-
  function(tempC, rhPct){
    ifelse(1/(1 + exp(-10.04 + 1.41 * tempC + 0.09 * rhPct)) < 0.5,
           "rain",
           "snow") 
  }


################################################################################
# Define and run the different scenarios and methods

scenarios = c("allphase_onlymet", "nomix_onlymet")

# Phase partitioning methods (ppms)
ppms <- 
  data.frame(
    method = c(rep("thresh", 7), "binlog", "map"),
    temp_type = c(rep("tair", 2), rep("twet", 3), rep("tdew", 2), NA, "tair"),
    thresh = c(1, 1.5, 0, 0.5, 1, 0, 0.5, rep(NA, 2))
  )

# Empty data frame to store predictions
preds <- data.frame()

# Loop through the methods and estimate rain vs. snow
for(j in 1:length(scenarios)){
  
  # Get the scenario
  tmp.scenario = scenarios[j]
  
  # Remove mixed if nomix
  if(tmp.scenario == "nomix_onlymet"){
    tmp.obs = obs_select %>% filter(phase != "mix")
  } else{
    tmp.obs = obs_select
  }
  
  # Loop through the methods and estimate rain vs. snow
  for(i in 1:length(ppms$method)){
    
    # Temporary vars for methods and types
    tmp.method    = ppms[i, "method"]
    tmp.temp_type = ppms[i, "temp_type"]
    
    # If-else statements for methods
    if(tmp.method == "thresh"){
      tmp.thresh = ppms[i, "thresh"]
      tmp.pred <- tmp.obs %>% 
        rename(temp_val = !!quo(all_of(tmp.temp_type))) %>% 
        mutate(phase_pred = phaseThresh(temp_val, tmp.thresh),
               ppm = paste(tmp.method, tmp.temp_type, tmp.thresh, sep = "_"))
      if(tmp.temp_type == "tair"){
        tmp.pred <- tmp.pred %>% 
          rename(tair = temp_val)
      }else if(tmp.temp_type == "twet"){
        tmp.pred <- tmp.pred %>% 
          rename(twet = temp_val)
      } else if(tmp.temp_type == "tdew"){
        tmp.pred <- tmp.pred %>% 
          rename(tdew = temp_val)
      }
    } else if(tmp.method == "binlog"){
      tmp.pred <- tmp.obs %>% 
        mutate(phase_pred = phaseBinlog(tair, rh),
               ppm = tmp.method)
    } else if(tmp.method == "map"){
      tmp.pred <- tmp.obs %>% 
        mutate(phase_pred = phaseThresh(tair, thresh_ta_sv),
               ppm = tmp.method)
    }
    tmp.pred$scenario <- tmp.scenario
    preds <- bind_rows(preds, tmp.pred)
  }
}

# Export
saveRDS(preds, paste0(data_pre, export_file))
