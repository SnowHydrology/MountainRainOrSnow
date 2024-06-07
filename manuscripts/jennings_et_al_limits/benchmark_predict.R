# Script to partition rain and snow in citizen science and northern hemisphere 
# datasets using benchmark methods from the literature

# This update to a previous version analyzes only the test data for each dataset
# for consistent comparisons with the ML methods

# Keith Jennings
# kjennings@lynker.com

################################################################################
# Crowdsourced Data
################################################################################

################################################################################
# Setup and data import for crowdsourced dataset

# Load packages
library(tidyverse)

# Data prefix and file names
# Note: data are local, so prefix needs to be changed if files are moved
data_pre = "../../data/"
export_file = "cs_benchmark_preds_ontest.RDS"

# Import
df_test_allphase = readRDS(paste0(data_pre, "cs_allphase_df_test.RDS"))
df_test_nomix = readRDS(paste0(data_pre, "cs_nomix_df_test.RDS"))

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

ppms <- 
  data.frame(
    method = c(rep("thresh", 7), "binlog"),
    temp_type = c(rep("tair", 2), rep("twet", 3), rep("tdew", 2), NA),
    thresh = c(1, 1.5, 0, 0.5, 1, 0, 0.5, NA)
  )

# Empty data frame to store predictions
preds <- data.frame()

# Loop through the methods and estimate rain vs. snow
for(j in 1:length(scenarios)){
  
  # Get the scenario
  tmp.scenario = scenarios[j]
  
  # Select data by scenario
  if(tmp.scenario == "nomix_onlymet"){
    tmp.obs = df_test_nomix %>% 
      mutate(phase = tolower(phase)) 
  } else{
    tmp.obs = df_test_allphase %>% 
      mutate(phase = tolower(phase)) 
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

################################################################################
# Synoptic Data
################################################################################

################################################################################
# Data import for synoptic dataset

# Note: data are local, so prefix needs to be changed if files are moved
export_file_syn = "nh_benchmark_preds_ontest.RDS"

# Import
df_test_nomix_syn = readRDS(paste0(data_pre, "nh_nomix_df_test.RDS"))

################################################################################
# Define and run the different scenarios and methods

# Only one scenario this time, no mixed data in synoptic
tmp.scenario = "nomix_onlymet"

# Empty data frame to store predictions
preds <- data.frame()

# Assign the data
tmp.obs <- df_test_nomix_syn %>% 
  rename(tair = Air_Temp,
         tdew = Dewpoint,
         rh = RH)

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

# Export
saveRDS(preds, paste0(data_pre, export_file_syn))
