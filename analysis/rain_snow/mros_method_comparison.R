# Script for comparing different modeling methods

# Import packages
library(tidyverse)

# User input
obs.input = "data/processed/mros_obs_processed_2020_2021.RDS"

# Import data
obs <- readRDS(obs.input) %>% 
  filter(., tair_flag == "Pass" & 
           ppt_flag == "Pass" & 
           rh_flag == "Pass" &
           dist_flag == "Pass" & 
           dupe_flag == "Pass")

#########################################################################
# Create functions for predicting rain vs. snow

# Threshold
phaseThresh = function(temp, thresh){
  ifelse(temp <= thresh,
     "Snow",
     "Rain")
}

# Range
phaseRange = function(temp, threshLo, threshHi){
  ifelse(temp <= threshLo,
         "Snow",
         ifelse(temp >= threshHi,
                "Rain",
                "Mixed"))
}

# Binary logistic regression with coefficients from Jennings et al. 2018
phaseBinlog <-
  function(tempC, rhPct){
    ifelse(1/(1 + exp(-10.04 + 1.41 * tempC + 0.09 * rhPct)) < 0.5,
           "Rain",
           "Snow") 
  }

# Scenarios
scenarios <- 
  data.frame(
    method = c(rep("thresh", 13), rep("range", 2), "binlog"),
    temp_type = c(rep("tair", 8), rep("twet", 3), rep("tdew", 2), rep("tair", 2), NA),
    thresh = c(0, 0.5, 1, 1.5, 1.8, 1.9, 2.7, 4.2, 0, 0.5, 1, 0, 0.5, rep(NA, 3)),
    threshLo = c(rep(NA, 13), -0.5, -1, NA),
    threshHi = c(rep(NA, 13), 0.5, 3, NA)
  )

# Make an obs with just id, phase, and met info
obs_trim <- obs %>% 
  select(id, phase, tair, twet, tdew, rh)

# Create empty df to store predictions
preds <- data.frame()

# Loop through the scenarios and estimate rain vs. snow
for(i in 1:length(scenarios$method)){
  tmp.method    = scenarios[i, "method"]
  tmp.temp_type = scenarios[i, "temp_type"]
  
  
  if(tmp.method == "thresh"){
    tmp.thresh    = scenarios[i, "thresh"]
    tmp.pred <- obs_trim %>% 
      rename(temp_val = !!quo(all_of(tmp.temp_type))) %>% 
      mutate(phase_pred = phaseThresh(temp_val, tmp.thresh),
             scenario = paste(tmp.method, tmp.temp_type, tmp.thresh, sep = "_"))
  } else if(tmp.method == "range"){
    tmp.threshLo = scenarios[i, "threshLo"]
    tmp.threshHi = scenarios[i, "threshHi"]
    tmp.pred <- obs_trim %>% 
      rename(temp_val = !!quo(all_of(tmp.temp_type))) %>% 
      mutate(phase_pred = phaseRange(temp_val, tmp.threshLo, tmp.threshHi),
             scenario = paste(tmp.method, tmp.temp_type, tmp.threshLo, tmp.threshHi, sep = "_"))
  } else {
    tmp.pred <- obs_trim %>% 
      mutate(phase_pred = phaseBinlog(tair, rh),
             scenario = tmp.method)
  }
  preds <- bind_rows(preds, tmp.pred)
}

# Make three comparsion types
# mixed
# no mixed
# mixed = rain
obs_pred <- bind_rows(
  obs_trim %>% 
    mutate(eval_type = "all"),
  obs_trim %>% 
    filter(phase != "Mixed") %>% 
    mutate(eval_type = "noMix"),
  obs_trim %>% 
    mutate(phase = ifelse(phase == "Mixed",
                          "Rain",
                          phase),
           eval_type = "mixRain")
)

# Join the predicted data to the obs_pred df for comparison
obs_pred <-
  left_join(obs_pred,
            select(preds, id, phase_pred, scenario),
            by = "id") %>% 
  mutate(tair_bin_num = floor(tair) + 0.5)

# Summarize overall performance by eval and method scenario
summary_all <- obs_pred %>% 
  group_by(eval_type, scenario) %>% 
  summarize(n = n(),
            perf_pct = sum(phase == phase_pred) / n() * 100,
            snow_bias_pct = (sum(phase_pred == "Snow" ) / sum(phase == "Snow") - 1) * 100,
            rain_bias_pct = (sum(phase_pred == "Rain" ) / sum(phase == "Rain") - 1) * 100,
            mixed_bias_pct = (sum(phase_pred == "Mixed" ) / sum(phase == "Mixed") - 1) * 100,
            snow_pct = sum(phase == "Snow") / n() * 100,
            rain_pct = sum(phase == "Rain") / n() * 100,
            mixed_pct = sum(phase == "Mixed") / n() * 100)

# Summarize per-temperature-bin performance by eval and method scenario
summary_byTair <-
  obs_pred %>% 
  group_by(eval_type, scenario, tair_bin_num) %>% 
  summarize(n = n(),
            perf_pct = sum(phase == phase_pred) / n() * 100,
            snow_bias_pct = (sum(phase_pred == "Snow" ) / sum(phase == "Snow") - 1) * 100,
            rain_bias_pct = (sum(phase_pred == "Rain" ) / sum(phase == "Rain") - 1) * 100,
            mixed_bias_pct = (sum(phase_pred == "Mixed" ) / sum(phase == "Mixed") - 1) * 100,
            snow_pct = sum(phase == "Snow") / n() * 100,
            rain_pct = sum(phase == "Rain") / n() * 100,
            mixed_pct = sum(phase == "Mixed") / n() * 100)

# Export the summaries
saveRDS(object = summary_all,
        file = "data/processed/mros_obs_sim_summary.RDS")
saveRDS(object = summary_byTair,
        file = "data/processed/mros_obs_sim_summary_byTair.RDS")

