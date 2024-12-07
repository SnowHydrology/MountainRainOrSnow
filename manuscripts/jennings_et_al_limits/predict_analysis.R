# Script for summarizing and analyzing the precipitation phase predictions

# Keith Jennings
# kjennings@lynker.com

################################################################################
# Setup

# Load packages
library(tidyverse)

# Data prefix and file names
# Note: data are local, so prefix needs to be changed if files are moved
data_pre = "../../data/"

# Output file names
summary_all_out = "performance_summary_all.RDS"
summary_bytemp_out = "performance_summary_bytemp.RDS"
all_data_out = "predictions_all.RDS"

# Benchmark data
benchmark_cs_file = "cs_benchmark_preds_ontest.RDS"
benchmark_nh_file = "nh_benchmark_preds_ontest.RDS"

# Random forest data
rf_cs_file = "rf_predict_imbal_CS.RDS"
rf_nh_file = "rf_predict_imbal_NH.RDS"

# XGBoost data
xg_cs_file = "xg_predict_imbal_CS.RDS"
xg_nh_file = "xg_predict_imbal_NH.RDS"

# NN data
nn_cs_file = "nn_predict_imbal_CS.RDS"
nn_nh_file = "nn_predict_imbal_NH.RDS"

################################################################################
# Import data

# Benchmark
benchmark_cs <- readRDS(paste0(data_pre, benchmark_cs_file)) %>% 
  mutate(scenario = ifelse(scenario == "allphase_onlymet",
                           "allphase_imbal",
                           ifelse(scenario == "nomix_onlymet",
                                  "nomix_imbal",
                                  scenario)))
benchmark_nh <- readRDS(paste0(data_pre, benchmark_nh_file)) %>% 
  select(-gridded_data_pres) %>% 
  mutate(scenario = ifelse(scenario == "nomix_onlymet",
                                  "nomix_imbal",
                                  scenario))

# Random forest
rf_cs_tmp <- readRDS(paste0(data_pre, rf_cs_file))
rf_cs <- bind_rows(rf_cs_tmp$allphase_imbal$rf_preds,
                   rf_cs_tmp$allphase_smote$rf_preds,
                   rf_cs_tmp$nomix_imbal$rf_preds,
                   rf_cs_tmp$nomix_smote$rf_preds) %>% 
  mutate(ppm = "rf") %>% 
  rename(phase_pred = pred_phase) %>% 
  select(-score)
rm(rf_cs_tmp)
rf_nh <- readRDS(paste0(data_pre, rf_nh_file))
rf_nh <- rf_nh$nomix_imbal$rf_preds %>% 
  select(phase, phase_pred = pred_phase, tair = Air_Temp, twet, tdew = Dewpoint, rh = RH) %>% 
  mutate(ppm = "rf",
         source = "nh") %>% 
  mutate(scenario = "nomix_imbal")

# XGBoost
xg_cs_tmp <- readRDS(paste0(data_pre, xg_cs_file))
xg_cs <- bind_rows(xg_cs_tmp$allphase_imbal$xg_preds,
                   xg_cs_tmp$allphase_smote$xg_preds,
                   xg_cs_tmp$nomix_imbal$xg_preds,
                   xg_cs_tmp$nomix_smote$xg_preds) %>% 
  mutate(ppm = "xg") %>% 
  rename(phase_pred = pred_phase) %>% 
  select(-score)
rm(xg_cs_tmp)
xg_nh_tmp <- readRDS(paste0(data_pre, xg_nh_file))
xg_nh <- xg_nh_tmp$nomix_imbal$rf_preds %>% 
  select(phase, phase_pred = pred_phase, tair = Air_Temp, twet, tdew = Dewpoint,
         rh = RH, source, scenario) %>% 
  mutate(ppm = "xg")
rm(xg_nh_tmp)

# NN
nn_cs_tmp <- readRDS(paste0(data_pre, nn_cs_file))
nn_cs <- bind_rows(nn_cs_tmp$allphase_imbal$nn_preds,
                   nn_cs_tmp$allphase_smote$nn_preds,
                   nn_cs_tmp$nomix_imbal$nn_preds,
                   nn_cs_tmp$nomix_smote$nn_preds) %>% 
  mutate(ppm = "nn") %>% 
  rename(phase_pred = pred_phase) %>% 
  select(-score)
rm(nn_cs_tmp)
nn_nh_tmp <- readRDS(paste0(data_pre, nn_nh_file))
nn_nh <- nn_nh_tmp$nomix_imbal$nn_preds %>% 
  select(phase, phase_pred = pred_phase, tair = Air_Temp, twet, tdew = Dewpoint,
         rh = RH, source, scenario) %>% 
  mutate(ppm = "nn")
rm(nn_nh_tmp)


################################################################################
# Join and bind data
df_all <- bind_rows(
  benchmark_cs %>% 
    mutate(source = "cs"),
  benchmark_nh %>% 
    mutate(source = "nh"),
  rf_cs,
  rf_nh,
  xg_cs,
  xg_nh,
  nn_cs,
  nn_nh
)

################################################################################
# Check for NAs just in case
df_filter <- df_all %>% 
  filter(!is.na(phase_pred)) %>% 
  filter(!is.na(phase))

################################################################################
# Calculate accuracy and bias

# Summarize overall performance by eval and method scenario
summary_all <- df_filter %>% 
  group_by(ppm, scenario, source) %>% 
  summarize(n = n(),
            accuracy_pct = sum(phase == phase_pred) / n() * 100,
            snow_bias_pct = (sum(phase_pred == "snow" ) / sum(phase == "snow") - 1) * 100,
            rain_bias_pct = (sum(phase_pred == "rain" ) / sum(phase == "rain") - 1) * 100,
            mixed_bias_pct = (sum(phase_pred == "mix" ) / sum(phase == "mix") - 1) * 100,
            snow_obs_pct = sum(phase == "snow") / n() * 100,
            rain_obs_pct = sum(phase == "rain") / n() * 100,
            mixed_obs_pct = sum(phase == "mix") / n() * 100,
            snow_pred_pct = sum(phase_pred == "snow") / n() * 100,
            rain_pred_pct = sum(phase_pred == "rain") / n() * 100,
            mixed_pred_pct = sum(phase_pred == "mix") / n() * 100)

# Summarize overall performance by eval and method scenario and temperature bin
summary_bytemp <- df_filter %>% 
  mutate(tair_bin = round(tair/0.5) * 0.5) %>% 
  group_by(ppm, scenario, source, tair_bin) %>% 
  summarize(n = n(),
            accuracy_pct = sum(phase == phase_pred) / n() * 100,
            snow_bias_pct = (sum(phase_pred == "snow" ) / sum(phase == "snow") - 1) * 100,
            rain_bias_pct = (sum(phase_pred == "rain" ) / sum(phase == "rain") - 1) * 100,
            mixed_bias_pct = (sum(phase_pred == "mix" ) / sum(phase == "mix") - 1) * 100,
            snow_obs_pct = sum(phase == "snow") / n() * 100,
            rain_obs_pct = sum(phase == "rain") / n() * 100,
            mixed_obs_pct = sum(phase == "mix") / n() * 100,
            snow_pred_pct = sum(phase_pred == "snow") / n() * 100,
            rain_pred_pct = sum(phase_pred == "rain") / n() * 100,
            mixed_pred_pct = sum(phase_pred == "mix") / n() * 100)

################################################################################
# Save the summaries and joined data
saveRDS(object = summary_all,
        file = paste0(data_pre, summary_all_out))
saveRDS(object = summary_bytemp,
        file = paste0(data_pre, summary_bytemp_out))
saveRDS(object = df_filter,
        file = paste0(data_pre, all_data_out))


