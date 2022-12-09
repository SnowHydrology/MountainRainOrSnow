# script to test different ways of predicting precipitation phase

# Keith Jennings 
# 2022-08-02

# Load packages
library(tidymodels)
library(ranger)
library(tidyverse)
library(doMC); registerDoMC(cores = 4) # for parallel model fitting & tuning
library(vip) # variable importance plot
library(pdp) 

########## User input

# Input files
#citsci.input = "data/processed/mros_obs_processed_20220503.RDS"
citsci.input = "data/processed/mros_obs_gpm_processed_2022.csv"

# Threshold for n valid obs per grouping unit
obs_thresh = 300

# Output files
output_dir = "data/processed/prediction/"
summary_output_allList = "summary_output_allList.RDS"
summary_output_byTair = "summary_output_byTair.RDS"

# Load data
#df <- readRDS(citsci.input) %>% ungroup() 
df <- read.csv(citsci.input) 

# Compute the ecoregion number of valid obs
eco3 <- df %>% group_by(eco_l3) %>% summarise(n = n()) %>% 
  filter(n > obs_thresh)

# Filter to just passing entries and 
# Select a few variables for the model
# Note: need to threshold the ecoregions so some obs are guaranteed in each split
df <- df %>% 
  filter(tair_flag == "Pass" &
           rh_flag == "Pass" &
           ppt_flag == "Pass" &
           dist_flag == "Pass" &
           closest_flag == "Pass" &
           nstation_flag == "Pass" &
           eco_l3 %in% eco3$eco_l3) %>% 
  select(phase, eco_l3, elev, tair, twet, tdew, rh, dowy) %>% 
  na.omit() # models no likey missing data

################################################################################
# Run the random forest analysis for all observations
################################################################################

# Set seed so that the analysis is reproducible
set.seed(6547)

# Split the data into training and testing
df_split <- initial_split(data = df,
                          prop = 0.75,  # This is the proportion of data allocated to training
                          strata = "phase")  # Stratify the sampling based on this variable

# Make new data frames of the training and testing data
df_train <- training(df_split)
df_test <- testing(df_split)


# Make a recipe
df_recipe <- recipe(phase ~ ., data = df_train)
  # %>% update_role(year, FracBurnedArea, huc6, new_role = "analysis")
  # you can use update role to add analysis or predictor variables

# Create some folds
df_folds <- vfold_cv(df_train, v = 10)

# Define the random forest model
rf_mod <- rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification") %>% 
  set_args(mtry = tune(),
           trees = tune())

# Make a workflow
rf_flow <- 
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(df_recipe)


# specify which values eant to try
rf_grid <- expand.grid(mtry = c(3, 4, 5), 
                       trees = c(100, 300, 500))

# Run the tuning grid
rf_tune_results <- rf_flow %>%
  tune_grid(resamples = df_folds, #CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(accuracy, roc_auc) # metrics we care about
  )

# print results
rf_tune_results %>%
  collect_metrics() %>% 
  knitr::kable()

# Extract the best model parameters
param_best <- rf_tune_results %>% 
  select_best(metric = "accuracy") # can also choose "roc_auc"

# Add this to the workflow
rf_flow_tuned <- rf_flow %>% 
  finalize_workflow(param_best)

# Evaluate the model by fitting to training and analyzing test
rf_fit <- rf_flow_tuned %>%
  # fit on the training set and evaluate on test set
  last_fit(df_split)

# Examine the model metrics
rf_fit %>% collect_metrics() %>% 
  knitr::kable()

# Create a confusion matrix
rf_fit %>% collect_predictions() %>% 
  conf_mat(truth = phase, estimate = .pred_class) %>% 
  autoplot(type = "heatmap" ) + scale_fill_viridis_c()

# To create the final version of the model, run the fit on the full dataset
# We will use this model to make future predictions
final_model <- rf_flow_tuned %>% 
  fit(df)

###############################################################################

# Extract the final fitted data
# This will be used for analysis, plotting, etc.
final_fit <- final_model %>% extract_fit_parsnip()

# Add the predictions to the data
df_preds <- df %>% 
  mutate(pred_mix = final_fit$fit$predictions[,1],
         pred_rain = final_fit$fit$predictions[,2],
         pred_snow = final_fit$fit$predictions[,3],
         pred_phase = ifelse(pred_mix >= 0.5,
                             "Mix",
                             ifelse(pred_rain >= 0.5,
                                    "Rain",
                                    "Snow")) %>% as.factor(),
         phase = phase %>% as.factor(),
         score = ifelse(phase == pred_phase, 
                        1, 0))

# Make a bin function for analysis
bin_fun <- function(temp, min, max, width){
  cut(temp, 
      breaks = seq(min, 
                   max, 
                   by = width))
}

# Make a numeric bin function for analysis
cuts2num_fun <- function(temp_bin, min, temp_bin_width){
  (((as.numeric(temp_bin) - 1) * temp_bin_width) + (min + (0.5 * temp_bin_width)))
}

# Add bins and numeric values to the data for tair, tdew, and twet
tair_max = max(df_preds$tair) %>% ceiling()
tair_min = min(df_preds$tair) %>% floor()
temp_bin_width = 1

# add bins to data
df_preds <- df_preds %>% 
  mutate(tair_bin = bin_fun(tair, tair_min, tair_max, temp_bin_width),
         tair_bin_num = cuts2num_fun(tair_bin, tair_min, temp_bin_width))

# SUmmarize score by bin
df_summary  <- df_preds %>% 
  group_by(tair_bin_num) %>% 
  summarize(n_obs = n(), 
            rf_perf_pct = (sum(score) / n_obs) * 100,
            snow_pct_obs = (sum(phase == "Snow") / n_obs) * 100,
            snow_pct_rf = (sum(pred_phase == "Snow") / n_obs) * 100)


# Save output to list
export_list <- list()
export_list[[1]] <- list(df_preds, 
                         df_summary, 
                         rf_fit)

################################################################################
# Run the random forest analysis for mixed = rain observations
################################################################################

# Set seed so that the analysis is reproducible
set.seed(1256)

# Convert mix to rain
df_nomix <- df %>% 
  mutate(phase = ifelse(phase == "Mix",
                        "Rain",
                        phase))

# Split the data into training and testing
df_split <- initial_split(data = df_nomix,
                          prop = 0.75,  # This is the proportion of data allocated to training
                          strata = "phase")  # Stratify the sampling based on this variable

# Make new data frames of the training and testing data
df_train <- training(df_split)
df_test <- testing(df_split)


# Make a recipe
df_recipe <- recipe(phase ~ ., data = df_train)
# %>% update_role(year, FracBurnedArea, huc6, new_role = "analysis")
# you can use update role to add analysis or predictor variables

# Create some folds
df_folds <- vfold_cv(df_train, v = 10)

# Define the random forest model
rf_mod <- rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification") %>% 
  set_args(mtry = tune(),
           trees = tune())

# Make a workflow
rf_flow <- 
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(df_recipe)

# specify which values eant to try
rf_grid <- expand.grid(mtry = c(3, 4, 5), 
                       trees = c(100, 300, 500))

# Run the tuning grid
rf_tune_results <- rf_flow %>%
  tune_grid(resamples = df_folds, #CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(accuracy, roc_auc) # metrics we care about
  )

# print results
rf_tune_results %>%
  collect_metrics() %>% 
  knitr::kable()

# Extract the best model parameters
param_best <- rf_tune_results %>% 
  select_best(metric = "accuracy") # can also choose "roc_auc"

# Add this to the workflow
rf_flow_tuned <- rf_flow %>% 
  finalize_workflow(param_best)

# Evaluate the model by fitting to training and analyzing test
rf_fit <- rf_flow_tuned %>%
  # fit on the training set and evaluate on test set
  last_fit(df_split)

# Examine the model metrics
rf_fit %>% collect_metrics() %>% 
  knitr::kable()

# Create a confusion matrix
rf_fit %>% collect_predictions() %>% 
  conf_mat(truth = phase, estimate = .pred_class) %>% 
  autoplot(type = "heatmap" ) + scale_fill_viridis_c()

# To create the final version of the model, run the fit on the full dataset
# We will use this model to make future predictions
final_model <- rf_flow_tuned %>% 
  fit(df_nomix)

###############################################################################

# Extract the final fitted data
# This will be used for analysis, plotting, etc.
final_fit <- final_model %>% extract_fit_parsnip()

# Add the predictions to the data
df_preds <- df_nomix %>% 
  mutate(pred_rain = final_fit$fit$predictions[,1],
         pred_snow = final_fit$fit$predictions[,2],
         pred_phase = ifelse(pred_rain >= 0.5,
                             "Rain",
                             "Snow") %>% as.factor(),
         phase = phase %>% as.factor(),
         score = ifelse(phase == pred_phase, 
                        1, 0))

# add bins to data
df_preds <- df_preds %>% 
  mutate(tair_bin = bin_fun(tair, tair_min, tair_max, temp_bin_width),
         tair_bin_num = cuts2num_fun(tair_bin, tair_min, temp_bin_width))

# SUmmarize score by bin
df_summary  <- df_preds %>% 
  group_by(tair_bin_num) %>% 
  summarize(n_obs = n(), 
            rf_perf_pct = (sum(score) / n_obs) * 100,
            snow_pct_obs = (sum(phase == "Snow") / n_obs) * 100,
            snow_pct_rf = (sum(pred_phase == "Snow") / n_obs) * 100)

# Save output to list
export_list[[2]] <- list(df_preds, 
                         df_summary, 
                         rf_fit)


################################################################################
# Run the analysis using common model partitioning methods (all and mix = rain)
################################################################################

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
                "Mix"))
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
obs_trim <- read.csv(citsci.input) %>% 
  filter(tair_flag == "Pass" &
           rh_flag == "Pass" &
           ppt_flag == "Pass" &
           dist_flag == "Pass" &
           closest_flag == "Pass" &
           nstation_flag == "Pass" &
           eco_l3 %in% eco3$eco_l3) %>% 
  select(id, phase, tair, twet, tdew, rh) %>% 
  na.omit()

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
    filter(phase != "Mix") %>% 
    mutate(eval_type = "noMix"),
  obs_trim %>% 
    mutate(phase = ifelse(phase == "Mix",
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
            mixed_bias_pct = (sum(phase_pred == "Mix" ) / sum(phase == "Mix") - 1) * 100,
            snow_pct = sum(phase == "Snow") / n() * 100,
            rain_pct = sum(phase == "Rain") / n() * 100,
            mixed_pct = sum(phase == "Mix") / n() * 100)

# Summarize per-temperature-bin performance by eval and method scenario
summary_byTair <-
  obs_pred %>% 
  group_by(eval_type, scenario, tair_bin_num) %>% 
  summarize(n = n(),
            perf_pct = sum(phase == phase_pred) / n() * 100,
            snow_bias_pct = (sum(phase_pred == "Snow" ) / sum(phase == "Snow") - 1) * 100,
            rain_bias_pct = (sum(phase_pred == "Rain" ) / sum(phase == "Rain") - 1) * 100,
            mixed_bias_pct = (sum(phase_pred == "Mix" ) / sum(phase == "Mix") - 1) * 100,
            snow_pct = sum(phase == "Snow") / n() * 100,
            rain_pct = sum(phase == "Rain") / n() * 100,
            mixed_pct = sum(phase == "Mix") / n() * 100)

################################################################################
# Run the analysis using IMERG (all and mix = rain)
################################################################################

# Make an df with relevant info
obsGPM <- read.csv(citsci.input) %>% 
  filter(tair_flag == "Pass" &
           rh_flag == "Pass" &
           ppt_flag == "Pass" &
           dist_flag == "Pass" &
           closest_flag == "Pass" &
           nstation_flag == "Pass" &
           eco_l3 %in% eco3$eco_l3) %>% 
  select(id, tair, gpm_prob, phase) %>% 
  na.omit()


# Add GPM probability thresholds for rain, snow, mixed
prob_thresh_upper_rain = 100
prob_thresh_lower_rain = 50
prob_thresh_upper_snow = 50
prob_thresh_lower_snow = 0
prob_thresh_upper_mixed = prob_thresh_lower_rain
prob_thresh_lower_mixed = prob_thresh_upper_snow

# Add tair bin number and GPM phase
obsGPM <- obsGPM %>% 
  mutate(tair_bin_num = floor(tair) + 0.5,
         gpm_phase = case_when(gpm_prob <= prob_thresh_upper_snow &
                                 gpm_prob >= prob_thresh_lower_snow ~ "Snow",
                               gpm_prob <= prob_thresh_upper_rain &
                                 gpm_prob >= prob_thresh_lower_rain ~ "Rain",
                               gpm_prob < prob_thresh_upper_mixed &
                                 gpm_prob > prob_thresh_lower_mixed~ "Mix"))

# Denote whether phase designation was correct or not
obsGPM_analyze <- bind_rows(
  select(obsGPM, tair_bin_num, phase, gpm_phase) %>% 
    mutate(eval_type = "all"),
  select(obsGPM, tair_bin_num, phase, gpm_phase) %>% 
    mutate(eval_type = "mixRain",
           phase = ifelse(phase == "Mix", "Rain", as.character(phase)))
)

# Summarize over all obs
gpm_summary <- obsGPM_analyze %>% 
  group_by(eval_type) %>% 
  summarize(perf_pct = sum(phase == gpm_phase) / length(phase) * 100,
            snow_pct = sum(gpm_phase == "Snow") / length(phase) * 100,
            rain_pct = sum(gpm_phase == "Rain") / length(phase) * 100,
            mixed_pct = sum(gpm_phase == "Mix") / length(phase) * 100,
            snow_bias_pct = (sum(gpm_phase == "Snow" ) / sum(phase == "Snow") - 1) * 100,
            rain_bias_pct = (sum(gpm_phase == "Rain" ) / sum(phase == "Rain") - 1) * 100,
            mixed_bias_pct = (sum(gpm_phase == "Mix" ) / sum(phase == "Mix") - 1) * 100)

# Summarize by tair bin
gpm_summary_byTair <- obsGPM_analyze %>% 
  group_by(eval_type, tair_bin_num) %>% 
  summarize(n = n(),
            perf_pct = sum(phase == gpm_phase) / length(phase) * 100,
            snow_pct = sum(gpm_phase == "Snow") / length(phase) * 100,
            rain_pct = sum(gpm_phase == "Rain") / length(phase) * 100,
            mixed_pct = sum(gpm_phase == "Mix") / length(phase) * 100,
            snow_bias_pct = (sum(gpm_phase == "Snow" ) / sum(phase == "Snow") - 1) * 100,
            rain_bias_pct = (sum(gpm_phase == "Rain" ) / sum(phase == "Rain") - 1) * 100,
            mixed_bias_pct = (sum(gpm_phase == "Mix" ) / sum(phase == "Mix") - 1) * 100)



################################################################################
# Join and synthesize the data products
################################################################################

# Data frame for summary stats for all methods
summary_allMethods_byTair <- data.frame()

export_list[[1]][[2]] <- export_list[[1]][[2]] %>% 
  rename(n = n_obs, perf_pct = rf_perf_pct) %>% 
  mutate(eval_type = "all",
         scenario = "randomForest")
export_list[[2]][[2]] <- export_list[[2]][[2]] %>% 
  rename(n = n_obs, perf_pct = rf_perf_pct) %>% 
  mutate(eval_type = "mixRain",
         scenario = "randomForest")
gpm_summary_byTair <- gpm_summary_byTair %>% 
  mutate(scenario = "IMERG")

summary_allMethods_byTair <- bind_rows(export_list[[1]][[2]],
                                       export_list[[2]][[2]],
                                       gpm_summary_byTair,
                                       summary_byTair)

# Put all df in a list

# method summaries
export_list[[3]] <- list(summary_all,
                         summary_byTair) #

# gpm summaries
export_list[[4]] <- list(gpm_summary,
                         gpm_summary_byTair)

# Name list elements
names(export_list) <- c("randomForestAll", "randomForestRainMix",
                        "phaseMethods", "gpmImerg")

# Save data
saveRDS(object = summary_allMethods_byTair, 
        file = paste0(output_dir, summary_output_byTair))
saveRDS(object = export_list, 
        file = paste0(output_dir, summary_output_allList))
