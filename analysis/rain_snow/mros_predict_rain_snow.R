# script to test different ways of predicting precipitation phase

# Keith Jennings 
# 2022-08-02

# Load packages
library(tidymodels)
library(ranger)
library(tidyverse)
library(doMC); registerDoMC(cores = 4) # for parallel model fitting & tuning

########## User input

# Input files
citsci.input = "data/processed/mros_obs_processed_20220503.RDS"

# Threshold for n valid obs per grouping unit
obs_thresh = 300

# Output files
summary.output = #"data/processed/mros_gpm_summary_20220503.RDS"

# Load data
df <- readRDS(citsci.input) %>% ungroup() 

# Compute the ecoregion number of valid obs
eco3 <- df %>% group_by(eco_l3) %>% summarise(n = n()) %>% 
  filter(n > obs_thresh)

# Filter to just passing entries and 
# Select a few variables for the model
df <- df %>% 
  filter(tair_flag == "Pass" &
           rh_flag == "Pass" &
           ppt_flag == "Pass" &
           dist_flag == "Pass" &
           closest_flag == "Pass" &
           nstation_flag == "Pass" &
           eco_l3 %in% eco3$eco_l3) %>% 
  select(phase, eco_l3, elev, tair, twet, tdew, rh) %>% 
  na.omit() # models no likey missing data

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
  autoplot(type = "heatmap", )

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

