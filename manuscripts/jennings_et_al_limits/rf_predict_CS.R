# Script using random forest to predict rain vs. snow
# Using Mountain rain or snow 2020-2023 crowdsourced dataset

# Keith Jennings
# kjennings@lynker.com

# Load packages
library(tidymodels)
library(ranger)
library(tidyverse)
library(doMC); registerDoMC(cores = 4) # for parallel model fitting & tuning
library(vip) # variable importance plot
library(pdp) 
library(humidity)

# # Random forest params
# mtry_num = 3
# ntrees_num = 100

# Import data
# Note this is the local address
# Full dataset to be posted to web soon (column names and order may vary)
df_full <- read.csv("../../data/mros_met_geog_2023_09_21_all.csv")

# Prep dataset
df <- df_full %>% 
  select(phase, 
         tair = temp_air_idw_lapse_var, 
         tdew = temp_dew_idw_lapse_var,
         twet = temp_wet,
         rh) %>% 
  na.omit() # some rows do have NA values

################################################################################
# Test if tuning is needed
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


# Start timee
overall_time_start = Sys.time()

# specify which values eant to try
rf_grid <- expand.grid(mtry = c(3, 4, 5), 
                       trees = c(100, 300, 500))

# hyperparameter tuning start time
param_time_start = Sys.time()

# Run the tuning grid
rf_tune_results <- rf_flow %>%
  tune_grid(resamples = df_folds, #CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(accuracy, roc_auc) # metrics we care about
  )

# hyperparameter tuning end time
param_time_end = Sys.time()
param_time_elapsed = param_time_end - param_time_start

# print results
rf_tune_results %>%
  collect_metrics() %>% 
  knitr::kable()

# Extract the best model parameters
param_best <- rf_tune_results %>% 
  select_best(metric = "accuracy") # can also choose "roc_auc"

# print results
param_best %>%
  knitr::kable()

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

# End timer
overall_time_end = Sys.time()
overall_time_elapsed = overall_time_end - overall_time_start







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
# you can use update role to add analysis or predictor variables

# Define the random forest model
rf_mod <- rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification") %>% 
  set_args(mtry = mtry_num,
           trees = ntrees_num)

# Make a workflow
rf_flow_tuned <- 
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(df_recipe)


# Start time
overall_time_start = Sys.time()

# To create the final version of the model, run the fit on the full dataset
# We will use this model to make future predictions
final_model <- rf_flow_tuned %>% 
  fit(df)

# End timer
overall_time_end = Sys.time()
overall_time_elapsed = overall_time_end - overall_time_start
overall_time_elapsed

###############################################################################

# Extract the final fitted data
# This will be used for analysis, plotting, etc.
final_fit <- final_model %>% extract_fit_parsnip()

# Add the predictions to the data
df_preds <- df %>% 
  mutate(pred_rain = final_fit$fit$predictions[,1],
         pred_snow = final_fit$fit$predictions[,2],
         pred_phase = ifelse(pred_rain >= 0.5,
                             "rain",
                             "snow") %>% as.factor(),
         phase = phase %>% as.factor(),
         score = ifelse(phase == pred_phase, 
                        1, 0))

# add bins to data
df_preds <- df_preds %>% 
  mutate(tair_bin_num = floor(Air_Temp) + 0.5)


# Summarize score by bin
df_summary  <- df_preds %>% 
  group_by(tair_bin_num) %>% 
  summarize(n_obs = n(), 
            rf_perf_pct = (sum(score) / n_obs) * 100,
            snow_pct_obs = (sum(phase == "snow") / n_obs) * 100,
            snow_pct_rf = (sum(pred_phase == "snow") / n_obs) * 100)

# Save output to list
export_list <- list(df_preds, 
                         df_summary, 
                         final_fit,
                         final_model)
save(export_list, file = "../../data/rf_predict_NHdataset_full.rdata")
