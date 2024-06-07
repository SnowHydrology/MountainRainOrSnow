# Script for evaluating the effectiveness of tuning the random forest model

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
library(themis) # for imbalanced data

# Set the data prefix
data_pre <- "../../data/"

# Set seed so that the analysis is reproducible
set.seed(6547)

################################################################################
# Crowdsourced dataset
################################################################################

# Import data
df_train_allphase <- readRDS(paste0(data_pre, "cs_allphase_df_train.RDS"))
df_test_allphase <- readRDS(paste0(data_pre, "cs_allphase_df_test.RDS"))
df_train_nomix <- readRDS(paste0(data_pre, "cs_nomix_df_train.RDS"))
df_test_nomix <- readRDS(paste0(data_pre, "cs_nomix_df_test.RDS"))

# Set the scenarios
scenarios <- c("allphase_imbal", # rain/snow/mixed, imbalanced data
               "allphase_smote", # rain/snow/mixed, balanced data w/smote
               "nomix_imbal", # rain/snow, imbalanced data
               "nomix_smote") # rain/snow, balanced data w/smote

# Make an export list
export_list <- list()

# Loop through scenarios and tune models
for(i in seq_along(scenarios)){
  
  # Get the scenario
  tmp.scenario = scenarios[i]
  
  # Assign the data based on scenario
  if(tmp.scenario %in% c("allphase_imbal", "allphase_smote") ){
    df_train <- df_train_allphase
    df_test <- df_test_allphase
  }else if(tmp.scenario %in% c("nomix_imbal", "nomix_smote")){
    df_train <- df_train_nomix
    df_test <- df_test_nomix
  }
  
  # Make a recipe based on imbalanced data or smote
  if(str_detect(tmp.scenario, "imbal")){
    df_recipe <- recipe(phase ~ ., data = df_train)
  }else if(str_detect(tmp.scenario, "smote")){
    df_recipe <- recipe(phase ~ ., data = df_train) %>% 
      step_smote(phase)
  }
  
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
  
  # Start timer
  overall_time_start = Sys.time()
  
  # specify which values to try
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
  param_time_elapsed
  
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
  
  # End timer
  overall_time_end = Sys.time()
  overall_time_elapsed = overall_time_end - overall_time_start
  overall_time_elapsed
  
  # Save info from the tuning exercise
  export_list[[tmp.scenario]] <- list(rf_flow_tuned = rf_flow_tuned, 
                                      param_best = param_best,
                                      rf_tune_results = rf_tune_results %>% collect_metrics())
}

# Export the tuning results
saveRDS(export_list, file = paste0(data_pre, "rf_tune_imbal_CS.RDS"))

################################################################################
# Northern Hemisphere dataset
################################################################################

# Import data
df_big <- read.csv("../../data/jennings_et_al_2018_file2_ppt_phase_met_observations.csv")

# Make a small version for testing
n_samples = 0.01 * nrow(df_big) %>% round(., digits = 0)
df <- df_big %>% sample_n(n_samples) %>% 
  mutate(phase = ifelse(Snow_Phase == 1, "snow", "rain"),
         twet = wetbulb(Air_Temp, RH)) %>% 
  select(phase, Air_Temp:gridded_data_pres, twet)

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

# End timer
overall_time_end = Sys.time()
overall_time_elapsed = overall_time_end - overall_time_start
overall_time_elapsed

# Save info from the tuning exercise
nh_nomix_metonly_rf_flow_tuned <- rf_flow_tuned
nh_nomix_metonly_rf_param_best <- param_best
nh_nomix_metonly_rf_tune_results <- rf_tune_results %>% collect_metrics()

################################################################################
# Save all of the metrics
################################################################################

save(cs_allphase_onlymet_rf_flow_tuned,
     cs_allphase_onlymet_rf_param_best,
     cs_allphase_onlymet_rf_tune_results,
     cs_nomix_onlymet_rf_flow_tuned,
     cs_nomix_onlymet_rf_param_best,
     cs_nomix_onlymet_rf_tune_results,
     cs_allphase_meteco_rf_flow_tuned,
     cs_allphase_meteco_rf_param_best,
     cs_allphase_meteco_rf_tune_results,
     cs_nomix_meteco_rf_flow_tuned,
     cs_nomix_meteco_rf_param_best,
     cs_nomix_meteco_rf_tune_results,
     nh_nomix_metonly_rf_flow_tuned,
     nh_nomix_metonly_rf_param_best,
     nh_nomix_metonly_rf_tune_results,
     file = "../../data/rf_tune_results.rdata")


# I redid the CS tuning with updated data
# To avoid rerunning NH tuning, I had to import and re-export the data
# hold_list <- list(
#   cs_allphase_onlymet_rf_flow_tuned = cs_allphase_onlymet_rf_flow_tuned,
#   cs_allphase_onlymet_rf_param_best = cs_allphase_onlymet_rf_param_best,
#   cs_allphase_onlymet_rf_tune_results = cs_allphase_onlymet_rf_tune_results,
#   cs_nomix_onlymet_rf_flow_tuned = cs_nomix_onlymet_rf_flow_tuned,
#   cs_nomix_onlymet_rf_param_best = cs_nomix_onlymet_rf_param_best,
#   cs_nomix_onlymet_rf_tune_results = cs_nomix_onlymet_rf_tune_results,
#   cs_allphase_meteco_rf_flow_tuned = cs_allphase_meteco_rf_flow_tuned,
#   cs_allphase_meteco_rf_param_best = cs_allphase_meteco_rf_param_best,
#   cs_allphase_meteco_rf_tune_results = cs_allphase_meteco_rf_tune_results,
#   cs_nomix_meteco_rf_flow_tuned = cs_nomix_meteco_rf_flow_tuned,
#   cs_nomix_meteco_rf_param_best = cs_nomix_meteco_rf_param_best,
#   cs_nomix_meteco_rf_tune_results = cs_nomix_meteco_rf_tune_results
# )
# rm(list=setdiff(ls(), "hold_list"))
# load("../../data/rf_tune_results.rdata")
# rm(list=setdiff(ls(), c("hold_list", "nh_nomix_metonly_rf_flow_tuned",
#                         "nh_nomix_metonly_rf_param_best",
#                         "nh_nomix_metonly_rf_tune_results")))
# list2env(hold_list, globalenv())

