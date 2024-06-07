# Script for evaluating the effectiveness of tuning the random forest model

# Keith Jennings
# kjennings@lynker.com

# Load packages
library(tidymodels)
library(xgboost)
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
for(i in 1:length(scenarios)){
  
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
  
  # Create a latin hypercube of the hyperparameter space
  xg_grid <- grid_latin_hypercube(
    trees(),
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), df_train),
    learn_rate(),
    size = 30
  )

  # Define the xgboost model
  xg_mod <- boost_tree() %>% 
    set_engine("xgboost") %>% 
    set_mode("classification") %>% 
    set_args(trees = tune(),
             tree_depth = tune(), 
             min_n = tune(),
             loss_reduction = tune(),                     
             sample_size = tune(),
             mtry = tune(),         
             learn_rate = tune())
  
  # Make a workflow
  xg_flow <- 
    workflow() %>%
    add_model(xg_mod) %>%
    add_recipe(df_recipe)
  
  # Start timer
  overall_time_start = Sys.time()
  
  # hyperparameter tuning start time
  param_time_start = Sys.time()
  
  # Run the tuning grid
  xg_tune_results <- xg_flow %>%
    tune_grid(resamples = df_folds, #CV object
              grid = xg_grid, # grid of values to try
              metrics = metric_set(accuracy, roc_auc) # metrics we care about
    )
  
  # hyperparameter tuning end time
  param_time_end = Sys.time()
  param_time_elapsed = param_time_end - param_time_start
  param_time_elapsed
  
  # print results
  xg_tune_results %>%
    collect_metrics() %>% 
    knitr::kable()
  
  # Extract the best model parameters
  param_best <- xg_tune_results %>% 
    select_best(metric = "accuracy") # can also choose "roc_auc"
  
  # print results
  param_best %>%
    knitr::kable()
  
  # Add this to the workflow
  xg_flow_tuned <- xg_flow %>% 
    finalize_workflow(param_best)
  
  # End timer
  overall_time_end = Sys.time()
  overall_time_elapsed = overall_time_end - overall_time_start
  overall_time_elapsed
  
  # Save info from the tuning exercise
  export_list[[tmp.scenario]] <- list(xg_flow_tuned = xg_flow_tuned, 
                                      param_best = param_best,
                                      xg_tune_results = xg_tune_results %>% collect_metrics())
}

# Export the tuning results
saveRDS(export_list, file = paste0(data_pre, "xg_tune_imbal_CS.RDS"))

################################################################################
# Northern Hemisphere dataset
################################################################################

# Import data
df_train_nomix <- readRDS(paste0(data_pre, "nh_nomix_df_train.RDS"))

# Make a small version for testing
n_samples = 0.01 * nrow(df_train_nomix) %>% round(., digits = 0)
df_train <- df_train_nomix %>% 
  sample_n(n_samples)

# Make a recipe
df_recipe <- recipe(phase ~ ., data = df_train)

# Create some folds
df_folds <- vfold_cv(df_train, v = 10)

# Create a latin hypercube of the hyperparameter space
xg_grid <- grid_latin_hypercube(
  trees(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), df_train),
  learn_rate(),
  size = 30
)

# Define the xgboost model
xg_mod <- boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") %>% 
  set_args(trees = tune(),
           tree_depth = tune(), 
           min_n = tune(),
           loss_reduction = tune(),                     
           sample_size = tune(),
           mtry = tune(),         
           learn_rate = tune())

# Make a workflow
xg_flow <- 
  workflow() %>%
  add_model(xg_mod) %>%
  add_recipe(df_recipe)

# Start timer
overall_time_start = Sys.time()

# hyperparameter tuning start time
param_time_start = Sys.time()

# Run the tuning grid
xg_tune_results <- xg_flow %>%
  tune_grid(resamples = df_folds, #CV object
            grid = xg_grid, # grid of values to try
            metrics = metric_set(accuracy, roc_auc) # metrics we care about
  )

# hyperparameter tuning end time
param_time_end = Sys.time()
param_time_elapsed = param_time_end - param_time_start
param_time_elapsed

# print results
xg_tune_results %>%
  collect_metrics() %>% 
  knitr::kable()

# Extract the best model parameters
param_best <- xg_tune_results %>% 
  select_best(metric = "accuracy") # can also choose "roc_auc"

# print results
param_best %>%
  knitr::kable()

# Add this to the workflow
xg_flow_tuned <- xg_flow %>% 
  finalize_workflow(param_best)

# End timer
overall_time_end = Sys.time()
overall_time_elapsed = overall_time_end - overall_time_start
overall_time_elapsed

# Save info from the tuning exercise
export_list <- list()
export_list[["nomix_imbal"]] <- list(xg_flow_tuned = xg_flow_tuned, 
                                    param_best = param_best,
                                    xg_tune_results = xg_tune_results %>% collect_metrics())

# Export the tuning results
saveRDS(export_list, file = paste0(data_pre, "xg_tune_imbal_NH.RDS"))
