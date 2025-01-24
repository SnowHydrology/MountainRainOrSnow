# Script for evaluating the effectiveness of tuning the neural network model
# with multiple hidden layers

# Keith Jennings
# ksjennin@uvm.edu

# Load packages
library(tidymodels)
library(tidyverse)
library(AppliedPredictiveModeling)
library(doParallel)
library(brulee)
library(themis) # for imbalanced data

# Set the data prefix
data_pre <- "../../data/"

# Set up parallelization info
cores <- parallel::detectCores(logical = F)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

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

# Create a latin hypercube of the hyperparameter space
nn_grid <- grid_latin_hypercube(
  epochs(),
  hidden_units(),
  penalty(),
  learn_rate(),
  size = 30
)

# Make it a two layer MLP
nn_grid <- nn_grid %>% 
  rowwise() %>% 
  mutate(hidden_units = list(c(hidden_units, hidden_units))) %>% 
  ungroup()

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
  
  # Define the nn model
  nn_mod <- 
    mlp() %>% 
    set_engine("brulee", validation = 0) %>% 
    set_mode("classification") %>% 
    set_args(epochs = tune(),
             hidden_units = tune(),
             penalty = tune(),
             learn_rate = tune())
  
  # Make a workflow
  nn_flow <- 
    workflow() %>%
    add_model(nn_mod) %>%
    add_recipe(df_recipe)
  
  # Start timer
  overall_time_start = Sys.time()
  
  # hyperparameter tuning start time
  param_time_start = Sys.time()
  
  # Run the tuning grid
  nn_tune_results <- nn_flow %>%
    tune_grid(resamples = df_folds, #CV object
              grid = nn_grid, # grid of values to try
              control = control_grid(parallel_over = "everything"),
              metrics = metric_set(accuracy, roc_auc) # metrics we care about
    )
  
  # hyperparameter tuning end time
  param_time_end = Sys.time()
  param_time_elapsed = param_time_end - param_time_start
  param_time_elapsed
  
  # print results
  nn_tune_results %>%
    collect_metrics() %>% 
    knitr::kable()
  
  # Extract the best model parameters
  param_best <- nn_tune_results %>% 
    select_best(metric = "accuracy") # can also choose "roc_auc"
  
  # print results
  param_best %>%
    knitr::kable()
  
  # Add this to the workflow
  nn_flow_tuned <- nn_flow %>% 
    finalize_workflow(param_best)
  
  # End timer
  overall_time_end = Sys.time()
  overall_time_elapsed = overall_time_end - overall_time_start
  overall_time_elapsed
  
  # Save info from the tuning exercise
  export_list[[tmp.scenario]] <- list(nn_flow_tuned = nn_flow_tuned, 
                                      param_best = param_best,
                                      nn_tune_results = nn_tune_results %>% collect_metrics())
}

# Export the tuning results
saveRDS(export_list, file = paste0(data_pre, "nn_tune_imbal_multi_CS.RDS"))
