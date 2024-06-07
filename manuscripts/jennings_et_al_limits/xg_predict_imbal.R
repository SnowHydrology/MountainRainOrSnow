# Script using xgboost to predict rain vs. snow
# Using Mountain rain or snow 2020-2023 crowdsourced dataset

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

# Best XGBoost params
# Import from tuning workflow
xg_tune_results <- readRDS(paste0(data_pre, "xg_tune_imbal_CS.RDS"))

# Set the scenarios
scenarios <- c("allphase_imbal", # rain/snow/mixed, imbalanced data
               "allphase_smote", # rain/snow/mixed, balanced data w/smote
               "nomix_imbal", # rain/snow, imbalanced data
               "nomix_smote") # rain/snow, balanced data w/smote

# Make an export list
export_list <- list()

# Loop through the scenarios
for(i in 1:length(scenarios)){
  
  # Get the scenario
  tmp.scenario = scenarios[i]
  
  # Prep the data based on scenario
  # And get tuned params
  if(tmp.scenario == "allphase_imbal"){
    df_train <- df_train_allphase
    df_test <- df_test_allphase
    param_best <- xg_tune_results$allphase_imbal$param_best
  }else if(tmp.scenario == "allphase_smote"){
    df_train <- df_train_allphase
    df_test <- df_test_allphase
    param_best <- xg_tune_results$allphase_smote$param_best
  }else if(tmp.scenario == "nomix_imbal"){
    df_train <- df_train_nomix
    df_test <- df_test_nomix
    param_best <- xg_tune_results$nomix_imbal$param_best
  }else if(tmp.scenario == "nomix_smote"){
    df_train <- df_train_nomix
    df_test <- df_test_nomix
    param_best <- xg_tune_results$nomix_smote$param_best
  }
  
  # Make a recipe based on imbalanced data or smote
  if(str_detect(tmp.scenario, "imbal")){
    df_recipe <- recipe(phase ~ ., data = df_train)
  }else if(str_detect(tmp.scenario, "smote")){
    df_recipe <- recipe(phase ~ ., data = df_train) %>% 
      step_smote(phase)
  }
  
  # Define the xgboost model
  xg_mod <- boost_tree() %>% 
    set_engine("xgboost") %>% 
    set_mode("classification") %>% 
    set_args(trees = param_best$trees,
             tree_depth = param_best$tree_depth, 
             min_n = param_best$min_n,
             loss_reduction = param_best$loss_reduction,                     
             sample_size = param_best$sample_size,
             mtry = param_best$mtry,         
             learn_rate = param_best$learn_rate)
  
  # Make a workflow
  xg_flow_tuned <- 
    workflow() %>%
    add_model(xg_mod) %>%
    add_recipe(df_recipe) %>% 
    finalize_workflow(param_best)
  
  # Start time
  overall_time_start = Sys.time()
  
  # To create the final version of the model, run the fit on the training dataset
  # We will use this model to make future predictions
  final_model <- xg_flow_tuned %>% 
    fit(df_train)
  
  # End timer
  overall_time_end = Sys.time()
  overall_time_elapsed = overall_time_end - overall_time_start
  overall_time_elapsed
  
  ###############################################################################
  
  # Extract the final fitted data
  # This will be used for analysis, plotting, etc.
  final_fit <- final_model %>% extract_fit_parsnip()
  
  # Add the predictions to the data
  df_preds <- bind_cols(
    df_test,
    predict(final_model, df_test)
  ) %>% 
    rename(pred_phase = .pred_class) %>% 
    mutate(score = ifelse(phase == pred_phase, 
                          1, 0),
           pred_phase = tolower(pred_phase) %>% as.factor(),
           phase = tolower(phase) %>% as.factor())
  
  # Add additional info for analysis
  df_preds$scenario <- tmp.scenario
  df_preds$source <- "cs"
  
  # Save output to a list
  export_list[[tmp.scenario]] <- list(xg_preds = df_preds, 
                                      xg_final_fit = final_fit,
                                      xg_final_model = final_model)
}

# Export the list of data
saveRDS(export_list, file = "../../data/xg_predict_imbal_CS.RDS")

################################################################################
# Synoptic dataset
################################################################################

# Import data
df_train <- readRDS(paste0(data_pre, "nh_nomix_df_train.RDS"))
df_test <- readRDS(paste0(data_pre, "nh_nomix_df_test.RDS"))

# Best XGBoost params
# Import from tuning workflow
xg_tune_results <- readRDS(paste0(data_pre, "xg_tune_imbal_NH.RDS"))

# Set the scenarios
scenarios <- c("nomix_imbal") # rain/snow, imbalanced data

# Make an export list
export_list <- list()

# Loop through the scenarios
for(i in 1:length(scenarios)){
  
  # Get the scenario
  tmp.scenario = scenarios[i]
  
  # get tuned params    
  param_best <- xg_tune_results$nomix_imbal$param_best

  # Make a recipe based on imbalanced data or smote
  if(str_detect(tmp.scenario, "imbal")){
    df_recipe <- recipe(phase ~ ., data = df_train)
  }else if(str_detect(tmp.scenario, "smote")){
    df_recipe <- recipe(phase ~ ., data = df_train) %>% 
      step_smote(phase)
  }
  
  # Define the xgboost model
  xg_mod <- boost_tree() %>% 
    set_engine("xgboost") %>% 
    set_mode("classification") %>% 
    set_args(trees = param_best$trees,
             tree_depth = param_best$tree_depth, 
             min_n = param_best$min_n,
             loss_reduction = param_best$loss_reduction,                     
             sample_size = param_best$sample_size,
             mtry = param_best$mtry,         
             learn_rate = param_best$learn_rate)
  
  # Make a workflow
  xg_flow_tuned <- 
    workflow() %>%
    add_model(xg_mod) %>%
    add_recipe(df_recipe) %>% 
    finalize_workflow(param_best)
  
  # Start time
  overall_time_start = Sys.time()
  
  # To create the final version of the model, run the fit on the training dataset
  # We will use this model to make future predictions
  final_model <- xg_flow_tuned %>% 
    fit(df_train)
  
  # End timer
  overall_time_end = Sys.time()
  overall_time_elapsed = overall_time_end - overall_time_start
  overall_time_elapsed
  
  ###############################################################################
  
  # Extract the final fitted data
  # This will be used for analysis, plotting, etc.
  final_fit <- final_model %>% extract_fit_parsnip()
  
  # Add the predictions to the data
  df_preds <- bind_cols(
    df_test,
    predict(final_model, df_test)
  ) %>% 
    rename(pred_phase = .pred_class) %>% 
    mutate(score = ifelse(phase == pred_phase, 
                          1, 0),
           pred_phase = tolower(pred_phase) %>% as.factor(),
           phase = tolower(phase) %>% as.factor())
  
  # Add additional info for analysis
  df_preds$scenario <- tmp.scenario
  df_preds$source <- "nh"
  
  # Save output to a list
  export_list[[tmp.scenario]] <- list(rf_preds = df_preds, 
                                      rf_final_fit = final_fit,
                                      rf_final_model = final_model)
}

# Export the list of data
saveRDS(export_list, file = "../../data/xg_predict_imbal_NH.RDS")
