# Script using ann to predict rain vs. snow
# Using Mountain rain or snow 2020-2023 crowdsourced dataset

# Keith Jennings

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

# nn params
# Import from tuning workflow
nn_tune_results <- readRDS(paste0(data_pre, "nn_tune_imbal_multi_CS.RDS"))

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
    param_best <- nn_tune_results$allphase_imbal$param_best
  }else if(tmp.scenario == "allphase_smote"){
    df_train <- df_train_allphase
    df_test <- df_test_allphase
    param_best <- nn_tune_results$allphase_smote$param_best
  }else if(tmp.scenario == "nomix_imbal"){
    df_train <- df_train_nomix
    df_test <- df_test_nomix
    param_best <- nn_tune_results$nomix_imbal$param_best
  }else if(tmp.scenario == "nomix_smote"){
    df_train <- df_train_nomix
    df_test <- df_test_nomix
    param_best <- nn_tune_results$nomix_smote$param_best
  }
  
  # Make a recipe based on imbalanced data or smote
  if(str_detect(tmp.scenario, "imbal")){
    df_recipe <- recipe(phase ~ ., data = df_train)
  }else if(str_detect(tmp.scenario, "smote")){
    df_recipe <- recipe(phase ~ ., data = df_train) %>% 
      step_smote(phase)
  }
  
  # Define the nn model
  nn_mod <- 
    mlp() %>% 
    set_engine("brulee", validation = 0) %>% 
    set_mode("classification") %>% 
    set_args(epochs = param_best$epochs,
             hidden_units = param_best$hidden_units %>% unlist(),
             penalty = param_best$penalty,
             learn_rate = param_best$learn_rate)
  
  # Make a workflow
  nn_flow_tuned <- 
    workflow() %>%
    add_model(nn_mod) %>%
    add_recipe(df_recipe) %>% 
    finalize_workflow(param_best)
  
  # Start time
  overall_time_start = Sys.time()
  
  # To create the final version of the model, run the fit on the training dataset
  # We will use this model to make future predictions
  final_model <- nn_flow_tuned %>% 
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
  export_list[[tmp.scenario]] <- list(nn_preds = df_preds, 
                                      nn_final_fit = final_fit,
                                      nn_final_model = final_model)
}

# Export the list of data
saveRDS(export_list, file = "../../data/nn_predict_imbal_multi_CS.RDS")

# ################################################################################
# # Synoptic dataset
# ################################################################################
# 
# # Import data
# df_train <- readRDS(paste0(data_pre, "nh_nomix_df_train.RDS"))
# df_test <- readRDS(paste0(data_pre, "nh_nomix_df_test.RDS"))
# 
# # nn params
# # Import from tuning workflow
# nn_tune_results <- readRDS(paste0(data_pre, "nn_tune_imbal_NH.RDS"))
# 
# 
# # Set the scenarios
# scenarios <- c("nomix_imbal") # rain/snow, imbalanced data
# 
# # Make an export list
# export_list <- list()
# 
# # Loop through the scenarios
# for(i in 1:length(scenarios)){
#   
#   # Get the scenario
#   tmp.scenario = scenarios[i]
#   
#   # And get tuned params
#   if(tmp.scenario == "allphase_imbal"){
#     param_best <- nn_tune_results$allphase_imbal$param_best
#   }else if(tmp.scenario == "allphase_smote"){
#     param_best <- nn_tune_results$allphase_smote$param_best
#   }else if(tmp.scenario == "nomix_imbal"){
#     param_best <- nn_tune_results$nomix_imbal$param_best
#   }else if(tmp.scenario == "nomix_smote"){
#     param_best <- nn_tune_results$nomix_smote$param_best
#   }
#   
#   # Make a recipe based on imbalanced data or smote
#   if(str_detect(tmp.scenario, "imbal")){
#     df_recipe <- recipe(phase ~ ., data = df_train)
#   }else if(str_detect(tmp.scenario, "smote")){
#     df_recipe <- recipe(phase ~ ., data = df_train) %>% 
#       step_smote(phase)
#   }
#   
#   # Define the nn model
#   nn_mod <- 
#     mlp() %>% 
#     set_engine("brulee", validation = 0) %>% 
#     set_mode("classification") %>% 
#     set_args(epochs = param_best$epochs,
#              hidden_units = param_best$hidden_units,
#              penalty = param_best$penalty,
#              learn_rate = param_best$learn_rate)
#   
#   # Make a workflow
#   nn_flow_tuned <- 
#     workflow() %>%
#     add_model(nn_mod) %>%
#     add_recipe(df_recipe) %>% 
#     finalize_workflow(param_best)
#   
#   # Start time
#   overall_time_start = Sys.time()
#   
#   # To create the final version of the model, run the fit on the training dataset
#   # We will use this model to make future predictions
#   final_model <- nn_flow_tuned %>% 
#     fit(df_train)
#   
#   # End timer
#   overall_time_end = Sys.time()
#   overall_time_elapsed = overall_time_end - overall_time_start
#   overall_time_elapsed
#   
#   ###############################################################################
#   
#   # Extract the final fitted data
#   # This will be used for analysis, plotting, etc.
#   final_fit <- final_model %>% extract_fit_parsnip()
#   
#   # Add the predictions to the data
#   df_preds <- bind_cols(
#     df_test,
#     predict(final_model, df_test)
#   ) %>% 
#     rename(pred_phase = .pred_class) %>% 
#     mutate(score = ifelse(phase == pred_phase, 
#                           1, 0),
#            pred_phase = tolower(pred_phase) %>% as.factor(),
#            phase = tolower(phase) %>% as.factor())
#   
#   # Add additional info for analysis
#   df_preds$scenario <- tmp.scenario
#   df_preds$source <- "nh"
#   
#   # Save output to a list
#   export_list[[tmp.scenario]] <- list(nn_preds = df_preds, 
#                                       nn_final_fit = final_fit,
#                                       nn_final_model = final_model)
# }
# 
# # Export the list of data
# saveRDS(export_list, file = "../../data/nn_predict_imbal_NH.RDS")
