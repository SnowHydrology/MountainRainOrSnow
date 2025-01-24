# Apply a stacked model ensemble to crowdsourced data

# Load packages
library(tidymodels)
library(tidyverse)
library(AppliedPredictiveModeling)
library(brulee) # for MLP
library(ranger) # for random forest
library(xgboost) # for xgboost
library(stacks) # for hybrid models
Sys.setenv(KMP_DUPLICATE_LIB_OK=TRUE) # needed to prevent Brulee error in ANN

# Set the data prefix
data_pre <- "../../data/"

# Set seed so that the analysis is reproducible
set.seed(6547)

################################################################################
# No mixed dataset
################################################################################

################################################################################
# Import data
df_train <- readRDS(paste0(data_pre, "cs_nomix_df_train.RDS"))
df_test <- readRDS(paste0(data_pre, "cs_nomix_df_test.RDS")) %>% 
  mutate(phase = as.factor(phase))

# Make a data recipe and folds
df_recipe <- recipe(phase ~ ., data = df_train)
df_folds <- vfold_cv(df_train, v = 10)

################################################################################
# Create models and workflows

# Random forest model
rf_mod <- rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification") %>% 
  set_args(mtry = 3,
           trees = 300)

# Random forest workflow
rf_flow <- 
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(df_recipe)

# XGBoost model 
xg_mod <- boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") %>% 
  set_args(trees = 577,
           tree_depth = 11, 
           min_n = 9,
           loss_reduction = 8.31e-07,                     
           sample_size = 0.788,
           mtry = 4,         
           learn_rate = 4.53e-07)

# XGBoost workflow
xg_flow <- 
  workflow() %>%
  add_model(xg_mod) %>%
  add_recipe(df_recipe)

# Multilayer perceptron (nn) model
nn_mod <- 
  mlp() %>% 
  set_engine("brulee", validation = 0) %>% 
  set_mode("classification") %>% 
  set_args(epochs = 635,
           hidden_units = 9,
           penalty = 1.49e-06,
           learn_rate = 0.00422)

# Multilayer perceptron (nn) workflow
nn_flow <- 
  workflow() %>%
  add_model(nn_mod) %>%
  add_recipe(df_recipe)

################################################################################
# Fit models

# Random forest
rf_res <- fit_resamples(rf_flow, df_folds, 
                        control = control_resamples(save_pred = TRUE, 
                                                    save_workflow = TRUE))

# XGBoost
xg_res <- fit_resamples(xg_flow, df_folds, 
                        control = control_resamples(save_pred = TRUE, 
                                                    save_workflow = TRUE))
# Multilayer perceptron
nn_res <- fit_resamples(nn_flow, df_folds,  
                        control = control_resamples(save_pred = TRUE, 
                                                    save_workflow = TRUE))
                        
################################################################################
# Make a hybrid model using the fit models

# Stack models using the stacks package
model_stack <- stacks() %>%
  add_candidates(rf_res) %>% 
  add_candidates(xg_res) %>% 
  add_candidates(nn_res)

# Fit the stacked hybrid model
stack_fit <- model_stack %>%
  blend_predictions() %>%
  fit_members()

# Evaluate the hybrid model
stack_preds <- predict(stack_fit, df_test) %>% 
  mutate(phase = df_test$phase) %>% 
  rename(phase_pred = .pred_class)

# Summarize the data
summary_nomix <- stack_preds %>% 
  mutate(phase_pred = tolower(phase_pred),
         phase = tolower(phase)) %>% 
  #group_by(ppm, scenario, source) %>% 
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
# Mixed dataset
################################################################################

################################################################################
# Import data
df_train <- readRDS(paste0(data_pre, "cs_allphase_df_train.RDS"))
df_test <- readRDS(paste0(data_pre, "cs_allphase_df_test.RDS")) %>% 
  mutate(phase = as.factor(phase))

# Make a data recipe and folds
df_recipe <- recipe(phase ~ ., data = df_train)
df_folds <- vfold_cv(df_train, v = 10)

################################################################################
# Create models and workflows

# Random forest model
rf_mod <- rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification") %>% 
  set_args(mtry = 5,
           trees = 500)

# Random forest workflow
rf_flow <- 
  workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(df_recipe)

# XGBoost model 
xg_mod <- boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") %>% 
  set_args(trees = 1040,
           tree_depth = 3, 
           min_n = 19,
           loss_reduction = 0.171,                     
           sample_size = 0.284,
           mtry = 1,         
           learn_rate = 0.00506)

# XGBoost workflow
xg_flow <- 
  workflow() %>%
  add_model(xg_mod) %>%
  add_recipe(df_recipe)

# Multilayer perceptron (nn) model
nn_mod <- 
  mlp() %>% 
  set_engine("brulee", validation = 0) %>% 
  set_mode("classification") %>% 
  set_args(epochs = 635,
           hidden_units = 9,
           penalty = 1.49e-06,
           learn_rate = 0.00422)

# Multilayer perceptron (nn) workflow
nn_flow <- 
  workflow() %>%
  add_model(nn_mod) %>%
  add_recipe(df_recipe)

################################################################################
# Fit models

# Random forest
rf_res_allphase <- fit_resamples(rf_flow, df_folds, 
                        control = control_resamples(save_pred = TRUE, 
                                                    save_workflow = TRUE))

# XGBoost
xg_res_allphase <- fit_resamples(xg_flow, df_folds, 
                        control = control_resamples(save_pred = TRUE, 
                                                    save_workflow = TRUE))
# Multilayer perceptron
nn_res_allphase <- fit_resamples(nn_flow, df_folds,  
                        control = control_resamples(save_pred = TRUE, 
                                                    save_workflow = TRUE,
                                                    parallel_over = "everything"))

################################################################################
# Make a hybrid model using the fit models

# Stack models using the stacks package
model_stack_allphase <- stacks() %>%
  add_candidates(rf_res_allphase) %>% 
  add_candidates(xg_res_allphase) %>% 
  add_candidates(nn_res_allphase)

# Fit the stacked hybrid model
stack_fit_allphase <- model_stack_allphase %>%
  blend_predictions() %>%
  fit_members()

# Evaluate the hybrid model
stack_preds_allphase <- predict(stack_fit_allphase, df_test) %>% 
  mutate(phase = df_test$phase) %>% 
  rename(phase_pred = .pred_class)

# Summarize the data
summary_allphase <- stack_preds_allphase %>% 
  mutate(phase_pred = tolower(phase_pred),
         phase = tolower(phase)) %>% 
  #group_by(ppm, scenario, source) %>% 
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
# Export the data
################################################################################

# Create a list for the data export
export_list <- list()

# Add the no mixed data
export_list[["nomix_imbal"]] <- 
  list(stack_preds = stack_preds %>% 
         mutate(scenario = "nomix_imbal",
                source = "cs"), 
       stack_final_fit = stack_fit,
       stack_final_model = model_stack)

# Add the all phase data
export_list[["allphase_imbal"]] <- 
  list(stack_preds = stack_preds_allphase %>% 
         mutate(scenario = "allphase_imbal",
                source = "cs"), 
       stack_final_fit = stack_fit_allphase,
       stack_final_model = model_stack_allphase)

# Export
saveRDS(export_list, file = "../../data/stacks_predict_CS.RDS")

