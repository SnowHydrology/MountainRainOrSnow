# Script using xgboost to predict rain vs. snow
# Using Jennings et al. (2018) northern hemisphere dataset

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

# XGBoost params
# Import from tuning workflow
load("../../data/xg_tune_results.rdata")

# Import data
# Note this is the local address
# Accessible via web (big file!):
# https://datadryad.org/stash/downloads/file_stream/67085
df <- read.csv("../../data/jennings_et_al_2018_file2_ppt_phase_met_observations.csv")

# Prep dataset
df <- df %>% 
  mutate(phase = ifelse(Snow_Phase == 1, "snow", "rain"),
         twet = wetbulb(Air_Temp, RH)) %>% 
  select(phase, Air_Temp:gridded_data_pres, twet)

# Rename columns
df <- df %>% 
  rename(tair = Air_Temp,
         tdew = Dewpoint,
         rh = RH)

################################################################################
# Run the xgboost analysis for all observations
################################################################################

# Set seed so that the analysis is reproducible
set.seed(6547)

# Make an empty list to store output
export_list <- list()

# Get the best params
param_best <- nh_nomix_metonly_xg_param_best

# Split the data into training and testing
df_split <- initial_split(data = df,
                          prop = 0.75,  # This is the proportion of data allocated to training
                          strata = "phase")  # Stratify the sampling based on this variable

# Make new data frames of the training and testing data
df_train <- training(df_split)
df_test <- testing(df_split)

# Make a recipe
df_recipe <- recipe(phase ~ ., data = df_train)

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

# To create the final version of the model, run the fit on the full dataset
# We will use this model to make future predictions
final_model <- xg_flow_tuned %>% 
  fit(df)

# End timer
overall_time_end = Sys.time()
overall_time_elapsed = overall_time_end - overall_time_start
overall_time_elapsed

###############################################################################

# Extract the final fit
final_fit <- final_model %>% extract_fit_parsnip()

# Add the predictions to the data
preds <- predict(final_model, df)
df_preds <- bind_cols(df, phase_pred = preds$.pred_class) %>% 
  mutate(phase_pred = tolower(phase_pred) %>% as.factor(),
         phase = as.factor(phase),
         source = "nh",
         ppm = "xg")

# Save output to a list
export_list[["nh_nomix_onlymet"]] <- list(df_preds, 
                                    final_fit,
                                    final_model)

# Export the list of data
saveRDS(export_list, file = "../../data/xg_predict_NHdataset_full.RDS")
