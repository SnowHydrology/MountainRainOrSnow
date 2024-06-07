# Script for evaluating the effectiveness of tuning the xgboost model

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

################################################################################
# Crowdsourced dataset
################################################################################

# Import data
# Note this is the local address
# Full dataset to be posted to web soon (column names and order may vary)
df_full <- read.csv("../../data/mros_QAQCflags_2024_01_24.csv")

# Remove obs that fail QC
df_full <- df_full %>% 
  filter(dupe_flag == "Pass",
         phase_flag == "Pass",
         temp_air_flag == "Pass",
         rh_flag == "Pass",
         nstation_temp_air_flag == "Pass",
         nstation_temp_dew_flag == "Pass",
  )

# Prep dataset for all scenarios
df_full <- df_full %>% 
  select(phase, 
         tair = temp_air, 
         tdew = temp_dew,
         twet = temp_wet,
         rh,
         ecoregion3) %>% 
  na.omit() # some rows do have NA values

################################################################################
# Rain, snow, and mixed, only met variables

# Prep dataset
df <- df_full %>% 
  select(-ecoregion3)

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


# Start timee
overall_time_start = Sys.time()

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
param_time_elapsed
overall_time_elapsed

# Save info from the tuning exercise
cs_allphase_onlymet_xg_flow_tuned <- xg_flow_tuned
cs_allphase_onlymet_xg_param_best <- param_best
cs_allphase_onlymet_xg_tune_results <- xg_tune_results %>% collect_metrics()

################################################################################
# Rain and snow only met variables

# Prep dataset
df <- df_full %>% 
  filter(phase != "Mix") %>% 
  select(-ecoregion3)

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


# Start timee
overall_time_start = Sys.time()

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
param_time_elapsed
overall_time_elapsed

# Save info from the tuning exercise
cs_nomix_onlymet_xg_flow_tuned <- xg_flow_tuned
cs_nomix_onlymet_xg_param_best <- param_best
cs_nomix_onlymet_xg_tune_results <- xg_tune_results %>% collect_metrics()


################################################################################
# Rain, snow, and mixed,met variables and ecoregion

# Prep dataset
df <- df_full

# Get the number of obs per ecoregion
n_per_eco <- df %>% 
  group_by(ecoregion3) %>% 
  summarize(n = n())

# Filter out ecoregions with less than a threshold observations
n_per_eco_thresh = 100
df <- df %>% 
  filter(ecoregion3 %in% filter(n_per_eco, n > n_per_eco_thresh)$ecoregion3)

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
# Step dummy converts the categorical variable to numeric
df_recipe <- recipe(phase ~ ., data = df_train) %>% 
  step_dummy(ecoregion3) %>% 
  prep()

# Create some folds
df_folds <- vfold_cv(df_train, v = 10)

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


# Start timee
overall_time_start = Sys.time()

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
param_time_elapsed
overall_time_elapsed

# Save info from the tuning exercise
cs_allphase_meteco_xg_flow_tuned <- xg_flow_tuned
cs_allphase_meteco_xg_param_best <- param_best
cs_allphase_meteco_xg_tune_results <- xg_tune_results %>% collect_metrics()


################################################################################
# Rain and snow, met variables and ecoregion

# Prep dataset
df <- df_full %>% 
  filter(phase != "Mix")

# Get the number of obs per ecoregion
n_per_eco <- df %>% 
  group_by(ecoregion3) %>% 
  summarize(n = n())

# Filter out ecoregions with less than a threshold observations
n_per_eco_thresh = 100
df <- df %>% 
  filter(ecoregion3 %in% filter(n_per_eco, n > n_per_eco_thresh)$ecoregion3)

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
# Step dummy converts the categorical variable to numeric
df_recipe <- recipe(phase ~ ., data = df_train) %>% 
  step_dummy(ecoregion3) %>% 
  prep()

# Create some folds
df_folds <- vfold_cv(df_train, v = 10)

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


# Start timee
overall_time_start = Sys.time()

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
param_time_elapsed
overall_time_elapsed

# Save info from the tuning exercise
cs_nomix_meteco_xg_flow_tuned <- xg_flow_tuned
cs_nomix_meteco_xg_param_best <- param_best
cs_nomix_meteco_xg_tune_results <- xg_tune_results %>% collect_metrics()

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


# Start timee
overall_time_start = Sys.time()

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
param_time_elapsed
overall_time_elapsed

# Save info from the tuning exercise
nh_nomix_metonly_xg_flow_tuned <- xg_flow_tuned
nh_nomix_metonly_xg_param_best <- param_best
nh_nomix_metonly_xg_tune_results <- xg_tune_results %>% collect_metrics()

################################################################################
# Save all of the metrics
################################################################################

save(cs_allphase_onlymet_xg_flow_tuned,
     cs_allphase_onlymet_xg_param_best,
     cs_allphase_onlymet_xg_tune_results,
     cs_nomix_onlymet_xg_flow_tuned,
     cs_nomix_onlymet_xg_param_best,
     cs_nomix_onlymet_xg_tune_results,
     cs_allphase_meteco_xg_flow_tuned,
     cs_allphase_meteco_xg_param_best,
     cs_allphase_meteco_xg_tune_results,
     cs_nomix_meteco_xg_flow_tuned,
     cs_nomix_meteco_xg_param_best,
     cs_nomix_meteco_xg_tune_results,
     nh_nomix_metonly_xg_flow_tuned,
     nh_nomix_metonly_xg_param_best,
     nh_nomix_metonly_xg_tune_results,
     file = "../../data/xg_tune_results.rdata")

# I redid the CS tuning with updated data
# To avoid rerunning NH tuning, I had to import and re-export the data
# hold_list <- list(
#   cs_allphase_onlymet_xg_flow_tuned = cs_allphase_onlymet_xg_flow_tuned,
#   cs_allphase_onlymet_xg_param_best = cs_allphase_onlymet_xg_param_best,
#   cs_allphase_onlymet_xg_tune_results = cs_allphase_onlymet_xg_tune_results,
#   cs_nomix_onlymet_xg_flow_tuned = cs_nomix_onlymet_xg_flow_tuned,
#   cs_nomix_onlymet_xg_param_best = cs_nomix_onlymet_xg_param_best,
#   cs_nomix_onlymet_xg_tune_results = cs_nomix_onlymet_xg_tune_results,
#   cs_allphase_meteco_xg_flow_tuned = cs_allphase_meteco_xg_flow_tuned,
#   cs_allphase_meteco_xg_param_best = cs_allphase_meteco_xg_param_best,
#   cs_allphase_meteco_xg_tune_results = cs_allphase_meteco_xg_tune_results,
#   cs_nomix_meteco_xg_flow_tuned = cs_nomix_meteco_xg_flow_tuned,
#   cs_nomix_meteco_xg_param_best = cs_nomix_meteco_xg_param_best,
#   cs_nomix_meteco_xg_tune_results = cs_nomix_meteco_xg_tune_results
# )
# rm(list=setdiff(ls(), "hold_list"))
# load("../../data/xg_tune_results.rdata")
# rm(list=setdiff(ls(), c("hold_list", "nh_nomix_metonly_xg_flow_tuned",
#                         "nh_nomix_metonly_xg_param_best",
#                         "nh_nomix_metonly_xg_tune_results")))
# list2env(hold_list, globalenv())
