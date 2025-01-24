# Script using random forest to predict rain vs. snow
# Using Jennings et al. (2018) northern hemisphere dataset

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

# Random forest params
mtry_num = 3
ntrees_num = 100

# time_scaling <- data.frame(obs = c(10000,100000,1000000,5000000), time_h = c(0.00124, 0.03472, 1.342686,13.95557) )

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

# Subset data for testing
#df_big <- df
#df <- df_big[1:1000000,]

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
