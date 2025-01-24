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

# Random forest params
# Import from tuning workflow
load("../../data/rf_tune_results.rdata")

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
# Run the random forest analysis for all observations
################################################################################

# Set seed so that the analysis is reproducible
set.seed(6547)

# List the modeling scenarios
scenarios = c("allphase_onlymet", "nomix_onlymet", "allphase_meteco", "nomix_meteco")

# Make an empty list to store output
export_list <- list()

# Loop through the scenarios
for(i in 1:length(scenarios)){
  
  # Get the scenario
  tmp.scenario = scenarios[i]
  
  # Prep the data based on scenario
  # And get tuned params
  if(tmp.scenario == "allphase_onlymet"){
    df <- df_full %>% 
      select(-ecoregion3)
    mtry_num = cs_allphase_onlymet_rf_param_best$mtry
    ntrees_num = cs_allphase_onlymet_rf_param_best$trees
  }else if(tmp.scenario == "nomix_onlymet"){
    df <- df_full %>% 
      filter(phase != "Mix") %>% 
      select(-ecoregion3)
    mtry_num = cs_nomix_onlymet_rf_param_best$mtry
    ntrees_num = cs_nomix_onlymet_rf_param_best$trees
  }else if(tmp.scenario == "allphase_meteco"){
    df <- df_full
    mtry_num = cs_allphase_meteco_rf_param_best$mtry
    ntrees_num = cs_allphase_meteco_rf_param_best$trees
  }else if(tmp.scenario == "nomix_meteco"){
    df <- df_full %>% 
      filter(phase != "Mix")
    mtry_num = cs_nomix_meteco_rf_param_best$mtry
    ntrees_num = cs_nomix_meteco_rf_param_best$trees
  }
  
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
  final_model <- rf_flow_tuned %>% last_fit(cs_nomix_onlymet_rf_flow_tuned, split = df_split)
    #fit(df)
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
    mutate(pred_phase = tolower(pred_phase) %>% as.factor(),
           phase = tolower(phase) %>% as.factor(),
           score = ifelse(phase == pred_phase, 
                          1, 0))
  
  # if(tmp.scenario == "allphase_onlymet" | tmp.scenario == "allphase_meteco"){
  #   df_preds <- df %>% 
  #     mutate(pred_mix = final_fit$fit$predictions[,1],
  #            pred_rain = final_fit$fit$predictions[,2],
  #            pred_snow = final_fit$fit$predictions[,3],
  #            pred_phase = ifelse(pred_rain >= 0.5,
  #                                "rain",
  #                                ifelse(pred_snow >= 0.5,
  #                                       "snow", 
  #                                       "mix")) %>% as.factor(),
  #            phase = tolower(phase) %>% as.factor(),
  #            score = ifelse(phase == pred_phase, 
  #                           1, 0))
  # }else{
  #   df_preds <- df %>% 
  #     mutate(pred_rain = final_fit$fit$predictions[,1],
  #            pred_snow = final_fit$fit$predictions[,2],
  #            pred_phase = ifelse(pred_rain >= 0.5,
  #                                "rain",
  #                                "snow") %>% as.factor(),
  #            phase = tolower(phase) %>% as.factor(),
  #            score = ifelse(phase == pred_phase, 
  #                           1, 0))
  # }
  # 
  # Add additional info for analysis
  df_preds$scenario <- tmp.scenario
  df_preds$source <- "cs"
  
  # Save output to a list
  export_list[[tmp.scenario]] <- list(df_preds, 
                                      final_fit,
                                      final_model)
}

# Export the list of data
#saveRDS(export_list, file = "../../data/rf_predict_CSdataset_full.RDS")
saveRDS(export_list, file = "../../data/rf_predict_CSdataset_full_TEST.RDS")
