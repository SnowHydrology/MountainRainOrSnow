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

# XGBoost params
# Import from tuning workflow
load("../../data/xg_tune_results.rdata")

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
# Run the xgboost analysis for all observations
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
    param_best <- cs_allphase_onlymet_xg_param_best

  }else if(tmp.scenario == "nomix_onlymet"){
    df <- df_full %>% 
      filter(phase != "Mix") %>% 
      select(-ecoregion3)
    param_best <- cs_nomix_onlymet_xg_param_best
    
  }else if(tmp.scenario == "allphase_meteco"){
    df <- df_full
    
    # Get the number of obs per ecoregion
    n_per_eco <- df %>% 
      group_by(ecoregion3) %>% 
      summarize(n = n())
    
    # Filter out ecoregions with less than a threshold observations
    n_per_eco_thresh = 100
    df <- df %>% 
      filter(ecoregion3 %in% filter(n_per_eco, n > n_per_eco_thresh)$ecoregion3)
    
    param_best <- cs_allphase_meteco_xg_param_best
    
  }else if(tmp.scenario == "nomix_meteco"){
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
    
    param_best <- cs_nomix_meteco_xg_param_best
    
  }
  
  # Split the data into training and testing
  df_split <- initial_split(data = df,
                            prop = 0.75,  # This is the proportion of data allocated to training
                            strata = "phase")  # Stratify the sampling based on this variable
  
  # Make new data frames of the training and testing data
  df_train <- training(df_split)
  df_test <- testing(df_split)
  
  if(tmp.scenario == "allphase_onlymet" | tmp.scenario == "nomix_onlymet"){
    # Make a recipe
    df_recipe <- recipe(phase ~ ., data = df_train)
  } else{
    # Step dummy converts the categorical variable to numeric
    df_recipe <- recipe(phase ~ ., data = df_train) %>% 
      step_dummy(ecoregion3) %>% 
      prep()
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
           phase = tolower(phase) %>% as.factor(),
           scenario = tmp.scenario,
           source = "cs",
           ppm = "xg")
  
  # Save output to a list
  export_list[[tmp.scenario]] <- list(df_preds, 
                                      final_fit,
                                      final_model)
}

# Export the list of data
saveRDS(export_list, file = "../../data/xg_predict_CSdataset_full.RDS")
