library(AppliedPredictiveModeling)
library(tidymodels)
library(doParallel)
library(brulee)

cores <- parallel::detectCores(logical = F)
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# Run on phase data
# Crowdsourced data, no mixed ppt
data_pre <- "../../data/"
df_train <- readRDS(paste0(data_pre, "cs_nomix_df_train.RDS"))
df_test <- readRDS(paste0(data_pre, "cs_nomix_df_test.RDS"))

# Make a recipe
df_recipe <- recipe(phase ~ ., data = df_train)

# Create some folds
df_folds <- vfold_cv(df_train, v = 10)

# Create a latin hypercube of the hyperparameter space
nn_grid <- grid_latin_hypercube(
  epochs(),
  hidden_units(),
  penalty(),
  learn_rate(),
  size = 4
)

# Specify the model
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









library(brulee)

# You also need to install the torch library
# torch::install_torch()

# Run on phase data
# Crowdsourced data, no mixed ppt
data_pre <- "../../data/"
df_train <- readRDS(paste0(data_pre, "cs_nomix_df_train.RDS"))
df_test <- readRDS(paste0(data_pre, "cs_nomix_df_test.RDS"))

# Make a recipe
df_recipe <- recipe(phase ~ ., data = df_train)

# Create some folds
df_folds <- vfold_cv(df_train, v = 10)

# Create a latin hypercube of the hyperparameter space
nn_grid <- grid_latin_hypercube(
  epochs(),
  hidden_units(),
  penalty(),
  learn_rate(),
  size = 4
)

# Specify the model
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


# Run again in parallel
library(doMC)
doMC::registerDoMC(cores = 4)

# Start timer
overall_time_start = Sys.time()

# hyperparameter tuning start time
param_time_start = Sys.time()

# Run the tuning grid
nn_tune_results <- nn_flow %>%
  tune_grid(resamples = df_folds, #CV object
            grid = nn_grid, # grid of values to try
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






# Create some folds
df_folds <- vfold_cv(df_train, v = 10)

# Create a latin hypercube of the hyperparameter space
nn_grid <- grid_latin_hypercube(
  epochs(),
  hidden_units(),
  penalty(),
  learn_rate(),
  size = 30
)

# Specify the model
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





# Split the data into train, validate, test
set.seed(321)
cls_train <- quadBoundaryFunc(2000) %>% select(A = X1, B = X2, class)
cls_val   <- quadBoundaryFunc( 500) %>% select(A = X1, B = X2, class)
cls_test  <- quadBoundaryFunc( 500) %>% select(A = X1, B = X2, class)

# Plot the data
ggplot(cls_train, aes(x = A, y = B, col = class)) + 
  geom_point(alpha = 1 / 4, cex = 3) + 
  coord_fixed()

# Recipe
biv_rec <- 
  recipe(class ~ ., data = cls_train) %>%
  step_normalize(all_predictors())

# Specify the model
nnet_spec <- 
  mlp(epochs = 1000, hidden_units = 10, penalty = 0.01, learn_rate = 0.1) %>% 
  set_engine("brulee", validation = 0) %>% 
  set_mode("classification")

# Create a workflow
nnet_wflow <- 
  biv_rec %>% 
  workflow(nnet_spec)

# Set random seed
set.seed(987)

# Start the timer
fit_start = Sys.time()

# Fit the model to the training data
nnet_fit <- fit(nnet_wflow, cls_train)

# End the timer
fit_end = Sys.time()
fit_time = fit_end - fit_start
fit_time

# Extract the fitted parameters
nnet_fit %>% extract_fit_engine()



# Run on phase data
# Crowdsourced data, no mixed ppt
data_pre <- "../../data/"
df_train <- readRDS(paste0(data_pre, "cs_nomix_df_train.RDS"))
df_test <- readRDS(paste0(data_pre, "cs_nomix_df_test.RDS"))

# Make a recipe
df_recipe <- recipe(phase ~ ., data = df_train)

# Create a workflow
nn_flow <- 
  df_recipe %>% 
  workflow(nnet_spec)

# Set random seed
set.seed(987)

# Start the timer
fit_start = Sys.time()

# Fit the model to the training data
nn_fit <- fit(nn_flow, df_train)

# End the timer
fit_end = Sys.time()
fit_time = fit_end - fit_start
fit_time

nn_fit %>% extract_fit_engine()

val_results <- 
  df_test %>%
  bind_cols(
    predict(nn_fit, new_data = df_test),
    predict(nn_fit, new_data = df_test, type = "prob")
  ) %>% 
  mutate(phase = as.factor(phase))

val_results %>% roc_auc(truth = phase, .pred_Rain)
val_results %>% accuracy(truth = phase, .pred_class)


# Create some folds
df_folds <- vfold_cv(df_train, v = 10)

# Create a latin hypercube of the hyperparameter space
nn_grid <- grid_latin_hypercube(
  epochs(),
  hidden_units(),
  penalty(),
  learn_rate(),
  size = 30
)

# Specify the model
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


# Now fit the data to the full training set

# Specify the model
nn_mod <- 
  mlp() %>% 
  set_engine("brulee", validation = 0) %>% 
  set_mode("classification") %>% 
  set_args(epochs = param_best$epochs,
           hidden_units = param_best$hidden_units,
           penalty = param_best$penalty,
           learn_rate = param_best$learn_rate)

# Update a workflow
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







# Try with parallel
library(doMC)
doMC::registerDoMC(cores = 6)



# Specify the model
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