# Script for rain-snow analysis of MRoS precipitation phase observations

# Keith Jennings
# kjennings@lynker.com
# 2021-08-02

# Load packages
library(tidyverse)
library(minpack.lm)

########## User input

# Input files
citsci.input = "data/processed/mros_obs_processed_20220503.RDS"

# Output files
output.file = "data/processed/mros_obs_rs_partitioning_20220503.RDS"
hyp_fit.output = "data/NOSHARE/mros_hyp_fit_20220503.RDS" # model objects for hyperbolic tangent
log_fit.output = "data/NOSHARE/mros_log_fit_20220503.RDS" # model objects for logistic regression

# Thresholds and bin width for summarizing snowfall probability
tair_max = 20
tair_min = -10
twet_max = 16
twet_min = -12
tdew_max = 12
tdew_min = -12
temp_bin_width = 1

# Threshold for n valid obs per grouping unit
obs_thresh = 300

# Threshold for obs per temp bin (fewer obs = greater noise)
obs_per_bin_thresh = 15

# Determine a temperature sequence for predicted data
temp_seq = seq(twet_min, tair_max, by = 0.5)

################################################################################
########################  Data Import and Preparation  #########################
################################################################################

# Import the data 
# And filter to just those passing the QC checks
obs <- readRDS(citsci.input)

# Compute the state and ecoregion number of valid obs
eco3 <- obs %>% group_by(eco_l3) %>% summarise(n = n()) %>% 
  filter(n > obs_thresh)
eco4 <- obs %>% group_by(eco_l4) %>% summarise(n = n()) %>% 
  filter(n > obs_thresh) %>% 
  filter(eco_l4 != "Sedimentary Subalpine Forests")
state <- obs %>% group_by(state) %>% summarise(n = n()) %>% 
  filter(n > obs_thresh)


################################################################################
#                         50% Snowfall Probability                             #
################################################################################

# Make a bin function for analysis
bin_fun <- function(temp, min, max, width){
  cut(temp, 
      breaks = seq(min, 
                   max, 
                   by = width))
}

# Make a numeric bin function for analysis
cuts2num_fun <- function(temp_bin, min, temp_bin_width){
  (((as.numeric(temp_bin) - 1) * temp_bin_width) + (min + (0.5 * temp_bin_width)))
}

# Add bins and numeric values to the data for tair, tdew, and twet
obs <- obs %>% 
  mutate(tair_bin = bin_fun(tair, tair_min, tair_max, temp_bin_width),
         tdew_bin = bin_fun(tdew, tdew_min, tdew_max, temp_bin_width),
         twet_bin = bin_fun(twet, twet_min, twet_max, temp_bin_width)) %>% 
  mutate(tair_bin_num = cuts2num_fun(tair_bin, tair_min, temp_bin_width),
         tdew_bin_num = cuts2num_fun(tdew_bin, tdew_min, temp_bin_width),
         twet_bin_num = cuts2num_fun(twet_bin, twet_min, temp_bin_width))

# Create a summary function
summary_fun <- function(df, temp, qc, group){
  if(temp == "tair") temp_group = quo(tair_bin_num)
  if(temp == "twet") temp_group = quo(twet_bin_num)
  if(temp == "tdew") temp_group = quo(tdew_bin_num)
  if(group == "eco_l3") group_loc = quo(eco_l3)
  if(group == "eco_l4") group_loc = quo(eco_l4)
  if(group == "state") group_loc = quo(state)
  df %>% 
    filter(., if(qc == "yes") tair_flag == "Pass" & ppt_flag == "Pass" & rh_flag == "Pass" &  
             dist_flag == "Pass" & closest_flag == "Pass" & nstation_flag == "Pass" & 
             dupe_flag == "Pass" else TRUE) %>% 
    group_by(temp_val = !!temp_group, name = !!group_loc) %>% 
    summarize(n_obs = n(), 
              snow_prob = (sum(phase == "Snow") / n_obs),
              rain_prob = (sum(phase == "Rain") / n_obs),
              mixed_prob = (sum(phase == "Mix") / n_obs),
              group = group,
              qc = qc,
              temp_type = temp)
}


# Create a prep function
prep_fun <- function(df, temp, qc, group){
  if(temp == "tair") temp_group = quo(tair)
  if(temp == "twet") temp_group = quo(twet)
  if(temp == "tdew") temp_group = quo(tdew)
  if(group == "eco_l3") group_loc = quo(eco_l3)
  if(group == "eco_l4") group_loc = quo(eco_l4)
  if(group == "state") group_loc = quo(state)
  df %>% 
    filter(., if(qc == "yes") tair_flag == "Pass" & ppt_flag == "Pass" & rh_flag == "Pass" &  
             dist_flag == "Pass" & closest_flag == "Pass" & nstation_flag == "Pass" & 
             dupe_flag == "Pass" else TRUE) %>% 
    select(temp_val = !!temp_group, name = !!group_loc, phase) %>% 
    mutate(snow_phase = ifelse(phase == "Snow", 0, 1))
}

# Functions for computing t50 based on a logistic regression and hyperbolic tangent
log_t50_fun <- function(log_fit){
  (log(1) - as.numeric(log_fit$coefficients[1])) / 
    as.numeric(log_fit$coefficients[2])
}
hyp_t50_fun <- function(nlsLMfit){
  (0.5 * log((1 + (0.5/as.numeric(coef(nlsLMfit)[1]) + as.numeric(coef(nlsLMfit)[4]))) / 
               (1 - (0.5/as.numeric(coef(nlsLMfit)[1]) + as.numeric(coef(nlsLMfit)[4])))))/
    as.numeric(coef(nlsLMfit)[2]) + 
    as.numeric(coef(nlsLMfit)[3])
}

# Create a function to predict snow probability from the binary logistic regression
# For the hyperbolic tangent, we can just use predict()
prob_binlog = function(logfit, temp){
  1/(1 + exp(as.numeric(logfit$coefficients[1]) + as.numeric(logfit$coefficients[2]) * temp ))
  }

# Loop through different groups and summarize
# Might be a better way to do this with map
qc_groups = c("yes", "no")
loc_groups = c("eco_l3", "eco_l4", "state")
temp_groups = c("tair", "tdew", "twet")
obs_summary <- data.frame()
t50 <- data.frame()
pred <- data.frame()
hyp_fit.l <- list() # for storing the hyperbolic tangent models
log_fit.l <-list() # for storing the logistic regression models
for(i in 1:length(temp_groups)){
  # Create and name the hyp_ and log_fit sublists
  hyp_fit.l[[i]] <- list()
  names(hyp_fit.l)[i] = temp_groups[i]
  log_fit.l[[i]] <- list()
  names(log_fit.l)[i] = temp_groups[i]
  
  for(j in 1:length(qc_groups)){
    # Create and name the hyp_ and log_fit sublists
    hyp_fit.l[[i]][[j]] <- list()
    names(hyp_fit.l[[i]])[j] = qc_groups[j]
    log_fit.l[[i]][[j]] <- list()
    names(log_fit.l[[i]])[j] = qc_groups[j]
    
    for(k in 1:length(loc_groups)){
      # Create and name the hyp_ and log_fit sublists
      hyp_fit.l[[i]][[j]][[k]] <- list()
      names(hyp_fit.l[[i]][[j]])[k] = loc_groups[k]
      log_fit.l[[i]][[j]][[k]] <- list()
      names(log_fit.l[[i]][[j]])[k] = loc_groups[k]
     
      # Get the valid locations based on the loc_group
      if(loc_groups[k] == "eco_l3") tmp.locs <- eco3$eco_l3
      if(loc_groups[k] == "eco_l4") tmp.locs <- eco4$eco_l4
      if(loc_groups[k] == "state") tmp.locs <- state$state
      
      # Run the summary function to get summarized data
      tmp <- summary_fun(obs, 
                         temp = temp_groups[i], 
                         qc = qc_groups[j], 
                         group = loc_groups[k])
      
      # Add to the data frame
      obs_summary <- bind_rows(obs_summary, tmp)
      
      # Fit the hyperbolic tangent to the summary data
      tmp.fit <-
        filter(tmp, name %in% tmp.locs) %>% 
        group_by(name) %>% 
        group_map(~ nlsLM(snow_prob ~ #predicts snow frequency
                            a * (tanh( b* (temp_val - c)) - d), #as a function of air temperature and 4 fitting parameters
                          #tanh is the hyperbolic tangent (gives curve shape)
                          data = ., 
                          start = list(a = -45, b = 0.7, c = 1.2, d = 1)))%>%
        setNames(unique(sort(tmp.locs)))
     
      # Add the hyperbolic tangent model objects to the list
      hyp_fit.l[[i]][[j]][[k]] <- tmp.fit
      
      # Run the prep function to get prepped data
      # And fit the logistic regression to the summary data
      tmp.fit <- prep_fun(obs, 
                          temp = temp_groups[i], 
                          qc = qc_groups[j], 
                          group = loc_groups[k]) %>% 
        filter(., name %in% tmp.locs) %>% 
        group_by(name) %>% 
        group_map(~ glm(snow_phase ~ temp_val,
                      family = binomial(link='logit'),
                      data = .))%>%
        setNames(unique(sort(tmp.locs)))
      
      # Add the logistic regression model objects to the list
      log_fit.l[[i]][[j]][[k]] <- tmp.fit
      
      # Compute t50 using the hyperbolic tangent
      tmp.t50 <- bind_rows(
        plyr::ldply(hyp_fit.l[[i]][[j]][[k]], hyp_t50_fun, .id = "name") %>% 
          mutate(method = "hyp") %>% rename(t50 = V1),
        plyr::ldply(log_fit.l[[i]][[j]][[k]], log_t50_fun, .id = "name") %>% 
          mutate(method = "log") %>% rename(t50 = V1)
      ) %>% 
        mutate(qc = qc_groups[j],
               temp_type = temp_groups[i],
               loc_group = loc_groups[k])
      t50 <- bind_rows(t50, tmp.t50)
      
      # Compute predicted data from the models
      tmp.pred <- bind_rows(
        lapply(hyp_fit.l[[i]][[j]][[k]], 
               as.data.frame(predict), newdata = data.frame(temp_val = temp_seq)) %>% 
          bind_rows(., .id = "name") %>% 
          mutate(method = "hyp", temp_val = rep(temp_seq, times = length(tmp.locs))) %>% 
          rename(snow_prob_pred = value),
        lapply(log_fit.l[[i]][[j]][[k]], as.data.frame(prob_binlog), temp = temp_seq) %>% 
          bind_rows(., .id = "name") %>% 
          mutate(method = "log", temp_val = rep(temp_seq, times = length(tmp.locs)))%>% 
          rename(snow_prob_pred = value)
      ) %>% 
        mutate(qc = qc_groups[j],
               temp_type = temp_groups[i],
               loc_group = loc_groups[k])
      pred <- bind_rows(pred, tmp.pred)
      
    }
  }
}




################################################################################
#################################  Export  #####################################
################################################################################

# Output the summarized, analyzed data
rs_partition <- list(temp50 = t50,            # thresholds
                     snow_prob = obs_summary, # binned probabilities
                     snow_pred = pred)        # predicted probabilities
saveRDS(object = rs_partition, 
        file = output.file)

# Output the model objects
saveRDS(object = hyp_fit.l,
        file = hyp_fit.output)
saveRDS(object = log_fit.l,
        file = log_fit.output)

