# Script for estimating relative humidity, dew point temperature, and wet 
# bulb temperature at each citizen science observation

# Keith Jennings
# 2021-07-28
# kjennings@lynker.com

# Load packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())
library(geosphere) # used for computing distances between points
library(foreach) # for parallel processing
library(doMC)
library(minpack.lm)
library(sp)
library(humidity) # devtools::install_github("SnowHydrology/humidity")

########## User input

# Input files
met.input = "data/NOSHARE/mros_met_all_20220503.RDS"
meta.input = "data/metadata/all_metadata_valid.csv"
citsci.input = "data/NOSHARE/mros_cit_sci_obs_processed_with_tair_20220503_v2.RDS"
elev.input = "data/NOSHARE/mros_elev_3dep_pts_20220503.csv"

# Output files
citsci.output = "data/NOSHARE/mros_cit_sci_obs_processed_with_met_all_20220503_v2.RDS"
model.output = "data/processed/tdew_model_data_full_20220503_v2.RDS"
validation.output = "data/processed/tdew_model_validation_20220503.RDS"
all.validation.output = "data/NOSHARE/met_equation_validation_20220503.RDS"

# Other
met.search.radius = 100000 # search radius for met stations (m)
n.station.thresh = 5 # threshold for the number of met stations in search radius to perform temp modeling
n.remove = 50000 # number of random tair obs to remove for model validation
stations.remove <- c("WRSV1", "XONC1", "DPHC1", "DKKC2" ) # bad data

# Assign cores for parallelization
registerDoMC(cores = 4)

############### Data import

# Import the met data
# Remove extreme values
# No current TWET or TDEW extreme values (airport data preprocessed by NCDC?)
met <- readRDS(file = met.input) %>% 
  filter(!(id %in% stations.remove)) %>% 
  mutate(tair = case_when(tair < -30 | tair > 45 ~ NA_real_,
                          TRUE ~ tair),
         tdew = case_when(tdew < -40 | tdew > 45 ~ NA_real_,
                          TRUE ~ tdew),
         twet = case_when(twet < -40 | twet > 45 ~ NA_real_,
                          TRUE ~ twet),
         rh = case_when(rh < 5 | rh > 100 ~ NA_real_,
                        TRUE ~ rh))

# Import the station metadata
meta <- read.csv(meta.input) %>% 
  mutate(lat = as.numeric(lat))

# Import the citizen science observations
# These were already pre-processed & filtered once
obs <- readRDS(citsci.input) %>% 
  mutate(phase = str_trim(phase)) 

################################################################################
#                Use RH and TAIR to compute TWET and TDEW                      #
################################################################################

# Compute TWET and TDEW when RH and TAIR exist
met <- met %>% 
  mutate(twet_est = wetbulb(tair, rh),
         tdew_est = dewpoint(tair, rh),
         rh_est   = relhum(tair, tdew))

# Validate TWET estimates at stations with TWET obs
twet_validation <- met %>% 
  filter(!is.na(twet))
summary(lm(twet ~ twet_est, twet_validation)) # r2 = 0.9972
mean(twet_validation$twet_est - twet_validation$twet, na.rm = T) # bias = -0.179°C

# # Plot
# ggplot(twet_validation, aes(twet, twet_est)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1, color = "red") + 
#   labs(x = expression("Observed Wet Bulb Temperature ("*degree*C*")"),
#        y = expression("Estimated Wet Bulb Temperature ("*degree*C*")")) +
#   annotate(geom = "text", x = 0, y = -2, label = "1:1 line", color = "red", hjust = 0)

# Validate TDEW estimates at stations with TDEW obs
tdew_validation <- met %>% 
  filter(!is.na(tdew))
summary(lm(tdew ~ tdew_est, tdew_validation)) # r2 = 0.9979
mean(tdew_validation$tdew_est - tdew_validation$tdew, na.rm = T) # bias = -0.041°C

# # Plot
# ggplot(tdew_validation, aes(tdew, tdew_est)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1, color = "red") + 
#   labs(x = expression("Observed Dew Point Temperature ("*degree*C*")"),
#        y = expression("Estimated Dew Point Temperature ("*degree*C*")")) +
#   annotate(geom = "text", x = 0, y = -2, label = "1:1 line", color = "red", hjust = 0)

# Validate RH estimates at stations with RH obs
rh_validation <- met %>% 
  filter(!is.na(rh))
summary(lm(rh ~ rh_est, rh_validation)) # r2 = 0.9988
mean(rh_validation$rh_est - rh_validation$rh, na.rm = T) # bias = -0.189%

# # Plot
# ggplot(rh_validation, aes(rh, rh_est)) + 
#   geom_point() +
#   geom_abline(intercept = 0, slope = 1, color = "red") + 
#   labs(x = "Observed Relative Humidity (%)",
#        y = "Estimated Relative Humidity (%)") +
#   annotate(geom = "text", x = 0, y = -2, label = "1:1 line", color = "red", hjust = 0)


################################################################################
##                  Model RH, TWET, and TDEW for Each Obs                      #           #
################################################################################

# Loop through each observation
rain_snow.l <-
  foreach(i = seq_along(obs$phase), .errorhandling = "pass") %dopar% {
    
    # Extract single phase observation
    # Plus other relevant data
    tmp.phase = obs[i, "phase"]
    tmp.lonlat = c(obs[i, "longitude"], obs[i, "latitude"])
    tmp.elev = obs[i, "elev"]
    tmp.datetime = obs[i, "utc_datetime"]
    tmp.tair = obs[i, "tair"] # already modeled in mros_air_temperature_model.R
    
    # Extract all met data within ±1 h
    tmp.met <- met %>% filter(datetime < (tmp.datetime + 3600) &
                                  datetime > (tmp.datetime - 3600))
    
   
    # Filter to only stations with tdew obs or est
    tmp.met <- tmp.met %>% 
      filter(!is.na(tdew) | !is.na(tdew_est))
    
    # Calculate distance for each unique station combo
    tmp.meta <- meta %>% 
      filter(id %in% unique(tmp.met$id)) %>% 
      rowwise() %>% 
      mutate(dist = distHaversine(tmp.lonlat, c(lon, lat))) %>% 
      ungroup() %>% 
      filter(dist <= met.search.radius)
    
    # Compute the number of unique station ids within the search radius
    # with valid data
    valid.ids <- unique(tmp.meta$id)
    
    # Model met if valid.ids >= threshold
    if(length(valid.ids) >= n.station.thresh){
      # Join the station metadata
      tmp.met <- left_join(filter(tmp.met, id %in% tmp.meta$id),
                           tmp.meta, 
                           by = "id")
  
      # Calculate the time gap so only one observation from each station
      # Some obs will still have two values from one station
      # Take mean of obs
      tmp.met <- tmp.met %>% 
        mutate(tdew = case_when(!is.na(tdew) ~ tdew,   # Use observed TDEW when it exists
                                TRUE ~ tdew_est)) %>% 
        mutate(time_gap = abs(datetime - tmp.datetime)) %>% 
        group_by(id) %>% 
        mutate(time_gap_min = min(time_gap)) %>% 
        filter(time_gap == min(time_gap)) %>% 
        group_by(id) %>% 
        summarise(tdew = mean(tdew), 
                  time_gap = mean(time_gap),
                  elev = mean(elev),
                  dist = mean(dist)) %>% 
        arrange(dist)
      
      # Remove outliers
      tmp.met <- tmp.met %>%
        filter(!(abs(tdew - median(tdew)) > 2*sd(tdew)))
      
      # Compute lapse rate from all stations using linear regression
      tmp.lapse.fit <- lm(tdew ~ elev, tmp.met)
      tmp.lapse = tmp.lapse.fit$coefficients[2] %>% as.numeric()
      tmp.lapse.r2 = summary(tmp.lapse.fit)$r.squared
      tmp.lapse.pval = summary(tmp.lapse.fit)$coefficients[2,4] 
      tmp.n_stations = length(tmp.met$id)
      tmp.avg_time = mean(tmp.met$time_gap) %>% as.numeric()
      
      # Calculate average distance
      tmp.avg_dist = mean(tmp.met$dist)
      
      # Get info on nearest station
      tmp.nearest_id = tmp.met[1, "id"] %>% pull()
      tmp.nearest_elev = tmp.met[1, "elev"] %>% pull()
      tmp.nearest_dist = tmp.met[1, "dist"] %>% pull()
      tmp.nearest_tdew = tmp.met[1, "tdew"] %>% pull()
      
      # Compute the IDW weights
      tmp.met <- tmp.met %>% 
        mutate(weight_raw = 1/(dist^2),               #calculate raw weight (1/distance squared)
               weight_total = sum(weight_raw, na.rm = T),        #total weights
               weight_norm = weight_raw/weight_total)
      
      #Estimate with IDW and constant/variable lapse rates
      tmp.met <- tmp.met %>% 
        mutate(tdew_sealevel_var = tdew + (tmp.lapse * (0 - elev)))
      
      # Compute the air temperature at the observation point
      tmp.tdew_idw_lapse_var = sum(tmp.met$weight_norm * tmp.met$tdew_sealevel_var, na.rm = T) + 
        (tmp.lapse * tmp.elev)
      tmp.tdew_nearest_site_var = (tmp.elev - tmp.nearest_elev) * tmp.lapse + tmp.nearest_tdew
      
      # Put everything into a single-row data frame
      tmp.rain_snow <- data.frame(id = obs[i, "id"],
                                  tdew_idw_lapse_var = tmp.tdew_idw_lapse_var,
                                  tdew_nearest_lapse_var = tmp.tdew_nearest_site_var,
                                  tdew_avg_obs = mean(tmp.met$tdew, na.rm = T),
                                  tdew_min_obs = min(tmp.met$tdew, na.rm = T),
                                  tdew_max_obs = max(tmp.met$tdew, na.rm = T),
                                  lapse_var = tmp.lapse,
                                  lapse_var_r2 = tmp.lapse.r2,
                                  lapse_var_pval = tmp.lapse.pval,
                                  n_stations = tmp.n_stations,
                                  avg_time_gap = tmp.avg_time,
                                  avg_dist = tmp.avg_dist,
                                  nearest_id = tmp.nearest_id ,
                                  nearest_elev = tmp.nearest_elev,
                                  nearest_dist = tmp.nearest_dist,
                                  nearest_tdew = tmp.nearest_tdew)
    }else{
      tmp.rain_snow <- data.frame(id = obs[i, "id"],
                                  tdew_idw_lapse_var = NA,
                                  tdew_nearest_lapse_var = NA,
                                  tdew_avg_obs = NA,
                                  tdew_min_obs = NA,
                                  tdew_max_obs = NA,
                                  lapse_var = NA,
                                  lapse_var_r2 = NA,
                                  lapse_var_pval = NA,
                                  n_stations = length(valid.ids),
                                  avg_time_gap = NA,
                                  avg_dist = NA,
                                  nearest_id = NA ,
                                  nearest_elev = NA,
                                  nearest_dist = NA,
                                  nearest_tdew = NA)
    }
    
    # Output the data frame to store in list
    tmp.rain_snow
  }

# Bind all into a data frame
rain_snow <- plyr::ldply(rain_snow.l, bind_rows)

################################################################################
#                            Validate TDEW Models                              #
################################################################################

# Use set.seed so random numbers are reproducible
set.seed(1039)

# Extract 5000 observations from dataset
rows_to_remove = sample(1:length(tdew_validation$tdew), 5000, replace=F)
tdew_test = tdew_validation[rows_to_remove, ]
tdew_validate = tdew_validation[-rows_to_remove, ]

# Loop through the validation dataset
tdew_distribution_validation.l <-
  foreach(i = seq_along(tdew_test$tdew), .errorhandling = "pass") %dopar% {
    
    #Filter to single row and join metadata
    tmp = tdew_test[i,] %>% 
      left_join(., meta, by = "id")
    
    # Extract 
    tmp.datetime = tmp$datetime
    tmp.lonlat = c(tmp$lon, tmp$lat)
    tmp.elev = tmp$elev
    
    # Extract all temperature data within ±1 h
    # And remove obs from the station being tested
    tmp.tdew <- tdew_validate %>% filter(datetime < (tmp.datetime + 3600) &
                                           datetime > (tmp.datetime - 3600)) %>% 
      filter(id != tmp$id)
    
    # Calculate the time gap so only one observation from each station
    # Some obs will still have two tdew values from one station
    # Take mean of obs
    tmp.tdew <- tmp.tdew %>% 
      filter(!is.na(tdew)) %>% 
      mutate(time_gap = abs(datetime - tmp.datetime)) %>% 
      group_by(id) %>% 
      mutate(time_gap_min = min(time_gap)) %>% 
      filter(time_gap == min(time_gap)) %>% 
      group_by(id) %>% 
      summarise(tdew = mean(tdew), 
                time_gap = mean(time_gap))
    
    # Join the station metadata
    tmp.tdew <- left_join(tmp.tdew,
                          meta, 
                          by = "id")
    
    # Compute lapse rate from all stations using linear regression
    tmp.lapse.fit <- lm(tdew ~ elev, tmp.tdew)
    tmp.lapse = tmp.lapse.fit$coefficients[2] %>% as.numeric()
    tmp.lapse.r2 = summary(tmp.lapse.fit)$r.squared
    tmp.lapse.pval = summary(tmp.lapse.fit)$coefficients[2,4] 
    tmp.n_stations = length(tmp.tdew$id)
    tmp.avg_time = mean(tmp.tdew$time_gap) %>% as.numeric()
    
    # Compute distance from obs point to tdew measurments
    tmp.tdew <- tmp.tdew %>% 
      rowwise() %>% 
      mutate(dist = distHaversine(tmp.lonlat, c(lon, lat))) %>% 
      ungroup() %>% 
      arrange(dist)
    
    # Calculate average distance
    tmp.avg_dist = mean(tmp.tdew$dist)
    
    # Get info on nearest station
    tmp.nearest_id = tmp.tdew[1, "id"] %>% pull()
    tmp.nearest_elev = tmp.tdew[1, "elev"] %>% pull()
    tmp.nearest_dist = tmp.tdew[1, "dist"] %>% pull()
    tmp.nearest_tdew = tmp.tdew[1, "tdew"] %>% pull()
    
    # Compute the IDW weights
    tmp.tdew <- tmp.tdew %>% 
      mutate(weight_raw = 1/(dist^2),               #calculate raw weight (1/distance squared)
             weight_total = sum(weight_raw, na.rm = T),        #total weights
             weight_norm = weight_raw/weight_total)
    
    #Estimate with IDW and variable lapse rate
    tmp.tdew <- tmp.tdew %>% 
      mutate(tdew_sealevel_var = tdew + (tmp.lapse * (0 - elev)))
    
    # Compute the air temperature at the observation point
    tmp.tdew_idw_lapse_var = sum(tmp.tdew$weight_norm * tmp.tdew$tdew_sealevel_var, na.rm = T) + 
      (tmp.lapse * tmp.elev)
    tmp.tdew_nearest_site_var = (tmp.elev - tmp.nearest_elev) * tmp.lapse + tmp.nearest_tdew
    
    # Put everything into a single-row data frame
    tmp.tdew_validation <- data.frame(id = tmp$id,
                                      tdew = tmp$tdew,
                                      tdew_idw_lapse_var = tmp.tdew_idw_lapse_var,
                                      tdew_nearest_lapse_var = tmp.tdew_nearest_site_var,
                                      lapse_var = tmp.lapse,
                                      lapse_var_r2 = tmp.lapse.r2,
                                      lapse_var_pval = tmp.lapse.pval,
                                      n_stations = tmp.n_stations,
                                      avg_time_gap = tmp.avg_time,
                                      avg_dist = tmp.avg_dist,
                                      nearest_id = tmp.nearest_id ,
                                      nearest_elev = tmp.nearest_elev,
                                      nearest_dist = tmp.nearest_dist,
                                      nearest_tdew = tmp.nearest_tdew)
    
    # Output the data frame to store in list
    tmp.tdew_validation
  }

# Bind all into a data frame
tdew_distribution_validation <- plyr::ldply(tdew_distribution_validation.l, bind_rows)

# Summary stats
summary(lm(tdew ~ tdew_idw_lapse_var, tdew_distribution_validation))
# r2 = 0.8139 
summary(lm(tdew ~ tdew_nearest_lapse_var, tdew_distribution_validation))
# r2 = 0.7782

mean(tdew_distribution_validation$tdew_idw_lapse_var - 
       tdew_distribution_validation$tdew, na.rm = T )
#[1] 0.3663961

mean(tdew_distribution_validation$tdew_nearest_lapse_var -
       tdew_distribution_validation$tdew, na.rm = T )
#[1] 0.6610014

################################################################################
#                            Compute RH and TWET                               #
################################################################################

# Join tair data from obs to rain_snow
rain_snow <- left_join(rain_snow,
                       dplyr::select(obs, id, tair),
                       by = "id")

# Calculate RH and TWET using tdew_idw_lapse_var
rain_snow <- rain_snow %>% 
  mutate(tdew = case_when(tdew_idw_lapse_var > tair ~ tair,
                          TRUE ~ tdew_idw_lapse_var),
           rh = relhum(tair, tdew_idw_lapse_var),
         twet = wetbulb(tair, rh)) %>% 
  mutate(rh = case_when(rh > 100 ~ 100,
                        TRUE ~ rh))


################################################################################
#                            Export the Datasets                               #
################################################################################

# Join the cit sci data with modeled TDEW, TWET, and RH
# Rename tdew_idw_lapse_var to tdew
# This is the dew point temp estimate we'll use moving forward
obs <- left_join(obs,
                 dplyr::select(rain_snow, id, twet, tdew = tdew_idw_lapse_var, rh),
                 by = "id") 

# Export the observations with all met
saveRDS(object = obs,
        file = citsci.output) 

# Export full model data
saveRDS(object = rain_snow,
        file = model.output)

# Export the model validation data
saveRDS(object = tdew_distribution_validation,
        file = "data/processed/tdew_model_validation.RDS")

# Bind the equation validation data into a single file
all_validation <- bind_rows(tdew_validation,
                            twet_validation,
                            rh_validation) %>% 
  distinct() # unique rows only

# Export
saveRDS(object = all_validation,
        file = all.validation.output)
