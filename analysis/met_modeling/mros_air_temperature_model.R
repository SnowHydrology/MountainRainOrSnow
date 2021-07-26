# Script for estimating air temperature at each citizen science observation

# Keith Jennings
# 2021-07-23
# kjennings@lynker.com

# Load packages
library(tidyverse)
library(geosphere) # used for computing distances between points
library(foreach) # for parallel processing
library(doMC)
library(minpack.lm)
library(sp)
library(raster)

# Assign cores for parallelization
registerDoMC(cores = 4)

# Import the air temperature data
# Remove extreme values
tair <- readRDS(file = "data/processed/met_all_NV_CA_2020_2021.RDS") %>% 
  mutate(tair = case_when(tair < -30 | tair > 45 ~ NA_real_,
                          TRUE ~ tair))

# Import the station metadata
meta <- read.csv("data/metadata/all_metadata_valid.csv")

# Import the citizen science observations
# These were already pre-processed & filtered once
obs <- readRDS("data/NOSHARE/mros_cit_sci_obs_processed.RDS") %>% 
  mutate(phase = str_trim(phase))

# Filter to just our ecoregions of interest
# We don't have air temperature for farther afield obs
eco_l3_include <- c("Central Basin and Range", "Sierra Nevada")
obs <- obs %>% 
  filter(., eco_l3 %in% eco_l3_include) 


#Spatial interpolations

#Just constant lapse from nearest station and time
#IDW plus constant lapse
#IDW plus lapse computed per timestep

#Define lapse constants
#Use constant lapse rate of -0.005 K/m from Girotto et al. (2014)
t_lapse_const = -0.005

################################################################################
##                      Model Tair for Each Obs                                #
################################################################################

# Loop through each observation
rain_snow.l <-
  foreach(i = seq_along(obs$phase), .errorhandling = "pass") %dopar% {
    
    # Extract single phase observation
    # Plus other relevant data
    tmp.phase = obs[i, "phase"]
    tmp.lonlat = c(obs[i, "longitude"], obs[i, "latitude"])
    tmp.elev = obs[i, "elev"]
    tmp.datetime = obs[i, "datetime"]
    
    # Extract all temperature data within ±1 h
    tmp.tair <- tair %>% filter(datetime < (tmp.datetime + 3600) &
                                  datetime > (tmp.datetime - 3600))
    
    # Calculate the time gap so only one observation from each station
    # Some obs will still have two tair values from one station
    # Take mean of obs
    tmp.tair <- tmp.tair %>% 
      filter(!is.na(tair)) %>% 
      mutate(time_gap = abs(datetime - tmp.datetime)) %>% 
      group_by(id) %>% 
      mutate(time_gap_min = min(time_gap)) %>% 
      filter(time_gap == min(time_gap)) %>% 
      group_by(id) %>% 
      summarise(tair = mean(tair), 
                time_gap = mean(time_gap))
    
    # Join the station metadata
    tmp.tair <- left_join(tmp.tair,
                          meta, 
                          by = "id")
    
    # Compute lapse rate from all stations using linear regression
    tmp.lapse.fit <- lm(tair ~ elev, tmp.tair)
    tmp.lapse = tmp.lapse.fit$coefficients[2] %>% as.numeric()
    tmp.lapse.r2 = summary(tmp.lapse.fit)$r.squared
    tmp.lapse.pval = summary(tmp.lapse.fit)$coefficients[2,4] 
    tmp.n_stations = length(tmp.tair$id)
    tmp.avg_time = mean(tmp.tair$time_gap) %>% as.numeric()
    
    # Compute distance from obs point to tair measurments
    tmp.tair <- tmp.tair %>% 
      rowwise() %>% 
      mutate(dist = distHaversine(tmp.lonlat, c(lon, lat))) %>% 
      ungroup() %>% 
      arrange(dist)
    
    # Calculate average distance
    tmp.avg_dist = mean(tmp.tair$dist)
    
    # Get info on nearest station
    tmp.nearest_id = tmp.tair[1, "id"] %>% pull()
    tmp.nearest_elev = tmp.tair[1, "elev"] %>% pull()
    tmp.nearest_dist = tmp.tair[1, "dist"] %>% pull()
    tmp.nearest_tair = tmp.tair[1, "tair"] %>% pull()
    
    # Compute the IDW weights
    tmp.tair <- tmp.tair %>% 
      mutate(weight_raw = 1/(dist^2),               #calculate raw weight (1/distance squared)
             weight_total = sum(weight_raw, na.rm = T),        #total weights
             weight_norm = weight_raw/weight_total)
    
    #Estimate with IDW and constant/variable lapse rates
    tmp.tair <- tmp.tair %>% 
      mutate(tair_sealevel_const = tair + (t_lapse_const * (0 - elev)),
             tair_sealevel_var = tair + (tmp.lapse * (0 - elev)))
    
    # Compute the air temperature at the observation point
    tmp.tair_idw_lapse_const = sum(tmp.tair$weight_norm * tmp.tair$tair_sealevel_const, na.rm = T) + 
      (t_lapse_const * tmp.elev)
    tmp.tair_idw_lapse_var = sum(tmp.tair$weight_norm * tmp.tair$tair_sealevel_var, na.rm = T) + 
      (tmp.lapse * tmp.elev)
    tmp.tair_nearest_site_const = (tmp.elev - tmp.nearest_elev) * t_lapse_const + tmp.nearest_tair
    tmp.tair_nearest_site_var = (tmp.elev - tmp.nearest_elev) * tmp.lapse + tmp.nearest_tair
    
    # Put everything into a single-row data frame
    tmp.rain_snow <- data.frame(id = obs[i, "id"],
                                tair_idw_lapse_const = tmp.tair_idw_lapse_const,
                                tair_idw_lapse_var = tmp.tair_idw_lapse_var,
                                tair_nearest_lapse_const = tmp.tair_nearest_site_const,
                                tair_nearest_lapse_var = tmp.tair_nearest_site_var,
                                lapse_var = tmp.lapse,
                                lapse_var_r2 = tmp.lapse.r2,
                                lapse_var_pval = tmp.lapse.pval,
                                n_stations = tmp.n_stations,
                                avg_time_gap = tmp.avg_time,
                                avg_dist = tmp.avg_dist,
                                nearest_id = tmp.nearest_id ,
                                nearest_elev = tmp.nearest_elev,
                                nearest_dist = tmp.nearest_dist,
                                nearest_tair = tmp.nearest_tair)
    
    # Output the data frame to store in list
    tmp.rain_snow
  }

# Bind all into a data frame
rain_snow <- plyr::ldply(rain_snow.l, bind_rows)


################################################################################
#                            Validate Tair Models                              #
################################################################################

# Use set.seed so random numbers are reproducible
set.seed(1039)

# Extract 5000 observations from dataset
rows_to_remove = sample(1:length(tair$tair), 5000, replace=F)
tair_test = tair[rows_to_remove, ]
tair_validate = tair[-rows_to_remove, ]

# Loop through the validation dataset
tair_validation.l <-
  foreach(i = seq_along(tair_test$tair), .errorhandling = "pass") %dopar% {
    
    #Filter to single row and join metadata
    tmp = tair_test[i,] %>% 
      left_join(., meta, by = "id")
    
    # Extract 
    tmp.datetime = tmp$datetime
    tmp.lonlat = c(tmp$lon, tmp$lat)
    tmp.elev = tmp$elev
    
    # Extract all temperature data within ±1 h
    # And remove obs from the station being tested
    tmp.tair <- tair_validate %>% filter(datetime < (tmp.datetime + 3600) &
                                           datetime > (tmp.datetime - 3600)) %>% 
      filter(id != tmp$id)
    
    # Calculate the time gap so only one observation from each station
    # Some obs will still have two tair values from one station
    # Take mean of obs
    tmp.tair <- tmp.tair %>% 
      filter(!is.na(tair)) %>% 
      mutate(time_gap = abs(datetime - tmp.datetime)) %>% 
      group_by(id) %>% 
      mutate(time_gap_min = min(time_gap)) %>% 
      filter(time_gap == min(time_gap)) %>% 
      group_by(id) %>% 
      summarise(tair = mean(tair), 
                time_gap = mean(time_gap))
    
    # Join the station metadata
    tmp.tair <- left_join(tmp.tair,
                          meta, 
                          by = "id")
    
    # Compute lapse rate from all stations using linear regression
    tmp.lapse.fit <- lm(tair ~ elev, tmp.tair)
    tmp.lapse = tmp.lapse.fit$coefficients[2] %>% as.numeric()
    tmp.lapse.r2 = summary(tmp.lapse.fit)$r.squared
    tmp.lapse.pval = summary(tmp.lapse.fit)$coefficients[2,4] 
    tmp.n_stations = length(tmp.tair$id)
    tmp.avg_time = mean(tmp.tair$time_gap) %>% as.numeric()
    
    # Compute distance from obs point to tair measurments
    tmp.tair <- tmp.tair %>% 
      rowwise() %>% 
      mutate(dist = distHaversine(tmp.lonlat, c(lon, lat))) %>% 
      ungroup() %>% 
      arrange(dist)
    
    # Calculate average distance
    tmp.avg_dist = mean(tmp.tair$dist)
    
    # Get info on nearest station
    tmp.nearest_id = tmp.tair[1, "id"] %>% pull()
    tmp.nearest_elev = tmp.tair[1, "elev"] %>% pull()
    tmp.nearest_dist = tmp.tair[1, "dist"] %>% pull()
    tmp.nearest_tair = tmp.tair[1, "tair"] %>% pull()
    
    # Compute the IDW weights
    tmp.tair <- tmp.tair %>% 
      mutate(weight_raw = 1/(dist^2),               #calculate raw weight (1/distance squared)
             weight_total = sum(weight_raw, na.rm = T),        #total weights
             weight_norm = weight_raw/weight_total)
    
    #Estimate with IDW and constant/variable lapse rates
    tmp.tair <- tmp.tair %>% 
      mutate(tair_sealevel_const = tair + (t_lapse_const * (0 - elev)),
             tair_sealevel_var = tair + (tmp.lapse * (0 - elev)))
    
    # Compute the air temperature at the observation point
    tmp.tair_idw_lapse_const = sum(tmp.tair$weight_norm * tmp.tair$tair_sealevel_const, na.rm = T) + 
      (t_lapse_const * tmp.elev)
    tmp.tair_idw_lapse_var = sum(tmp.tair$weight_norm * tmp.tair$tair_sealevel_var, na.rm = T) + 
      (tmp.lapse * tmp.elev)
    tmp.tair_nearest_site_const = (tmp.elev - tmp.nearest_elev) * t_lapse_const + tmp.nearest_tair
    tmp.tair_nearest_site_var = (tmp.elev - tmp.nearest_elev) * tmp.lapse + tmp.nearest_tair
    
    # Put everything into a single-row data frame
    tmp.tair_validation <- data.frame(id = tmp$id,
                                      tair = tmp$tair,
                                      tair_idw_lapse_const = tmp.tair_idw_lapse_const,
                                      tair_idw_lapse_var = tmp.tair_idw_lapse_var,
                                      tair_nearest_lapse_const = tmp.tair_nearest_site_const,
                                      tair_nearest_lapse_var = tmp.tair_nearest_site_var,
                                      lapse_var = tmp.lapse,
                                      lapse_var_r2 = tmp.lapse.r2,
                                      lapse_var_pval = tmp.lapse.pval,
                                      n_stations = tmp.n_stations,
                                      avg_time_gap = tmp.avg_time,
                                      avg_dist = tmp.avg_dist,
                                      nearest_id = tmp.nearest_id ,
                                      nearest_elev = tmp.nearest_elev,
                                      nearest_dist = tmp.nearest_dist,
                                      nearest_tair = tmp.nearest_tair)
    
    # Output the data frame to store in list
    tmp.tair_validation
  }

# Bind all into a data frame
tair_validation <- plyr::ldply(tair_validation.l, bind_rows)

# Summary stats
summary(lm(tair ~ tair_idw_lapse_const, tair_validation))
# r2 = 0.8714
summary(lm(tair ~ tair_idw_lapse_var, tair_validation))
# r2 = 0.8836
summary(lm(tair ~ tair_nearest_lapse_const, tair_validation))
# r2 = 0.8441
summary(lm(tair ~ tair_nearest_lapse_var, tair_validation))
# r2 = 0.8584
mean(tair_validation$tair_idw_lapse_const - tair_validation$tair, na.rm = T )
#[1] -0.02153004
mean(tair_validation$tair_idw_lapse_var - tair_validation$tair, na.rm = T )
#[1] 0.02517419
mean(tair_validation$tair_nearest_lapse_const - tair_validation$tair, na.rm = T )
#[1] -0.1117725
mean(tair_validation$tair_nearest_lapse_var - tair_validation$tair, na.rm = T )
#[1] -0.07254033


################################################################################
#                            Export the Datasets                               #
################################################################################

# Join the cit sci data with modeled air temperature
# Rename tair_idw_lapse_var to tair
# This is the air temp estimate we'll use moving forward
obs <- left_join(obs,
                 rain_snow,
                 by = "id") %>% 
  rename(tair = tair_idw_lapse_var)


# Export the observations with air temperature
saveRDS(object = obs,
        file = "data/NOSHARE/mros_cit_sci_obs_processed_with_tair.RDS") 

# Export the model validation data
saveRDS(object = tair_validation,
        file = "data/processed/tair_model_validation.RDS")
