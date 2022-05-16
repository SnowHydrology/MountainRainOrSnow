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

########## User input

# Input files
met.input = "data/NOSHARE/mros_met_all_20220503.RDS"
meta.input = "data/metadata/all_metadata_valid.csv"
citsci.input = "data/NOSHARE/mros_obs_cit_sci_processed_20220503.RDS"
elev.input = "data/NOSHARE/mros_elev_3dep_pts_20220503.csv"

# Output files
citsci.output = "data/NOSHARE/mros_cit_sci_obs_processed_with_tair_20220503_v2.RDS"
model.output = "data/processed/tair_model_data_full_20220503_v2.RDS"
validation.output = "data/processed/tair_model_validation_20220503_v2.RDS"

  
# Other
met.search.radius = 100000 # search radius for met stations (m)
n.station.thresh = 5 # threshold for the number of met stations in search radius to perform temp modeling
n.remove = 50000 # number of random tair obs to remove for model validation
stations.remove <- c("WRSV1", "XONC1", "DPHC1", "DKKC2" ) # bad data

# Assign cores for parallelization
registerDoMC(cores = 4)

########## Import Data

# Import the air temperature data
# Remove extreme values
tair <- readRDS(file = met.input) %>% 
  mutate(tair = case_when(tair < -30 | tair > 45 ~ NA_real_,
                          TRUE ~ tair)) %>% 
  filter(!(id %in% stations.remove))

# Import the station metadata
meta <- read.csv(meta.input) %>% 
  mutate(lat = as.numeric(lat))

# Import the citizen science observations
# These were already pre-processed & filtered once
obs <- readRDS(citsci.input) %>% 
  mutate(phase = str_trim(phase)) %>% 
  left_join(.,
            select(read.csv(elev.input), id, elev = elevation),
            by = "id")

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
  foreach(i = seq_along(obs$id), .errorhandling = "pass") %dopar% {
    
    # Extract single phase observation
    # Plus other relevant data
    tmp.phase = obs[i, "phase"]
    tmp.lonlat = c(obs[i, "longitude"], obs[i, "latitude"])
    tmp.elev = obs[i, "elev"]
    tmp.datetime = obs[i, "utc_datetime"]
    
    # Extract all temperature data within ±1 h
    tmp.tair <- tair %>% filter(datetime < (tmp.datetime + 3600) &
                                  datetime > (tmp.datetime - 3600))
    
    # Join the station metadata
    tmp.tair <- left_join(tmp.tair,
                          meta, 
                          by = "id")
    
    # Calculate distance between obs and all stations
    # Filter to only stations within search radius
    tmp.tair <- tmp.tair %>% 
      rowwise() %>% 
      mutate(dist = distHaversine(tmp.lonlat, c(lon, lat))) %>% 
      ungroup() %>% 
      arrange(dist) %>% 
      filter(dist <= met.search.radius &
               !is.na(tair))
    
    # Compute the number of unique station ids within the search radius
    # with valid data
    valid.ids <- unique(tmp.tair$id)
    
    # Model air temp if valid.ids >= threshold
    if(length(valid.ids) >= n.station.thresh){
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
                  time_gap = mean(time_gap),
                  elev = mean(elev),
                  dist = mean(dist))
      
      # Remove outliers
      tmp.tair <- tmp.tair %>%
        filter(!(abs(tair - median(tair)) > 2*sd(tair)))

      # Compute lapse rate from all stations using linear regression
      tmp.lapse.fit <- lm(tair ~ elev, tmp.tair)
      tmp.lapse = tmp.lapse.fit$coefficients[2] %>% as.numeric()
      tmp.lapse.r2 = summary(tmp.lapse.fit)$r.squared
      tmp.lapse.pval = summary(tmp.lapse.fit)$coefficients[2,4] 
      tmp.n_stations = length(tmp.tair$id)
      tmp.avg_time = mean(tmp.tair$time_gap) %>% as.numeric()
      tmp.avg_dist = mean(tmp.tair$dist)
      
      # Get info on nearest station
      tmp.tair <- arrange(tmp.tair, dist)
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
                                  tair_avg_obs = mean(tmp.tair$tair, na.rm = T),
                                  tair_min_obs = min(tmp.tair$tair, na.rm = T),
                                  tair_max_obs = max(tmp.tair$tair, na.rm = T),
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
    }else{
      tmp.rain_snow <- data.frame(id = obs[i, "id"],
                                  tair_idw_lapse_const = NA,
                                  tair_idw_lapse_var = NA,
                                  tair_nearest_lapse_const = NA,
                                  tair_nearest_lapse_var = NA,
                                  tair_avg_obs = NA,
                                  tair_min_obs = NA,
                                  tair_max_obs = NA,
                                  lapse_var = NA,
                                  lapse_var_r2 = NA,
                                  lapse_var_pval = NA,
                                  n_stations = length(valid.ids),
                                  avg_time_gap = NA,
                                  avg_dist = NA,
                                  nearest_id = NA ,
                                  nearest_elev = NA,
                                  nearest_dist = NA,
                                  nearest_tair = NA)
    }
    
    # Output the data frame to store in list
    tmp.rain_snow
  }

# Bind into one dataframe
rain_snow <- plyr::ldply(rain_snow.l, bind_rows)


################################################################################
#                            Validate Tair Models                              #
################################################################################

# First filter tair to non-na values only
tair <- filter(tair, !is.na(tair) & !is.na(datetime))

# Determine which stations are within the search radius of each other
stations_per_station <- data.frame()
for(i in seq_along(meta$id)){
  tmp.lonlat = c(meta[i, "lon"], meta[i, "lat"])
  tmp.id = meta[i, "id"]
  tmp.stations <- meta %>% 
    rowwise() %>% 
    mutate(dist = distHaversine(tmp.lonlat, c(lon, lat))) %>% 
    ungroup() %>% 
    filter(dist <= met.search.radius) %>% 
    select(id_valid = id, dist = dist) %>% 
    mutate(id = tmp.id)
  stations_per_station <- bind_rows(stations_per_station,
                                    tmp.stations)
  meta[i, "n_stations"] = tmp.stations %>% 
    summarise(n_stations = n()) %>% 
    pull()
}

# Use set.seed so random numbers are reproducible
set.seed(1039)

# Extract n.remove observations from dataset
rows_to_remove = sample(1:length(tair$tair), n.remove, replace=F)
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
  
    # Compute distance from obs point to tair measurments
    tmp.tair <- left_join(tmp.tair,
                          select(filter(stations_per_station, 
                                 id == as.character(tmp[1, "id"])),
                                 id_valid, dist),
                          by = c("id" = "id_valid")) %>% 
      filter(dist <= met.search.radius) %>% 
      arrange(dist)
    
    # Compute lapse rate from all stations using linear regression
    tmp.lapse.fit <- lm(tair ~ elev, tmp.tair)
    tmp.lapse = tmp.lapse.fit$coefficients[2] %>% as.numeric()
    tmp.lapse.r2 = summary(tmp.lapse.fit)$r.squared
    tmp.lapse.pval = summary(tmp.lapse.fit)$coefficients[2,4] 
    tmp.n_stations = length(tmp.tair$id)
    tmp.avg_time = mean(tmp.tair$time_gap) %>% as.numeric()
    
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
#tair_validation <- plyr::ldply(tair_validation.l, bind_rows)
tair_validation <- data.frame()
for(i in 1:length(tair_validation.l)){
  if(length(tair_validation.l[[i]]) == 16){
    tair_validation <- bind_rows(tair_validation, tair_validation.l[[i]])
  }
}

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
                 dplyr::select(rain_snow, id, tair = tair_idw_lapse_var),
                 by = "id") 

# Export the observations with air temperature
saveRDS(object = obs,
        file = citsci.output) 

# Export full model data
saveRDS(object = rain_snow,
        file = model.output)

# Export the model validation data
saveRDS(object = tair_validation,
        file = validation.output)
