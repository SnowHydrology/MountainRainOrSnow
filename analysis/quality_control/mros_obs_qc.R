# Script for quality controlling the citizen science observations 

# 1) Check nearby airports for precipitation
# 2) Check air temperature
# 3) Check relative humidity
# 4) Check average distance from temperature observations
# 5) Check distance from closest station
# 6) Check number of stations
# 7) Check p-value of variable lapse rate
# 8) Check whether timestamp is duplicate of report from same observer

# Keith Jennings
# 2021-07-26
# kjennings@lynker.com

# Load packages
library(tidyverse)
library(geosphere) # used for computing distances between points

########## User input

# Input files
citsci.input = "data/NOSHARE/mros_cit_sci_obs_processed_with_met_all_20220503_v2.RDS"
dist.input = "data/processed/tair_model_data_full_20220503_v2.RDS" # for met station distance
lcd.input = "data/NOSHARE/mros_met_lcd_20220503.RDS"
meta.input = "data/metadata/all_metadata_valid.csv"

# Output files
noshare.output = "data/NOSHARE/mros_cit_sci_obs_processed_with_tair_QCflags_20220503_v2.RDS"
share.output = "data/processed/mros_obs_processed_20220503.RDS"

# Import data
obs <- 
  readRDS(citsci.input)
dist <- 
  readRDS(dist.input) %>% 
  dplyr::select(id, avg_dist, nearest_dist, n_stations, lapse_var_pval)
obs <- left_join(obs, dist,
                 by = "id")

################################################################################
# Add LCD precipitation data to observations

# Import station metadata
lcd_meta <- read.csv(meta.input) %>% 
  filter(network == "lcd")

# Find the 5 nearest LCD sites to each obs point
stations_per_station <- data.frame()
for(i in seq_along(obs$id)){
  tmp.lonlat = c(obs[i, "longitude"], obs[i, "latitude"])
  tmp.id = obs[i, "id"]
  tmp.stations <- lcd_meta %>% 
    rowwise() %>% 
    mutate(dist = distHaversine(tmp.lonlat, c(lon, lat))) %>% 
    ungroup() %>% 
    slice_min(order_by = dist, n = 5) %>% 
    select(id_valid = id) %>% 
    mutate(id = tmp.id)
  stations_per_station <- bind_rows(stations_per_station,
                                    tmp.stations) 
}

# Import LCD data
lcd <- readRDS(lcd.input) %>% 
  dplyr::select(STATION:REPORT_TYPE, HourlyPrecipitation) %>% 
  filter(REPORT_TYPE == "FM-15")

# Convert any data points containing an "s" to NA
# These don't pass the stations QC checks
lcd <- lcd %>% 
  mutate(ppt = ifelse(HourlyPrecipitation == "T",
                      "0.001",
                      ifelse(grepl("s", HourlyPrecipitation),
                             NA,
                             HourlyPrecipitation))) %>%
  mutate(ppt = as.numeric(ppt),
         date = as.Date(substr(DATE, 1, 10)))

# Summarize by date
lcd_dly <- lcd %>% 
  group_by(id = STATION, date) %>% 
  summarise(ppt = sum(ppt, na.rm = T))
  
# Make dummy ppt column
obs$ppt <- NA

# Loop through the obs and extract daily lcd ppt data
for(i in 1:length(obs$id)){
  
  # Get the datetime
  date.tmp <- obs[i, "date"]
  
  # Get the valid stations
  stations.tmp <- filter(stations_per_station, id == obs[i, "id"])
  
  # Filter lcd data
  lcd.tmp <- lcd_dly %>% filter(date == date.tmp & 
                                  !is.na(ppt) &
                                  id %in% stations.tmp$id_valid)
  lcd_ppt.tmp = sum(lcd.tmp$ppt) / length(stations.tmp$id_valid)
  
  obs[i, "ppt"] = lcd_ppt.tmp
}

################################################################################
# Add data flags
tair_snow_max = 10 # max tair in °C for snow
tair_rain_min = -5 # min tair in °C for rain
ppt_thresh = 0
rh_thresh = 30
avgdist_thresh = 2e5 # maximum average distance (200 km)
closest_thresh = 3e4 # maximum nearest station distance
nstation_thresh = 5 # min number of stations within search radius
pval_thresh = 0.05 # maximum pval for lapse rate calc


obs <- obs %>% 
  mutate(tair_flag = case_when(tair >= tair_snow_max & phase == "Snow" ~ "WarmSnow",
                               tair <= tair_rain_min & phase == "Rain" ~ "CoolRain",
                               TRUE ~ "Pass"),
         ppt_flag = case_when(ppt == ppt_thresh ~ "NoPrecip",
                              TRUE ~ "Pass"),
         rh_flag = case_when(rh < rh_thresh ~ "LowRH",
                             TRUE ~ "Pass"),
         dist_flag = case_when(avg_dist >= avgdist_thresh ~ "TooFar",
                               TRUE ~ "Pass"),
         closest_flag = case_when(nearest_dist >= closest_thresh ~ "TooFar",
                                  TRUE ~ "Pass"),
         nstation_flag = case_when(n_stations < nstation_thresh ~ "FewStations",
                                  TRUE ~ "Pass"),
         pval_flag = case_when(lapse_var_pval > pval_thresh ~ "PoorLapse",
                                   TRUE ~ "Pass"))

################################################################################
# Check for duplicate time stamps

# Group by observer and datetime, then count with n() function
# Values > 1 mean one observer submitted multiple reports at same time
obs <- obs %>% 
  group_by(observer, utc_datetime) %>% 
  mutate(observer_count = n(),
         dupe_flag = case_when(observer_count > 1 ~ "Dupe",
                               TRUE ~ "Pass"),
         observer_count = NULL) %>% 
  ungroup()

################################################################################
# Export data

# Export complete data to NOSHARE
saveRDS(object = obs,
        file = noshare.output)

# Remove usernames and trim geolocation to two decimal places
# And export
obs %>%
  mutate(observer = NULL,
         latitude = round(latitude, digits = 2),
         longitude = round(longitude, digits = 2)) %>% 
  saveRDS(share.output)
  


