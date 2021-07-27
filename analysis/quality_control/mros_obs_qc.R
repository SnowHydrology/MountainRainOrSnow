# Script for quality controlling the citizen science observations 

# 1) Check nearby airports for precipitation
# 2) Check air temperature
# 3) Check distance from temperature observations

# PRECIP Y OR N
# AIR TEMPERATURE DATA
# DISTANCE FROM OBS

# Keith Jennings
# 2021-07-26
# kjennings@lynker.com

# Load packages
library(tidyverse)

# Import data
obs <- 
  readRDS("data/NOSHARE/mros_cit_sci_obs_processed_with_tair.RDS")

################################################################################
# Add airport precipitation data to observations

# Import airport data
airport <- read.csv("data/met/airp_data_all.csv") %>% 
  dplyr::select(STATION:REPORT_TYPE, DailyPrecipitation) %>% 
  mutate(REPORT_TYPE = str_trim(REPORT_TYPE)) %>% 
  filter(REPORT_TYPE == "SOD") %>% 
  mutate(DailyPrecipitation = case_when(DailyPrecipitation == "T" ~ 0.001,
                                        TRUE ~ as.numeric(as.character(DailyPrecipitation))),
         date = as.Date(substr(DATE, 1, 10)))

# Make dummy ppt column
obs$ppt_airport <- NA

# Loop through the obs and extract daily airport ppt data
for(i in 1:length(obs$id)){
  
  # Get the datetime
  date.tmp <- obs[i, "date"]
  
  # Filter airport data
  airport.tmp <- airport %>% filter(date == date.tmp & 
                                      !is.na(DailyPrecipitation))
  airport_ppt.tmp = sum(airport.tmp$DailyPrecipitation)
  
  obs[i, "ppt_airport"] = airport_ppt.tmp
}

################################################################################
# Add data flags
tair_snow_max = 7 # max tair in °C for snow
tair_rain_min = -1 # min tair in °C for rain
ppt_thresh = 0
dist_thresh = 1e5 # maximum average distance (100 km)

obs <- obs %>% 
  mutate(tair_flag = case_when(tair >= tair_snow_max & phase == "Snow" ~ "WarmSnow",
                               tair <= tair_rain_min & phase == "Rain" ~ "CoolRain",
                               TRUE ~ "Pass"),
         ppt_flag = case_when(ppt_airport == ppt_thresh ~ "NoPrecip",
                              TRUE ~ "Pass"),
         dist_flag = case_when(avg_dist >= dist_thresh ~ "TooFar",
                               TRUE ~ "Pass"))

################################################################################
# Check for duplicate time stamps

# Group by observer and datetime, then count with n() function
# Values > 1 mean one observer submitted multiple reports at same time
obs <- obs %>% 
  group_by(observer, datetime) %>% 
  mutate(observer_count = n(),
         dupe_flag = case_when(observer_count > 1 ~ "Dupe",
                               TRUE ~ "Pass"),
         observer_count = NULL) %>% 
  ungroup()

################################################################################
# Export data

# Export complete data to NOSHARE
saveRDS(object = obs,
        file = "data/NOSHARE/mros_cit_sci_obs_processed_with_tair_QCflags.RDS")

# Remove usernames and trim geolocation to two decimal places
# And export
obs %>%
  mutate(observer = NULL,
         latitude = round(latitude, digits = 2),
         longitude = round(longitude, digits = 2)) %>% 
  saveRDS("data/processed/mros_obs_processed_2020_2021.RDS")
  


