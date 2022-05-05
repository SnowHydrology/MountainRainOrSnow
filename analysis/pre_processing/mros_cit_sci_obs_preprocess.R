# Script for processing raw Mountain Rain or Snow citizen science observations

# Keith Jennings
# kjennings@lynker.com
# 2021-07-23

# Load packages
library(tidyverse)
library(lubridate)
library(cowplot); theme_set(theme_cowplot())
library(sp)
library(rgdal)
library(raster)
library(lutz) # time zone calculations

################################################################################
########################  Data Import and Preparation  #########################
################################################################################
raw.file = "data/NOSHARE/mros_obs_raw_20220503.csv" # 2021-2022 data
# 2020-2021 data: "data/NOSHARE/Tahoe__Rain_or_Snow_Export_06012021.csv"
export.file = "data/NOSHARE/mros_obs_cit_sci_processed_20220503.RDS"
export.shape.dsn = "data/geospatial/"
export.shape.layer = "mros_obs_cit_sci_shp_20220503"

# Import the citizen science data 
obs <- read.csv(raw.file,
                stringsAsFactors = F) %>% 
  rename("phase" = "precipitation") %>% 
  mutate(timezone = tz_lookup_coords(lat = latitude, 
                                     lon = longitude, 
                                     method = "accurate")) %>% 
  split(.$timezone)

# Calculate time info
# Note this is more complex than just calculating a new column
# Why? R no likey multiple time zones in one column
# So we split out time zones into different list elements
# Then compute local time info, convert to UTC and join everything back together
obs <- lapply(obs, function(x){
  data.frame(x) %>% 
    mutate(datetime = with_tz(time = x$createdAt, 
                              tzone = x[[1, "timezone"]]),
           date = as.Date(datetime, tz = x[[1, "timezone"]]),
           year = year(date),
           month = month(date),
           day = day(date),
           hour = hour(datetime),
           minute = minute(datetime),
           second = second(datetime),
           day_of_week = weekdays(datetime),
           doy = yday(datetime),
           dowy = ifelse(year %% 4 == 0,
                         ifelse(month >= 10,
                                doy - 274,
                                doy + 92),
                         ifelse(month >= 10,
                                doy - 273,
                                doy + 92)),
           utc_datetime = with_tz(time = datetime, 
                                  tzone = "UTC"), 
           datetime = NULL)
})

# Bind all the data back together
obs <- plyr::ldply(obs, bind_rows)

# Add id
obs$id <- 1:length(obs$phase)

# Import ecoregion data
eco <- readOGR(dsn = "data/geospatial/", layer = "us_eco_l4_no_st_4326") %>% 
  spTransform(., CRS("+init=epsg:4326"))

# Import state data
states <- readOGR(dsn = "data/geospatial/", layer = "USA_adm1_WGS84") %>% 
  spTransform(., CRS("+init=epsg:4326"))

################################################################################
#########################  Data Joins and Extraction  ##########################
################################################################################

# Convert the data to spatial object
# Extract the relevant info for spatial conversion
coords <- dplyr::select(obs, longitude, latitude)
ids <- dplyr::select(obs, id)
crs_obs <- CRS("+init=epsg:4326")

# Convert obs to a SpatialPointsDataFrame
obs_sp <- SpatialPointsDataFrame(coords = coords, 
                                 data = ids, 
                                 proj4string = crs_obs)

# Use sp::over to perform spatial join of obs and eco & states data
obs <- bind_cols(obs, 
                 sp::over(obs_sp, eco),
                 sp::over(obs_sp, states))

################################################################################
#########################  Reformat and Export Data  ###########################
################################################################################

# Filter out data outside the US and without a valid phase
obs <- obs %>% 
  filter(!is.na(US_L3NAME) & phase != " - ")
obs_sp <- obs_sp[obs_sp$id %in% obs$id, ]

# Select and rename relevant data
obs <- obs %>% 
  dplyr::select(., id,
                datetime_local_obs = createdAt,
                datetime_local_sub = updatedAt,
                observer:utc_datetime,
                eco_l4 = US_L4NAME,
                eco_l3 = US_L3NAME,
                eco_l2 = NA_L2NAME,
                state = NAME_1)

# Export the preprocessed dataset
saveRDS(object = obs,
        file = export.file)

# Export the preprocessed shapefile
writeOGR(obs_sp, 
         dsn = export.shape.dsn, 
         layer = export.shape.layer, 
         driver = "ESRI Shapefile")
