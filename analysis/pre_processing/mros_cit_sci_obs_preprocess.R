# Script for processing raw Mountain Rain or Snow citizen science observations

# Keith Jennings
# kjennings@lynker.com
# 2021-07-23

# Load packages
library(tidyverse)
library(cowplot); theme_set(theme_cowplot())
library(sp)
library(rgdal)
library(raster)

################################################################################
########################  Data Import and Preparation  #########################
################################################################################

# Import the citizen science data 
# Add some date and time info
obs <- read.csv("data/NOSHARE/Tahoe__Rain_or_Snow_Export_06012021.csv",
                stringsAsFactors = F) %>% 
  rename("phase" = "What_is_falling_from_the_sky_right_now._") %>% 
  mutate(datetime = as.POSIXct(createdAt, 
                               format = "%m/%d/%Y %H:%M",
                               tz = "Etc/GMT+8"),
         date = as.Date(datetime, tz = "Etc/GMT+8"),
         hour = as.numeric(format(datetime, "%H")),
         time = format(datetime, "%H:%M"),
         day_of_week = weekdays(datetime))

# Filter center-of-Tahoe obs
# obs w/ no lat or lon
# obs before 2020
obs <- obs %>% 
  filter(., latitude != 39.096800000000002 & 
           longitude != -120.032399999999996) %>% 
  filter(., !is.na(latitude) | !is.na(longitude)) %>% 
  filter(., date >= "2020-01-01")

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

# Add elevation data to obs based on location
# Note: velox package is faster for extraction but time spent 
# converting to velox object eliminates extraction gains here
obs$elev <- raster("data/geospatial/elev_westUS_90m.tif") %>% 
  extract(., obs_sp)

# Use sp::over to perform spatial join of obs and eco & states data
obs <- bind_cols(obs, 
                 sp::over(obs_sp, eco),
                 sp::over(obs_sp, states))

################################################################################
#########################  Reformat and Export Data  ###########################
################################################################################

# Select and rename relevant data
obs <- obs %>% 
  dplyr::select(., observer, latitude:elev,
                eco_l4 = US_L4NAME,
                eco_l3 = US_L3NAME,
                eco_l2 = NA_L2NAME,
                state = NAME_1)

# Export
saveRDS(object = obs,
        file = "data/NOSHARE/mros_cit_sci_obs_processed.RDS")
