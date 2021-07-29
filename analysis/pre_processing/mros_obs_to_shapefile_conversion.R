# Convert obs data to shapefile

# Load packages
library(tidyverse)
library(sp)
library(rgdal)

# Import the processed observation data
obs <- readRDS("data/NOSHARE/mros_cit_sci_obs_processed_with_tair_QCflags.RDS") %>% 
  filter(tair_flag == "Pass" & 
           ppt_flag == "Pass" & 
           rh_flag == "Pass" &
           dist_flag == "Pass" & 
           dupe_flag == "Pass") %>% 
  dplyr::select(., longitude, latitude, id)

# Add CRS to raster stack
crs_obs <- CRS("+init=EPSG:4326")

# Convert to shapefile
obs_sp <- SpatialPointsDataFrame(coords = dplyr::select(obs, longitude, latitude), 
                                 data = dplyr::select(obs, id), 
                                 proj4string = crs_obs)

# Export
writeOGR(obs_sp, dsn = "data/NOSHARE", layer = "mros_obs_2020-2021_shapefile", 
         driver = "ESRI Shapefile")
