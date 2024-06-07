# Script to extract Jennings et al. 2018 spatially variable, optimized
# rain-snow temperature threshold for each observation

# Keith Jennings
# kjennings@lynker.com

# Load packages
library(tidyverse)
library(terra)

# Data prefix and file names
# Note: data are local, so prefix needs to be changed if files are moved
data_pre = "../../data/"
nh_locs_file = "jennings_et_al_2018_file1_station_locs_elev.csv"
nh_obs_file = "jennings_et_al_2018_file2_ppt_phase_met_observations.csv"
cs_locs_file = "mros_QAQCflags_2024_01_24.csv"
thresh_binlog_file = "jennings_et_al_2018_file4_temp50_raster.tif"
thresh_linreg_file = "jennings_et_al_2018_file5_temp50_linregr_raster.tif"
export_file = "nh_cs_thresh_ta_sv"

# Import location data
nh_locs <- read.csv(paste0(data_pre, nh_locs_file)) %>% 
  select(Station_ID, Latitude, Longitude)
cs_locs <- read.csv(paste0(data_pre, cs_locs_file)) %>% 
  select(all.id, latitude, longitude)

# Convert locations to SpatVectors
nh_locs_sv <- vect(nh_locs, geom=c("Longitude", "Latitude") )
cs_locs_sv <- vect(cs_locs, geom=c("longitude", "latitude") )

# Import threshold maps
thresh_binlog <- rast(paste0(data_pre, thresh_binlog_file))
thresh_linreg <- rast(paste0(data_pre, thresh_linreg_file))

# Merge threshold maps
thresh_all <- terra::merge(thresh_binlog, thresh_linreg)

# Extract 
nh_thresh <- terra::extract(thresh_all, nh_locs_sv, bind = T) %>% 
  as.data.frame() %>% 
  rename(thresh_ta_sv = jennings_et_al_2018_file4_temp50_raster)
cs_thresh <- terra::extract(thresh_all, cs_locs_sv, bind = T) %>% 
  as.data.frame() %>% 
  rename(thresh_ta_sv = jennings_et_al_2018_file4_temp50_raster)

# Export
saveRDS(object = list(nh_thresh = nh_thresh,
                      cs_thresh = cs_thresh), 
        file = paste0(data_pre, export_file))
