# Script for importing DPR data and associating it with citizen science obs

# Load packages
library(raster)
library(sp)
library(tidyverse)
library(lubridate)
library(geosphere) # used for computing distances between points
library(rhdf5)

####################################
# User input
path2files <- "data/NOSHARE/dpr/" # Set data directory
input.file = "data/NOSHARE/mros_cit_sci_obs_processed_with_met_all.RDS"
output.file = "data/processed/mros_dpr_processed_2020.RDS"

# Import the processed observation data
obs <- readRDS(input.file)

# List dpr files
files <- data.frame(file = list.files(path = path2files, pattern = "h5$")) %>% 
  mutate(datetime = as.POSIXct(paste0(str_sub(file, 24, 31),
                                      str_sub(file, 34, 37)),
                               format = "%Y%m%d%H%M", tz = "UTC"))

# Assign a buffer, in hours, which will be used for matching cit sci obs
time_buff <- 1.5 # hours ± to consider

# Add start and end time
files <- files %>% 
  mutate(datetime_start = datetime - (3600 * time_buff),
         datetime_end = datetime + (3600 * time_buff))

# Add CRS for data processing
crs_obs <- CRS("+init=EPSG:4326")

# Make dummy dpr columns 
obs$dpr_hs_phase <- NA
obs$dpr_hs_dist  <- NA
obs$dpr_ms_phase <- NA
obs$dpr_ms_dist  <- NA
obs$dpr_ns_phase <- NA
obs$dpr_ns_dist  <- NA

# Loop through the obs and extract the DPR value at each point
for(i in 1:length(obs$id)){
  
  # Get the datetime
  datetime.tmp <- obs[i, "datetime"]
  
  # Make a single observation point
  coords.tmp <- obs %>% slice(i) %>% dplyr::select(longitude, latitude)
  ids.tmp <- obs %>% slice(i) %>% dplyr::select(id)
  obs_sp <- SpatialPointsDataFrame(coords = coords.tmp, 
                                   data = ids.tmp, 
                                   proj4string = crs_obs)
  
  # Find the DPR scene at that timestamp
  dpr_stack_position = which(datetime.tmp >= files$datetime_start &
                               datetime.tmp <= files$datetime_end)
  
  # If there is a GPM scene, then extract
  # If not set to NA
  # THis will help identify the missing GPM scenes
  # NASA's auto downloads no work good
  if(length(dpr_stack_position == 1)){
    # Get DPR scene
    f <- paste0(path2files, files[dpr_stack_position, "file"])
    
    ################################################################
    # Extract HS data
    # First phaseNearSurface, then lat-lon
    dpr.tmp <- as.integer(h5read(file = f, name = "/HS/SLV/phaseNearSurface"))
    lat <- h5read(file = f, name = "/HS/Latitude")
    lat[lat < 0] <- NA
    lon <- h5read(file = f, name = "/HS/Longitude")
    lon[lon < -200] <- NA
    
    # Convert data to points
    # and remove NAs
    pts.tmp <- data.frame(lon = as.vector(lon), 
                          lat = as.vector(lat),
                          dat = as.vector(dpr.tmp)) %>% 
      na.omit()
    
    # Compute all DPR to obs distances
    pts.tmp <- pts.tmp %>% 
      rowwise() %>% 
      mutate(dist = distHaversine(coords.tmp, c(lon, lat))) %>% 
      ungroup() %>% 
      arrange(dist)
    
    # Extract data from nearest DPR point to cit sci obcs
    tmp.nearest_dist = pts.tmp[1, "dist"] %>% pull()
    tmp.nearest_phase = pts.tmp[1, "dat"] %>% pull()
    
    # Add the DPR data
    obs[i, "dpr_hs_phase"] = tmp.nearest_phase
    obs[i, "dpr_hs_dist"]  = tmp.nearest_dist
    
    ################################################################
    # Extract MS data
    # First phaseNearSurface, then lat-lon
    dpr.tmp <- as.integer(h5read(file = f, name = "/MS/SLV/phaseNearSurface"))
    lat <- h5read(file = f, name = "/MS/Latitude")
    lat[lat < 0] <- NA
    lon <- h5read(file = f, name = "/MS/Longitude")
    lon[lon < -200] <- NA
    
    # Convert data to points
    # and remove NAs
    pts.tmp <- data.frame(lon = as.vector(lon), 
                          lat = as.vector(lat),
                          dat = as.vector(dpr.tmp)) %>% 
      na.omit()
    
    # Compute all DPR to obs distances
    pts.tmp <- pts.tmp %>% 
      rowwise() %>% 
      mutate(dist = distHaversine(coords.tmp, c(lon, lat))) %>% 
      ungroup() %>% 
      arrange(dist)
    
    # Extract data from nearest DPR point to cit sci obcs
    tmp.nearest_dist = pts.tmp[1, "dist"] %>% pull()
    tmp.nearest_phase = pts.tmp[1, "dat"] %>% pull()
    
    # Add the DPR data
    obs[i, "dpr_ms_phase"] = tmp.nearest_phase
    obs[i, "dpr_ms_dist"]  = tmp.nearest_dist
    
    ################################################################
    # Extract NS data
    # First phaseNearSurface, then lat-lon
    dpr.tmp <- as.integer(h5read(file = f, name = "/NS/SLV/phaseNearSurface"))
    lat <- h5read(file = f, name = "/NS/Latitude")
    lat[lat < 0] <- NA
    lon <- h5read(file = f, name = "/NS/Longitude")
    lon[lon < -200] <- NA
    
    # Convert data to points
    # and remove NAs
    pts.tmp <- data.frame(lon = as.vector(lon), 
                          lat = as.vector(lat),
                          dat = as.vector(dpr.tmp)) %>% 
      na.omit()
    
    # Compute all DPR to obs distances
    pts.tmp <- pts.tmp %>% 
      rowwise() %>% 
      mutate(dist = distHaversine(coords.tmp, c(lon, lat))) %>% 
      ungroup() %>% 
      arrange(dist)
    
    # Extract data from nearest DPR point to cit sci obcs
    tmp.nearest_dist = pts.tmp[1, "dist"] %>% pull()
    tmp.nearest_phase = pts.tmp[1, "dat"] %>% pull()
    
    # Add the DPR data
    obs[i, "dpr_ns_phase"] = tmp.nearest_phase
    obs[i, "dpr_ns_dist"]  = tmp.nearest_dist
    
  } else {
    # Set to NA
    obs[i, "dpr_hs_phase"] = NA
    obs[i, "dpr_hs_dist"]  = NA
    obs[i, "dpr_ms_phase"] = NA
    obs[i, "dpr_ms_dist"]  = NA
    obs[i, "dpr_ns_phase"] = NA
    obs[i, "dpr_ns_dist"]  = NA
    
  }
} # end

# Trim to just the rows and columns of valid data
obs_4export <- obs %>% 
  filter(!is.na(dpr_hs_phase), !is.na(dpr_ms_phase), !is.na(dpr_ns_phase)) %>% 
  select(id, dpr_hs_phase:dpr_ns_dist)

# Export the data
saveRDS(object = obs_4export, 
        file = output.file)
