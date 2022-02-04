# Script for processing GPM data by elevation
# This uses data from Google Earth Engine

# Keith Jennings
# 2021-12-06

# Load packages
library(tidyverse)
library(lubridate)
library(cowplot); theme_set(theme_cowplot())
library(foreach) # for parallel processing
library(doMC); registerDoMC(cores = 4)

# Import processed GPM data
gpm <- read.csv("data/NOSHARE/gpm_imerg_pts_2020_2021.csv") %>% 
  rename(gpm_prob = probabilityLiquidPrecipitation)  %>% # renamed for easier typing
  select(-.geo, elev_m = tahoe_ros_)

# Format date information
gpm <- gpm %>% 
  mutate(datetime = as.POSIXct(str_sub(system.index, 1, 12),
                               format = "%Y%m%d%H%M",
                               tz = "UTC"))

# Remove sys index
gpm <- gpm %>% 
  select(-system.index)

# Get unique datetimes
datetimes <- unique(gpm$datetime)

# Compute 50% rain-snow elevation per scene
rain_snow_line.l <-
  foreach(i = 1:length(datetimes), .errorhandling = "pass") %dopar% {
    
    # Filter GPM to datetime
    tmp.df <- filter(gpm, datetime == datetimes[i])
    
    # Check that there are probabilities above and below 50%
    if(min(tmp.df$gpm_prob) >= 50){
      tmp.rs_line = NA
      tmp.note = "above"
    } else if(max(tmp.df$gpm_prob) < 50){
      tmp.rs_line = NA
      tmp.note = "below"
    } else {
      # Build linear model predicting prob by elev
      tmp.lm <- lm(gpm_prob ~ elev_m, tmp.df)
      tmp.slope = tmp.lm$coefficients[2] %>% as.numeric()
      tmp.intercept = tmp.lm$coefficients[1] %>% as.numeric()
      tmp.rs_line = (50 - tmp.intercept) / tmp.slope
      #tmp.r2 = summary(tmp.lm)$r.squared
      #tmp.pval = summary(tmp.lm)$coefficients[2,4] 
      tmp.note = "valid"
    }
    
    # Output into dataframe
    tmp.rain_snow_line <- data.frame(datetime = datetimes[i],
                                     rs_line = tmp.rs_line,
                                     note = tmp.note)
    
    # Print to list
    tmp.rain_snow_line
    
  }

# Bind list into dataframe
rain_snow_line <- plyr::ldply(rain_snow_line.l, bind_rows)

# Export data
saveRDS(object = rain_snow_line,
        file = "data/processed/gpm_rain_snow_line_2020_2021.RDS")
