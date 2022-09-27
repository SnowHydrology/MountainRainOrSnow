# This script is set up to download a selection of:
# SNOTEL/SNOLITE and SCAN stations in the MRoS study area
# for WY2022

# Station metadata accessed from:
# https://wcc.sc.egov.usda.gov/nwcc/inventory

# To add more, select you network, element, and state,
# then click "View Inventory"
# For this script, I pasted the state inventories into a CSV

# Data can also be accessed through the SNOTEL data portal:
# https://wcc.sc.egov.usda.gov/reportGenerator/

# Keith Jennings
# kjennings@lynker.com
# 2021-07-22
# update 2022-05-09

# Load package
library(tidyverse)

# User input
export.file = "data/NOSHARE/mros_met_wcc_20220927.RDS"
# Create vector of water years to download (Oct. 1 through Sept. 30)
wyears <- 2022 # use c(WY1, WY2,...) for multiple wyears

# Import metadata
meta <- read_csv("data/metadata/wcc_station_metadata.csv") %>% 
  group_by(Id) %>% 
  mutate(id = str_split(Id, " ")[[1]][2])

# Identify SNOTEL sites of interest
sites <- meta$id

# Download strings
dl1 = "https://wcc.sc.egov.usda.gov/nwcc/view?intervalType=Historic+&report=ALL&timeseries=Hourly&format=copy&sitenum="
dl2 = "&year="
dl3 = "&month=WY"

# Make empty list for data
met <- data.frame()

# Loop by site and wyear
for(i in 1:length(sites)){
  
  # Make temporary list for downloaded data
  tmp.l <- list()
  
  # Loop through the water years
  for(j in 1:length(wyears)){
    
    # Download data into list
    tmp.l[[j]] <-
      read_csv(url(paste0(dl1,
                          sites[i],
                          dl2,
                          wyears[j],
                          dl3)),
               col_types = cols(.default = "c"),
               skip = 4)
  }
  
  # Bind data from temporary list into a single data frame
  tmp.df <- plyr::ldply(tmp.l, bind_rows)
  
  # Bind temporary data frame to snotel data frame
  met <- bind_rows(met, tmp.df)
  
  # Remove tmp files
  rm(tmp.l, tmp.df)
  
}

# Down select to just the temp, humidity data
met_select <- met %>% 
  select(`Site Id`:Time, matches('TOBS|TAVG|RHUM|DPTP')) %>% 
  mutate(across(4:16, as.numeric))

# This select results in the following vars (NOT IN ORDER):
# "Site Id","Date","Time" >> standard site, datetime info
# "TOBS.I-1 (degC)" >> instantaneous air temp
# "TOBS.I-2 (degC)" >> instantaneous air temp
# "TAVG.H-1 (degC)" >> average air temp for hour
# "TAVG.H-2 (degC)" >> average air temp for hour
# "RHUM.I-1 (pct)"  >> instantaneous relative humidity
# "RHUM.I-1 (per)"  >> instantaneous relative humidity
# "RHUMN.H-1 (pct)" >> minimum relative humidity for hour
# "RHUMN.H-1 (per)" >> minimum relative humidity for hour
# "RHUMV.H-1 (pct)" >> average relative humidity for hour
# "RHUMV.H-1 (per)" >> average relative humidity for hour
# "RHUMX.H-1 (pct)" >> maximum relative humidity for hour
# "RHUMX.H-1 (per)" >> maximum relative humidity for hour
# "DPTP.H-1 (degC)" >> instantaneous dew point temperature

# TOBS.I-2 can be ignored in favor of TOBS.I-1
# RHUM.I-1 (pct) and (per) need to be combined
# So combine them
met_select$rh <- ifelse(is.na(met_select$`RHUM.I-1 (pct)`),
                        met_select$`RHUM.I-1 (per)`,
                        met_select$`RHUM.I-1 (pct)`)


# Downselect and rename
met_select <- met_select %>% 
  select(id = `Site Id`, date = Date, time = Time,
         tair = `TOBS.I-1 (degC)`,
         tdew = `DPTP.H-1 (degC)`,
         rh)

# Convert -99.9 vals to NA
newNA <- function(x){ifelse(x == -99.9, NA, x)}
met_select <- met_select %>% 
  mutate(across(where(is.numeric), newNA))

# Save file
saveRDS(met_select,
        export.file)

# NOTE: SNOTEL can also be downloaded by state and sensor:
# User options
# State(s)
# snotel_url02_sta = c("NV", "CA")
# 
# # Start and end dates (YYYY-MM-DD)
# snotel_start_date = "2020-01-01"
# snotel_end_date   = "2021-07-20"
# snotel_url04_dts = paste(snotel_start_date,
#                          snotel_end_date,
#                          sep = ",")
# 
# # Build the URL
# snotel_url01_str = "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport,metric/hourly/start_of_period/state=%22"
# snotel_url03_str ="%22%20AND%20network=%22SNTL%22%20AND%20outServiceDate=%222100-01-01%22%7Cname/"
# snotel_url05_str = "/stationId,name,TOBS::value?fitToScreen=false"
# 
# # Loop through the options and download SNOTEL data
# for(i in seq_along(snotel_url02_sta)){
#   dest = paste0('data/met/snotel_',
#                 snotel_url02_sta[i],
#                 '.txt')
#   source = paste0(snotel_url01_str, snotel_url02_sta[i], snotel_url03_str,
#                   snotel_url04_dts, snotel_url05_str)
#   download.file(url = source, destfile = dest)
# }
