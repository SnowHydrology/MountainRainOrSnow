# This script is currently (2021-07-22) set up to download a selection of:
# SNOTEL stations in Nevada and California near Lake Tahoe
# For water years 2020 and 2021

# The addition of more states and stations will require identification
# on the SNOTEL site:
# https://www.nrcs.usda.gov/wps/portal/wcc/home/snowClimateMonitoring/

# Data can also be accessed through the SNOTEL data portal:
# https://wcc.sc.egov.usda.gov/reportGenerator/

# Keith Jennings
# kjennings@lynker.com
# 2021-07-22

# Load package
library(tidyverse)

# Identify SNOTEL sites of interest
sites <- c(356,1051,1067,428,462,463,473,1049,508,518,1050,539,540,541,633,697,724,778,784,1052,809,834,848,340,1242,615,652)

# Create vector of water years to download (Oct. 1 through Sept. 30)
wyears <- c(2020,2021)

# Download strings
dl1 = "https://wcc.sc.egov.usda.gov/nwcc/view?intervalType=Historic+&report=STAND&timeseries=Hourly&format=copy&sitenum="
dl2 = "&year="
dl3 = "&month=WY"

# Make empty list for data
snotel <- data.frame()

# Loop by site and wyear
for(i in 1:length(sites)){
  
  # Make temporary list for downloaded data
  tmp.l <- list()
  
  # Loop through the water years
  for(j in 1:length(wyears)){
    
    # Download data into list
    tmp.l[[j]] <-
      read.csv(url(paste0(dl1,
                          sites[i],
                          dl2,
                          wyears[j],
                          dl3)),
               stringsAsFactors = F,
               skip = 4)
  }
  
  # Bind data from temporary list into a single data frame
  tmp.df <- plyr::ldply(tmp.l, bind_rows)
  
  # Bind temporary data frame to snotel data frame
  snotel <- bind_rows(snotel, tmp.df)
  
  # Remove tmp files
  rm(tmp.l, tmp.df)
  
}

# Rename select columns
snotel <- snotel %>% 
  rename(#tair = TOBS.I.1..degC.,
         ppt = PREC.I.1..in.) %>% 
  mutate(datetime = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M", tz = "Etc/GMT+8"))

# Arrange by station, then by datetime
snotel <- snotel %>% 
  arrange(Site.Id, datetime)

# Remove NA ppt values and convert to mm
in_to_mm <- 25.4
snotel <- snotel %>% 
  mutate(ppt = ifelse(ppt < 0,
                      NA,
                      ppt * in_to_mm))

# Save file
saveRDS(snotel,
        "data/met/snotel_nv_ca_wy2020_2021.RDS")

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
