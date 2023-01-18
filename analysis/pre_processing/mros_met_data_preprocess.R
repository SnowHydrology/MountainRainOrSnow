# This script accesses meteorological data and processes it into a usable form
# for further analysis

# Some of the met data are not available programmatically (e.g. RAWS), so 
# they are downloaded beforehand

# Keith Jennings
# kjennings@lynker.com

# Load packages
library(tidyverse)

# As of 2021-07-20, we are using the following data sources:
  # HADS
    # Can be downloaded programmatically using URL builder below
    # https://mesonet.agron.iastate.edu/request/dcp/fe.phtml
  # SNOTEL
    # Can be downloaded programmatically using URL builder below
    # https://wcc.sc.egov.usda.gov/reportGenerator/
    # Or see other SNOTEL data access scripts
  # RAWS
    # Must be downloaded per-site using password
    # e.g., https://raws.dri.edu/cgi-bin/rawMAIN.pl?nvndss
    # Go to Data Lister and select several options:
    # 1) Start date (January 01 2020)
    # 2) End date (August 01 2021)
    # 3) Enter password
    # 4) Data format: Delimited format (.txt Win/PC)
    # 5) Select the units: Metric
  # Airports part of NCDC LCD network 
    # Downloaded via online interface
    # https://gis.ncdc.noaa.gov/maps/ncei/lcd 
    # Select area to download, add date range, download CSV
    
# List files in data folder
files <- data.frame(files = list.files("data/met/")) %>% 
  mutate(network = substr(files, 1, 4))

###############################################################################
#                                 HADS data                                   #
###############################################################################

# Import the HADS data
hads.l <- lapply(paste0("data/met/", filter(files, network == "hads")$files), 
                 read.csv, stringsAsFactors = F)

# Convert to dataframe 
hads <- plyr::ldply(hads.l, bind_rows) %>% 
  select(.,
         station, # station name - corresponds to unique idea in HADS metadata
         utc_valid, # UTC time
         XRIRGZ, # relative humidity nominally instantaneous but various time steps
         TAIRGZ, # air temp nominally instantaneous but various time steps
         PAIRGZ, # air pressure nominally instantaneous but various time steps
         PPHRGZ) # hourly precip # 15 minute ppt

# Remove old data 
rm(hads.l)

# Format datetime
hads$datetime <- as.POSIXct(hads$utc_valid, 
                            format = "%Y-%m-%d %H:%M:%S",
                            tz = "UTC")

# Arrange by station, then by datetime
hads <- hads %>% 
  arrange(station, datetime)

# Find the time step for each station
hads$time_gap <- c(0, diff(hads$datetime))

# Get average value per station
hads_time_gaps <- filter(hads, time_gap > 0) %>% 
  group_by(station) %>% 
  summarise(time_gap_avg = mean(time_gap),
            time_gap_max = max(time_gap),
            time_gap_min = min(time_gap),
            time_gap_med = median(time_gap),
            time_gap_mod = mode(time_gap))

# Remove all stations w/ median time gaps > 60 minutes
# NOTE: sometimes the time gap will be in minutes, sometimes it will be in seconds
# double-check before filtering!
hads <- filter(hads, station %in% 
                 filter(hads_time_gaps, time_gap_med <= 60)$station)


# Import HADS metadata
hads_meta <- read.csv("data/metadata/hads_station_metadata.csv",
                      stringsAsFactors = F)

# Check the valid stations
hads_meta_valid <- filter(hads_meta, stid %in% 
                            unique(filter(hads, !is.na(TAIRGZ))$station))


###############################################################################
#                               SNOTEL data                                   #
###############################################################################

# Import the SNOTEL data
snotel <- readRDS("data/met/snotel_nv_ca_wy2020_2021.RDS")

# Import SNOTEL metadata
snotel_meta <- read.csv("data/metadata/nwcc_station_metadata.csv",
                        stringsAsFactors = F)

# Filter to imported stations only
snotel_meta_valid <- filter(snotel_meta, station.id %in% unique(snotel$Site.Id))

# Convert elevation to meteres
snotel_meta_valid$elev <- snotel_meta_valid$elev / 3.28

###############################################################################
#                                RAWS data                                    #
###############################################################################

# Import data from RAWS

# Make dummy dataframe for storing data
raws <- data.frame()

# Filter the raws files for import
raws.files <- filter(files, network == "raws")$files

# Loop through and import files
# Needed to handle wonky formatting
for(i in 1:length(raws.files)){
  
  # Identify tmp file string and new file string
  file.string.tmp <- paste0("data/met/", raws.files[i])
  file.string.tmp.clean <- paste0("data/met/", 
                                  substr(raws.files[i], 1, 14), "_clean.dat")
  
  # First run system command to remove lines that start with "<"
  system(paste0("sed '/^</d' ",
                file.string.tmp,
                " > ",
                file.string.tmp.clean))
  
  # Import headers
  # These vary by file because diff stations have diff sensors
  raws.headers.tmp <- reader::n.readLines(file.string.tmp.clean, 
                                          skip = 4, n = 1)
  raws.headers.tmp <- unlist(strsplit(raws.headers.tmp, ","))
  
  # import data
  raws.tmp = read.csv(file.string.tmp.clean, skip = 6, header = F)
  colnames(raws.tmp) = make.names(raws.headers.tmp, unique = T)
  
  # Rename and select columns
  raws.tmp <- raws.tmp %>% 
    select(., "X..Date.Time", "X.Av.Air", "X.Precip", "X..Rel..") %>% 
    rename("datetime" = "X..Date.Time", "tair" = "X.Av.Air", "ppt" = "X.Precip",
           "rh" = "X..Rel..")
  
  # Add station column
  raws.tmp$station <- substr(raws.files[i], 11, 14)
  
  # Bind to previous data
  raws <- bind_rows(raws, raws.tmp)
  
}

# Format datetime
raws <- raws %>% 
  mutate(datetime = as.POSIXct(as.character(datetime),
                               format = "%y%m%d%H%M", tz = "Etc/GMT+8"))

# Arrange by station, then by datetime
raws <- raws %>% 
  arrange(station, datetime)

# Get valid stations
raws_meta <- read.csv("data/metadata/raws_station_metadata.csv")
raws_meta_valid <- filter(raws_meta, id %in% unique(raws$station))

# Convert DDMMSS lat and lon to decimal degrees
raws_meta_valid <- raws_meta_valid %>% 
  mutate(lat = as.numeric(substr(lat_ddmmss, 1,2)) +
           (as.numeric(substr(lat_ddmmss, 3,4)) / 60) +
           (as.numeric(substr(lat_ddmmss, 5,6)) / 3600),
         lon = (as.numeric(substr(lon_ddmmss, 1,3)) +
                  (as.numeric(substr(lon_ddmmss, 4,5)) / 60) +
                  (as.numeric(substr(lon_ddmmss, 6,7)) / 3600)) * -1 )

# Convert elevation to meters
raws_meta_valid$elev <- as.numeric(as.character(raws_meta_valid$elev_ft)) / 3.28

###############################################################################
#                               Airport data                                  #
###############################################################################

# Import data from airports
# And select just the wanted columns
airport <- read.csv("data/met/airp_data_all.csv") %>% 
  select(STATION:SOURCE, HourlyDewPointTemperature:HourlyPresentWeatherType,
         HourlyRelativeHumidity,HourlyWetBulbTemperature)

# Format datetime
airport <- airport %>% 
  mutate(datetime = as.POSIXct(DATE, format = "%Y-%m-%dT%H:%M:%S",
                               tz = "Etc/GMT+8"))

# Import the airport metadata (all stations report valid data)
airport_meta_valid <- read.csv("data/metadata/airp_station_metadata.csv",
                               stringsAsFactors = F)


###############################################################################
#                            AGGREGATE METADATA                               #
###############################################################################

# Aggregate all valid station metadata 
# Need:
# Name
# ID
# lat
# lon
# elev

# Remove RAWS from HADS data (these are duplicates)
hads_meta_remove <- filter(hads_meta_valid, grepl("RAWS", station_name))
hads_meta_valid <- filter(hads_meta_valid, !station_name %in% hads_meta_remove$station_name)

# Rename metadata colnames to match
# And convert numeric ids to character
hads_meta_valid <- hads_meta_valid %>% 
  rename(id = stid, name = station_name, network = iem_network)
snotel_meta_valid <- snotel_meta_valid %>% 
  rename(id = station.id, name = site_name) %>% 
  mutate(id = as.character(id))
airport_meta_valid <- airport_meta_valid %>% 
  mutate(id = as.character(id))

# Bind all together
# Select only the columns we want
# Remove duplicate entries
all_meta_valid <- bind_rows(hads_meta_valid, snotel_meta_valid,
                            raws_meta_valid, airport_meta_valid) %>% 
  select(., network, name, id, elev, lat, lon) %>% 
  distinct(id, .keep_all = T)

# Export the metadata
write.csv(x = all_meta_valid,
          file = "data/metadata/all_metadata_valid.csv",
          row.names = F, quote = F)

###############################################################################
#                        AGGREGATE METEOROLOGICAL DATA                        #
###############################################################################

################################################################################
# Clean up the data and put into dataframe with the following columns:
# id
# datetime
# tair
# ppt
# relative humidity
# wet bulb temperature 
# dewpoint temperature

# Not all networks have the above data

#######################################
# HADS

# Clean the station for export
# Select only the appropriate columns and rename
hads_valid <- filter(hads, station %in% hads_meta_valid$id) %>% 
  select(id = station, datetime, tair = TAIRGZ, ppt = PPHRGZ, rh = XRIRGZ)

# Convert time zone from UTC to Pacific Standard time
attr(hads_valid$datetime, "tzone") <- "Etc/GMT+8"

# Convert fahrenheit to celsius and in to mm
f_to_c <- function(temp_F){ (temp_F - 32) * (5/9) }
in_to_mm <- function(ppt_in){ ppt_in * 25.4}
hads_valid <- hads_valid %>% 
  mutate(tair = f_to_c(tair),
         ppt = in_to_mm(ppt))

# Remove stations with few valid data points
hads_remove <- c("FOIC1", "GKSC1", "HYSC1", "SEYC1", "CNLC1")
hads_valid <- filter(hads_valid, !id %in% hads_remove)

#######################################
# SNOTEL

# Select only the appropriate columns and rename
snotel_valid <- snotel %>%  
  select(id = Site.Id, datetime, tair)

# Remove stations with few valid data points
# snotel_remove <- c("724", "834", "848")
# snotel_valid <- filter(snotel_valid, !id %in% snotel_remove)

# We do not use snotel hourly precip because it is unreliable
# Convert precip per station from cumulative to increment
# snotel_valid <- snotel_valid %>% 
#   mutate(ppt = c(0, diff(ppt))) %>% 
#   mutate(ppt = ifelse(ppt < 0 | ppt > 50,
#                       NA,
#                       ppt))

#######################################
# RAWS
raws_valid <- raws %>% 
  select(id = station, datetime, tair, ppt, rh)

# Convert ppt from cumulative to increment
raws_valid <- raws_valid %>% 
    mutate(ppt = c(0, diff(ppt))) %>%
    mutate(ppt = ifelse(ppt < 0 | ppt > 50,
                        NA,
                        ppt))

#######################################
# Airports

# Select only the appropriate columns and rename
# But first filter to only the FM-15 reports
airport_valid <- airport %>% 
  filter(REPORT_TYPE == "FM-15") %>% 
  select(id = STATION, datetime, tair = HourlyDryBulbTemperature,
         ppt = HourlyPrecipitation, rh = HourlyRelativeHumidity,
         twet = HourlyWetBulbTemperature, tdew = HourlyDewPointTemperature)

# Convert any data points containing an "s" to NA
# These don't pass the stations QC checks
airport_valid <- airport_valid %>% 
  mutate(tair2 = case_when(grepl("s", tair) ~ NA_real_,
                           TRUE ~ as.numeric(as.character(tair))),
         twet2 = case_when(grepl("s", twet) ~ NA_real_,
                           TRUE ~ as.numeric(as.character(twet))),
         tdew2 = case_when(grepl("s", tdew) ~ NA_real_,
                           TRUE ~ as.numeric(as.character(tdew))),
         ppt2  = case_when(grepl("s", ppt) ~ NA_real_,
                           TRUE ~ as.numeric(as.character(ppt))),
         rh2   = case_when(grepl("s", rh) ~ NA_real_,
                           TRUE ~ as.numeric(as.character(rh))))

# Convert temp to celsius and ppt to mm
airport_valid <- airport_valid %>%
  select(-c(tair, twet, tdew, ppt, rh), 
         tair = tair2, twet = twet2, tdew = tdew2, ppt = ppt2, rh = rh2) %>% 
  mutate(tair = f_to_c(tair),
         twet = f_to_c(twet),
         tdew = f_to_c(tdew),
         ppt = in_to_mm(ppt))


# Bind all the met data for export
met_all <- bind_rows(hads_valid,
                     snotel_valid %>% mutate(id = as.character(id)),
                     raws_valid,
                     airport_valid %>% mutate(id = as.character(id)))

# Export 
saveRDS(object = met_all, file = "data/processed/met_all_NV_CA_2020_2021.RDS")

