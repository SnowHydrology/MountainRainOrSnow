# This script accesses meteorological data and processes it into a usable form
# for further analysis

# All accessed met data are now available programatically

# Keith Jennings
# kjennings@lynker.com
# update 2022-05-09

# Load packages
library(tidyverse)
library(lubridate)
library(lutz) # time zone calculations

# As of 2022-05-09, we are using the following data sources:
  # HADS
    # Can be downloaded programmatically using URL builder in
    # mros_hads_download.R
  # WCC (includes SNOTEL, SNOLITE, SCAN)
    # Can be downloaded programmatically using URL builder in
    # data_download_snotel.R
  # Local Climatological Dataset
    # Can be downloaded programmatically using URL builder in
    # mros_lcd_download.R
    
# Import the RDS files
hads <- readRDS("data/NOSHARE/mros_met_hads_20220927.RDS")
  # bind_rows(readRDS("data/NOSHARE/TEMP1_met_hads_20220503.RDS"),
  #                 readRDS("data/NOSHARE/TEMP2_met_hads_20220503.RDS"),
  #                 readRDS("data/NOSHARE/TEMP3_met_hads_20220503.RDS"))
wcc <- readRDS("data/NOSHARE/mros_met_wcc_20220927.RDS")
lcd <- readRDS("data/NOSHARE/mros_met_lcd_20220927.RDS")

# Name the export file (to save all aggregated met data)
export_file = "data/processed/mros_met_all_20220927.RDS"

###############################################################################
#                                 HADS data                                   #
###############################################################################

# The HADS download brought in all the data, but we only want a few vars
# station = unique id in HADS metadata
# utc_valid = UTC datetime
# XRIRGZZ = relative humidity nominally instantaneous but various time steps
# TAIRGZZ = air temp nominally instantaneous but various time steps
# TDIRGZZ = dew point temp nominally instantaneous but various time steps
# The HADS var names use SHEF identifiers where:
# Positions 1+2 = var name (TA = air temp)
# Position 3 = time stamp (I = instantaneous)
# Positions 4+5 = source code (RG = GOES, RZ = nonspecified)
# Positions 6+7 = extremum and probability codes (we just want the null values, ZZ)

hads <- hads %>% 
  select(station, utc_valid, TAIRGZZ, TDIRGZZ, XRIRGZZ) %>% 
  mutate(across(3:5, as.numeric))

# Format datetime
# All hads data are given in UTC
hads$datetime <- as.POSIXct(hads$utc_valid, 
                            format = "%Y-%m-%d %H:%M:%S",
                            tz = "UTC")

# Arrange by station, then by datetime
hads <- hads %>% 
  arrange(station, datetime)

# Import HADS metadata
# List the metadata files
meta.dir <- "data/metadata/hads_stations_byState/"
meta.files <- paste0(meta.dir, list.files(meta.dir, pattern = "*.csv"))

# Bind the files
hads_meta <- meta.files %>% 
  lapply(read_csv) %>% 
  bind_rows()

# Name the var of interest (air temperature)
var = "TA"

# Filter to only stations that monitor that var
hads_meta <- hads_meta %>% 
  filter(if_any(pe1:pe25, ~ . == var))

# Check the valid stations
hads_meta_valid <- filter(hads_meta, nwsli %in% 
                            unique(filter(hads, !is.na(TAIRGZZ))$station))

# Convert valid stations to a shapefile to extract elevation via
# Google Earth Engine
# Convert the data to spatial object
# Extract the relevant info for spatial conversion
coords <- select(hads_meta_valid, longitude_d, latitude_d)
ids <- select(hads_meta_valid, nwsli)
crs_obs <- sp::CRS("+init=epsg:4326")

# Convert obs to a SpatialPointsDataFrame
hads_sp <- sp::SpatialPointsDataFrame(coords = coords, 
                                 data = ids, 
                                 proj4string = crs_obs)

# Export
rgdal::writeOGR(hads_sp, 
                dsn = "data/geospatial/", 
                layer = "hads_stations_2022", 
                driver = "ESRI Shapefile")

######## RUN ELEVATION SCRIPT IN GEE, THEN REIMPORT #################
# Read in elevation data for HADS stations
hads_elev <- read.csv("data/metadata/hads_stations_byState/elevation/hads_2022_elev_3dep_pts.csv")

# Bind
hads_meta_valid <- left_join(hads_meta_valid, select(hads_elev, nwsli, elevation),
                             by = "nwsli")

###############################################################################
#                               WCC data                                   #
###############################################################################

# Import SNOTEL metadata
wcc_meta <- read_csv("data/metadata/wcc_station_metadata.csv") %>% 
  group_by(Id) %>% 
  mutate(id = str_split(Id, " ")[[1]][2],
         network = str_split(Id, " ")[[1]][1])

# Format datetime
# For some reason, all western SNOTEL stations are given in PST
# While all other locations are local standard time
# This means CO is given in PST
# All SCAN data are local standard time
# From: https://www.nrcs.usda.gov/wps/portal/wcc/home/dataAccessHelp/faqs/!ut/p/z1/nZHBcoIwEEB_BQ8cmV0gknikHKrFGceCVXLppCEKrQTUjLR_3-h4FafdW2Y3u2_3AYcNcC3O9U6YutVib98Fj95nAYunLMP0OSEE40lC83lGMc0jWD8q4DaNdyJGeAMOXGrTmQqKXkpHttoobVys2ka5WAojHCGlOp2cSu07F7ujKtW21qp0GtGdLv87WZdQjBWdIPpjLwgJ8UjkK--DiIkXUIV-6NOIsODGy1hC_AXBObJZgMtkmZLFGENMyZV3cF_-aCU-2OPpUYEdUlhIeh-Swvpcqx5Wuj02VlL2xxtMEV6uCANnsOLrz8OBx9bOxci3gc2_9HTNqmHhj_f1yvp8W-138Wj0C_s17ig!/dz/d5/L2dJQSEvUUt3QS80TmxFL1o2Xzg4QzQxTzQwTEc1MjMwUTJBTjFDTEsyMEMy/

# First, get the time zone for each station
wcc_meta <- wcc_meta %>% 
  mutate(timezone = ifelse(network == "SCAN",
                           tz_lookup_coords(lat = Lat, 
                                            lon = Lon, 
                                            method = "accurate"),
                           "America/Los_Angeles"), 
         timezone_lst = ifelse(timezone == "America/Los_Angeles",
                               "Etc/GMT+8",
                               ifelse(timezone == "America/Denver",
                                      "Etc/GMT+7", 
                                      ifelse(timezone == "America/New_York",
                                             "Etc/GMT+5",
                                             NA))))  %>% 
  ungroup()

# Join the time zone info to the data
wcc.l <- left_join(wcc, select(wcc_meta, id, timezone_lst),
                 by = "id") %>% 
  split(.$timezone_lst)

# Calculate time info
# Note this is more complex than just calculating a new column
# Why? R no likey multiple time zones in one column
# So we split out time zones into different list elements
# Then compute local time info, convert to UTC and join everything back together
wcc.l <- lapply(wcc.l, function(x){
  data.frame(x) %>% 
    mutate(datetime = as.POSIXct(x = paste(x$date, x$time), 
                              tz = x[[1, "timezone_lst"]],
                              format = "%Y-%d-%m %H:%M"),
           utc_datetime = with_tz(time = datetime, 
                                  tzone = "UTC"), 
           datetime = NULL)
})
  
wcc <- plyr::ldply(wcc.l, bind_rows)

# Filter to imported stations only
wcc_meta_valid <- filter(wcc_meta, id %in% unique(wcc$id))

# Convert elevation to meteres
wcc_meta_valid$Elev <- wcc_meta_valid$Elev / 3.28



###############################################################################
#                               Local Climatological Data                                  #
###############################################################################

# Extract unique ids from LCD data to match in metadata
lcd_ids <- data.frame(id_full = unique(lcd$STATION)) %>%
  mutate(id = str_sub(id_full, 7, 11))

# Import the LCD metadata
lcd_meta <- read.csv("data/metadata/lcd_station_metadata.csv",
                           stringsAsFactors = F) %>% 
  mutate(id = str_sub(STATION_ID, 6, 10))

# Join the full and partial IDs
lcd_meta <- left_join(lcd_meta, lcd_ids,
                      by = "id")

# Format datetime
# LCD data are in Local Standard Time
# As per https://www1.ncdc.noaa.gov/pub/data/cdo/documentation/LCD_documentation.pdf
# From: https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc:C00684/html#

# First, get the time zone for each station
lcd_meta <- lcd_meta %>% 
  mutate(timezone = tz_lookup_coords(lat = LATITUDE, 
                                     lon = LONGITUDE, 
                                     method = "accurate"),
         timezone_lst = ifelse(timezone == "America/Los_Angeles",
                               "Etc/GMT+8",
                               ifelse(timezone == "America/Denver",
                                      "Etc/GMT+7", 
                                      ifelse(timezone == "America/Chicago",
                                             "Etc/GMT+6",
                                             ifelse(timezone == "America/New_York",
                                                    "Etc/GMT+5",
                                                    NA)))))

# Join the time zone info to the data
lcd.l <- left_join(lcd, select(lcd_meta, id_full, timezone_lst),
                   by = c("STATION" = "id_full")) %>% 
  split(.$timezone_lst)

# Calculate time info
# Note this is more complex than just calculating a new column
# Why? R no likey multiple time zones in one column
# So we split out time zones into different list elements
# Then compute local time info, convert to UTC and join everything back together
lcd.l <- lapply(lcd.l, function(x){
  data.frame(x) %>% 
    mutate(datetime = as.POSIXct(x = x$DATE, 
                                 tz = x[[1, "timezone_lst"]],
                                 format = "%Y-%d-%mT%H:%M:%S"),
           utc_datetime = with_tz(time = datetime, 
                                  tzone = "UTC"), 
           datetime = NULL)
})

lcd <- plyr::ldply(lcd.l, bind_rows)

# Get valid metadata
lcd_meta_valid <- filter(lcd_meta, id_full %in% unique(lcd$STATION))

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
# network

# Rename metadata colnames to match
# And convert numeric ids to character
hads_meta_valid2 <- hads_meta_valid %>% 
  select(name = owner_code, 
         id = nwsli, 
         lat = latitude_d,
         lon = longitude_d,
         elev = elevation) %>% 
  mutate(network = "hads") %>% 
  filter(!is.na(elev)) # fail-safe to remove entries with missing elevation
wcc_meta_valid2 <- wcc_meta_valid %>% 
  select(name = `Site Name`, 
         id, 
         lat = Lat,
         lon = Lon,
         elev = Elev,
         network) %>% 
  filter(!is.na(elev)) # fail-safe to remove entries with missing elevation
lcd_meta_valid2 <- lcd_meta_valid %>% 
  select(name = STATION, 
         id = id_full, 
         lat = LATITUDE,
         lon = LONGITUDE,
         elev = ELEVATION_.M.) %>% 
  mutate(network = "lcd") %>% 
  filter(!is.na(elev)) # fail-safe to remove entries with missing elevation

# Bind all together
all_meta_valid <- bind_rows(hads_meta_valid2, wcc_meta_valid2,
                            lcd_meta_valid2) 

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
# datetime (UTC only!)
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
hads_valid <- filter(hads, station %in% hads_meta_valid2$id) %>% 
  select(id = station, datetime, tair = TAIRGZZ, tdew = TDIRGZZ, rh = XRIRGZZ)

# Convert fahrenheit to celsius and in to mm
f_to_c <- function(temp_F){ (temp_F - 32) * (5/9) }
in_to_mm <- function(ppt_in){ ppt_in * 25.4}
hads_valid <- hads_valid %>% 
  mutate(tair = f_to_c(tair),
         tdew = f_to_c(tdew))

#######################################
# WCC

# Select only the appropriate columns and rename
wcc_valid <- wcc %>%  
  select(id, datetime = utc_datetime, tair, tdew, rh) %>% 
  filter(id %in% wcc_meta_valid2$id)

#######################################
# LCD

# Select only the appropriate columns and rename
# But first filter to only the FM-15 reports
lcd_valid <- lcd %>% 
  filter(REPORT_TYPE == "FM-15" & STATION %in% lcd_meta_valid2$id) %>% 
  select(id = STATION, datetime = utc_datetime, 
         tair = HourlyDryBulbTemperature, tdew = HourlyDewPointTemperature,
         rh = HourlyRelativeHumidity, twet = HourlyWetBulbTemperature, 
         ppt = HourlyPrecipitation)

# Convert any data points containing an "s" to NA
# These don't pass the stations QC checks
lcd_valid <- lcd_valid %>% 
  mutate(tair2 = case_when(grepl("s", tair) ~ NA_real_,
                           TRUE ~ as.numeric(as.character(tair))),
         twet2 = case_when(grepl("s", twet) ~ NA_real_,
                           TRUE ~ as.numeric(as.character(twet))),
         tdew2 = case_when(grepl("s", tdew) ~ NA_real_,
                           TRUE ~ as.numeric(as.character(tdew))),
         ppt2  = case_when(grepl("s", ppt) ~ NA_real_,
                           TRUE ~ as.numeric(as.character(ppt))),
         rh2   = case_when(grepl("s", rh) ~ NA_real_,
                           TRUE ~ as.numeric(as.character(rh)))) %>% 
  mutate(ppt2 = ifelse(ppt == "T", 0.0001, ppt2))

# Convert temp to celsius and ppt to mm
lcd_valid <- lcd_valid %>%
  select(id, datetime, 
         tair = tair2, tdew = tdew2, twet = twet2, rh = rh2, ppt = ppt2) %>% 
  mutate(tair = f_to_c(tair),
         tdew = f_to_c(tdew),
         twet = f_to_c(twet),
         ppt = in_to_mm(ppt))


# Bind all the met data for export
met_all <- bind_rows(hads_valid,
                     wcc_valid,
                     lcd_valid)

# Export 
saveRDS(object = met_all, file = export_file)

