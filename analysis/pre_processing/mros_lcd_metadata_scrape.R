# Script for filtering NOAA Local Climatological Data stations

library(tidyverse)
library(rvest)

# First access the metadata from NOAA
# https://www.ncei.noaa.gov/maps/lcd/ 
# go to mapping tool, use polygon selector
# drag over area of interest
# and then click "Download Station List"
lcd_meta <- read.csv("data/metadata/lcd_station_metadata_conus.csv")
export.file = "data/metadata/lcd_station_metadata_conus_withID.csv"

# Format date and filter to current stations only
lcd_meta <- lcd_meta %>% 
  mutate(short_id = str_sub(STATION_ID, 6, 10),
         END_DATE = as.Date(END_DATE)) %>% 
  filter(END_DATE > as.Date("2023-06-12"))

# URL for finding station codes
baseURL = "https://www.ncei.noaa.gov/data/local-climatological-data/access/"
year = 2023

# Build URL
URL = paste0(baseURL, year)
  
# Get all links from URL
links_full <- read_html(URL) %>% 
  html_nodes("a") %>% 
  html_attr('href') 
links <- links_full %>% 
  str_sub(7,11)
 
# Make data frame to store info
info <- data.frame()

# Loop through IDs to finding matching stations and URLS  
for(j in 1:length(lcd_meta$short_id)){
  # Check for partial match in station ID and URLs
  link.match = str_detect(links, pattern = lcd_meta[j, "short_id"])
  
  # Find n matches
  matches = logical()
  matches = which(link.match == TRUE)
  
  # Skip or download
  print(paste("Station id =", lcd_meta[j, "short_id"]))
  if(length(matches) == 0){
    print("No match found, going to next station")
    # Extract info for station and data
    tmp.info <- data.frame(short_id = lcd_meta[j, "short_id"],
                           id = NA,
                           name = lcd_meta[j, "STATION"],
                           n_matches = length(matches),
                           link = NA)
    info <- bind_rows(info, tmp.info)
  } else if(length(matches) > 0){
    print(paste(length(matches), "match(es) found"))
    for(k in 1:length(matches)){
      # Link to met data
      met.link = paste0(URL, "/",
                        links_full[matches[k]])
      # Extract info for station and data
      tmp.info <- data.frame(short_id = lcd_meta[j, "short_id"],
                             id = substr(links_full[matches[k]], 1, 11),
                             name = lcd_meta[j, "STATION"],
                             n_matches = length(matches),
                             link = met.link)
      info <- bind_rows(info, tmp.info)
    }
  }
}

# Join the data
lcd_meta_withID <- left_join(lcd_meta, 
                             info,
                             by = c("short_id", "STATION" = "name"))


# Export the met data
saveRDS(lcd_meta_withID, export.file)
