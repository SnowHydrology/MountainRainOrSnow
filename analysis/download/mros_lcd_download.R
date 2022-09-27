# Script for gathering Local Climatological Data from web

library(rvest)
library(tidyverse)

# User input
baseURL = "https://www.ncei.noaa.gov/data/local-climatological-data/access/"
years = c("2021", "2022")
export.file = "data/NOSHARE/mros_met_lcd_20220927.RDS"

# Import station metadata
meta <- read.csv("data/metadata/lcd_station_metadata.csv") %>% 
  mutate(id = str_sub(STATION_ID, 6, 10),
         END_DATE = as.Date(END_DATE, format = "%m/%d/%y")) %>% 
  filter(END_DATE > as.Date(paste0(years[1], "-10-01")))

# Empty df to store data
met <- data.frame()
info <- data.frame()

# Loop through stations and years
for(i in 1:length(years)){
  # Temp df
  tmp <- data.frame()
  
  # Build URL
  URL = paste0(baseURL, years[i])
  
  # Get all links from URL
  links_full <- read_html(URL) %>% 
    html_nodes("a") %>% 
    html_attr('href') 
  links <- links_full %>% 
    str_sub(7,11)
    
  for(j in 1:length(meta$id)){
    # Check for partial match in station ID and URLs
    link.match = str_detect(links, pattern = meta[j, "id"])
    
    # Find n matches
    matches = logical()
    matches = which(link.match == TRUE)
    
    # Skip or download
    print(paste("Station id =", meta[j, "id"]))
    if(length(matches) == 0){
      print("No match found, going to next station")
      # Extract info for station and data
      tmp.info <- data.frame(year = years[i],
                             id = meta[j, "id"],
                             name = meta[j, "STATION"],
                             n_matches = length(matches),
                             link = NA)
      info <- bind_rows(info, tmp.info)
    } else if(length(matches) > 0){
      print(paste(length(matches), "match(es) found, downloading station data"))
      for(k in 1:length(matches)){
        # Build met link and download
        met.link = paste0(URL, "/",
                          links_full[matches[k]])
        tmp.met <- url(met.link) %>% read_csv(col_types = cols(.default = "c"))
        
        # Get data we want
        tmp.met <- tmp.met %>% 
          dplyr::select(STATION, DATE, REPORT_TYPE, SOURCE,
                        HourlyDewPointTemperature:HourlyPresentWeatherType,
                        HourlyRelativeHumidity,HourlyWetBulbTemperature) %>% 
          mutate(across(is.integer,
                        character))
        
        # Bind to other met data
        met <- bind_rows(met, tmp.met)
        
        # Extract info for station and data
        tmp.info <- data.frame(year = years[i],
                               id = meta[j, "id"],
                               name = meta[j, "STATION"],
                               n_matches = length(matches),
                               link = met.link)
        info <- bind_rows(info, tmp.info)
      }
    }
  }
}

# Export the met data
saveRDS(met, export.file)
