# Script for gathering HADS Data from web

library(tidyverse)

# https://hads.ncep.noaa.gov/csv/csvbystate.shtml

# List the metadata files
meta.dir <- "data/metadata/hads_stations_byState/"
meta.files <- paste0(meta.dir, list.files(meta.dir))

# Bind the files
meta <- meta.files %>% 
  lapply(read_csv) %>% 
  bind_rows()

# Name the var of interest (air temperature)
var = "TA"

# Filter to only stations that monitor that var
meta <- meta %>% 
  filter(if_any(pe1:pe25, ~ . == var))

# Specify chunks to download
# HADS can only serve from 1 calendar year at a time
# Chunks specify the date ranges in each year
chunks = data.frame(
  chunk = c(1,2),
  year = c(2021,2022),
  month1 = c(10,1),
  day1 = c(7,1),
  hour1 = c(0,0),
  min1 = c(0,0),
  month2 = c(12,5),
  day2 = c(31,4),
  hour2 = c(23,23),
  min2 = c(59,59)
)

# Build the URL string
hads_url01_str = "https://mesonet.agron.iastate.edu/cgi-bin/request/hads.py?network="
hads_url04_str = "&year="
hads_url06_str = "&month1="
hads_url08_str = "&day1="
hads_url10_str = "&hour1="
hads_url12_str = "&minute1="
hads_url14_str = "&month2="
hads_url16_str = "&day2="
hads_url18_str = "&hour2="
hads_url20_str = "&minute2="

# Reset the timeout option so download can finish (default is 60 seconds)
# getOption('timeout')
# [1] 60
options(timeout = 1000) # this will reset to default when session terminated

# Empty df to store data
met <- data.frame()

# Loop through the options and download HADS data
for(i in seq_along(chunks$chunk)){
  # Sub loop by years
  for(j in seq_along(meta$nwsli)){
    # Build the URL parts
    hads_url02_net = paste0(meta[j, "state_code"], "_DCP")
    hads_url03_sta = paste0("&stations=", meta[j, "nwsli"])
    hads_url05_yr1 = chunks[i, "year"]
    hads_url07_mo1 = chunks[i, "month1"]
    hads_url09_dy1 = chunks[i, "day1"]
    hads_url11_hr1 = chunks[i, "hour1"]
    hads_url13_mi1 = chunks[i, "min1"]
    hads_url15_mo2 = chunks[i, "month2"]
    hads_url17_dy2 = chunks[i, "day2"]
    hads_url19_hr2 = chunks[i, "hour2"]
    hads_url21_mi2 = chunks[i, "min2"]
    
    # Concatenate parts into URL
    met.link = paste0(hads_url01_str, 
                    hads_url02_net, 
                    hads_url03_sta,
                    hads_url04_str, 
                    hads_url05_yr1, 
                    hads_url06_str,
                    hads_url07_mo1, 
                    hads_url08_str,
                    hads_url09_dy1,
                    hads_url10_str, 
                    hads_url11_hr1,
                    hads_url12_str,
                    hads_url13_mi1, 
                    hads_url14_str,    
                    hads_url15_mo2,
                    hads_url16_str, 
                    hads_url17_dy2,    
                    hads_url18_str,
                    hads_url19_hr2, 
                    hads_url20_str,    
                    hads_url21_mi2)
    
    # Download data
    tmp.met <- met.link %>% read_csv(col_types = cols(.default = "c"))
    
    # Bind data
    met <- bind_rows(met, tmp.met)
  }
}
