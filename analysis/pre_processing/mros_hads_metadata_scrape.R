# Script for scraping HADS COOP and DCP metadata

# Load tidyverse
library(tidyverse)

# Get all locations (50 states + DC)
states = c(state.abb, "DC")
networks = c("COOP", "DCP")

# URL strings
url_01 = "https://mesonet.agron.iastate.edu/sites/networks.php?network="
url_02 = "_"
url_03 = "&format=csv&nohtml=on"

# Empty dataframe
hads_meta <- data.frame()

# Loop through states
for(i in 1:length(states)){
  # Loop through networks
  for(j in 1:length(networks)){
    tmp_url = paste0(url_01, states[i], 
                     url_02, networks[j],
                     url_03)
    hads_meta <- bind_rows(hads_meta, 
                           read.csv(tmp_url))
  } # end networks loop
} # end states loop

# Export
write.csv(file = "data/metadata/hads_station_metadata_US.csv", 
          x = hads_meta)
