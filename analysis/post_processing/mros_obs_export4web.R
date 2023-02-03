# Script for outputting processed citizen science data and ancillary info

# TODO: move other post-processing steps here (currently buried in other files)

# Load package
library(tidyverse)

# User input
obs_file = "data/NOSHARE/mros_cit_sci_obs_processed_with_tair_QCflags_20220927.RDS"
gpm_file = "data/NOSHARE/mros_gpm_processed_20220927.RDS"
output_file = "data/processed/mros_obs_processed_20220927" # save as RDS and CSV

# Read in RDS files
obs <- readRDS(obs_file)
gpm <- readRDS(gpm_file) %>% 
  mutate(longitude = sf::st_coordinates(geometry)[, "X"],
         latitude = sf::st_coordinates(geometry)[, "Y"])

# Join the data
df <- left_join(obs,
                select(gpm, utc_datetime, latitude, longitude, phase,
                       gpm_prob = probabilityLiquidPrecipitation), 
                by = c("utc_datetime", "latitude", "longitude", "phase")) %>% 
  mutate(geometry = NULL)

# Remove PII and truncate lat-lon precision
df <- df %>%
  mutate(observer = NULL,
         latitude = round(latitude, digits = 2),
         longitude = round(longitude, digits = 2))

# Export csv and RDS
write.csv(df, paste0(output_file, ".csv"), quote = F, row.names = F)
saveRDS(df, paste0(output_file, ".RDS"))
