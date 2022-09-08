# Mountain rain or snow rain-snow line creation from citsci data


# Keith Jennings
# 2021-12-06
# ksj.hydro@gmail.com

# Load packages
library(tidyverse)
library(lubridate)
library(cowplot); theme_set(theme_cowplot())
library(foreach) # for parallel processing
library(doMC); registerDoMC(cores = 4)

# Import obs
obs <- readRDS("data/processed/mros_obs_processed_2020_2021.RDS") %>% 
  filter(tair_flag == "Pass" & 
           ppt_flag == "Pass" & 
           rh_flag == "Pass" &
           dist_flag == "Pass" & 
           dupe_flag == "Pass")

# Create elevation bins and names
obs <- obs %>%
  mutate(elev_bin2 = cut_width(elev, width = 250))

obs_by_elev_date <- obs %>% 
  group_by(date, elev_bin2) %>% 
  summarise(snow_prob = sum(phase == "Snow")/length(phase) * 100) %>% 
  mutate(elev = case_when(elev_bin2 == "[625,875]" ~ 750,
                          elev_bin2 == "(875,1.12e+03]" ~ 1000,
                          TRUE ~ as.numeric(substr(elev_bin2, 2, 5)) * 1000 + 125),
         n = n())

# Get unique dates
dates <- unique(obs_by_elev_date$date)

# Compute 50% rain-snow elevation per scene
rain_snow_line.l <-
  foreach(i = 1:length(dates), .errorhandling = "pass") %dopar% {
    
    # Filter GPM to datetime
    tmp.df <- filter(obs_by_elev_date, date == dates[i])
    
    # Check that there are probabilities above and below 50%
    if(min(tmp.df$snow_prob) >= 50){
      tmp.rs_line = NA
      tmp.note = "above"
    } else if(max(tmp.df$snow_prob) < 50){
      tmp.rs_line = NA
      tmp.note = "below"
    } else {
      # Build linear model predicting prob by elev
      tmp.lm <- lm(snow_prob ~ elev, tmp.df)
      tmp.slope = tmp.lm$coefficients[2] %>% as.numeric()
      tmp.intercept = tmp.lm$coefficients[1] %>% as.numeric()
      tmp.rs_line = (50 - tmp.intercept) / tmp.slope
      #tmp.r2 = summary(tmp.lm)$r.squared
      #tmp.pval = summary(tmp.lm)$coefficients[2,4] 
      tmp.note = "valid"
    }
    
    # Output into dataframe
    tmp.rain_snow_line <- data.frame(date = dates[i],
                                     rs_line = tmp.rs_line,
                                     note = tmp.note,
                                     n = sum(tmp.df$n))
    
    # Print to list
    tmp.rain_snow_line
    
  }

# Bind list into dataframe
rain_snow_line <- plyr::ldply(rain_snow_line.l, bind_rows)


# Export data
saveRDS(object = rain_snow_line,
        file = "data/processed/mros_obs_rain_snow_line_2020-2021.RDS")
saveRDS(object = obs_by_elev_date,
        file = "data/processed/mros_obs_rain_snow_prob_elev_2020-2021.RDS")